#include "./compiler.h"
#include <ctype.h>
#include <libgen.h>
#include <stdio.h>
#include <unistd.h>

#define array_length(arr) (sizeof(arr) / sizeof((arr)[0]))

void print_type(const type *ty) {
    if (!ty) {
        printf("(missing)");
        return;
    }

    switch (ty->tag) {
    case basic:
        switch (ty->primitive) {
        case void_:
            printf("void");
            break;
        case char_:
            printf("char");
            break;
        case signed_char:
            printf("signed char");
            break;
        case short_:
            printf("short");
            break;
        case int_:
            printf("int");
            break;
        case long_:
            printf("long");
            break;
        case long_long:
            printf("long long");
            break;
        case bool_:
            printf("_Bool");
            break;
        case unsigned_char:
            printf("unsigned char");
            break;
        case unsigned_short:
            printf("unsigned short");
            break;
        case unsigned_int:
            printf("unsigned int");
            break;
        case unsigned_long:
            printf("unsigned long");
            break;
        case unsigned_long_long:
            printf("unsigned long long");
            break;
        case float_:
            printf("float");
            break;
        case double_:
            printf("double");
            break;
        case long_double:
            printf("long double");
            break;
        default:
            printf("(unknown primitive %d)", ty->primitive);
        }
        break;
    case array:
        print_type(ty->array.element_type);
        printf("[%zu]", ty->array.length);
        break;
    case structure:
        printf("struct %.*s", (int)ty->structure.name.length,
               ty->structure.name.data);
        break;
    case function: {
        print_type(ty->function.return_type);
        printf("(");
        for (size_t i = 0; i < ty->function.parameters.size; i++) {
            if (i > 0) {
                printf(", ");
            }
            print_type(vector_at(declarator, &ty->function.parameters, i).ty);
        }
        printf(")");
        break;
    }
    case pointer: {
        print_type(ty->pointer.ty);
        printf("*");
        if (ty->pointer.restrict_) {
            printf(" restrict");
        }
        break;
    }
    }

    if (ty->const_) {
        printf(" const ");
    }
    if (ty->volatile_) {
        printf(" volatile ");
    }
}

void init_context(context *ctx, const char *filepath) {
    FILE *source_fp = fopen(filepath, "r");
    if (!source_fp) {
        // Failed to open source file
        longjmp(ctx->error_jump, 1);
    }

    string source;

    fseek(source_fp, 0, SEEK_END);
    source.length = ftell(source_fp);
    fseek(source_fp, 0, SEEK_SET);

    source.data = malloc(source.length);
    if (!source.data) {
        // Failed to allocate memory for source file
        fclose(source_fp);
        longjmp(ctx->error_jump, 1);
    }

    fread(source.data, 1, source.length, source_fp);
    fclose(source_fp);

    ctx->filedir = dirname((char *)filepath);
    ctx->filename = basename((char *)filepath);
    chdir(ctx->filedir);

    ctx->entry = malloc(sizeof(source_entry));
    *ctx->entry = (source_entry){source, 0, NULL};

    __FILE__;

    ctx->types = (vector){0};
    for (int i = 0; i < n_primitive_types; i++) {
        type ty = {0};
        ty.id = i;
        ty.tag = basic;
        ty.primitive = i;
        vector_push(type, &ctx->types, ty);
    }
    ctx->structs = (vector){0};
    ctx->unions = (vector){0};
    ctx->enums = (vector){0};
    ctx->typedefs = (vector){0};

    ctx->macros = (vector){0};

    ctx->globals = (vector){0};
    ctx->functions = (vector){0};

    ctx->result = (vector){0};
}

bool oob_safe_peek(context *ctx, char *c) {
    if (ctx->entry == NULL) {
        return false;
    }
    *c = ctx->entry->source.data[ctx->entry->i];
    return true;
}

char peek(context *ctx) {
    char c;
    if (!oob_safe_peek(ctx, &c)) {
        // unexpected end of input
        longjmp(ctx->error_jump, 1);
    }
    return c;
}

shortstring multipeek(context *ctx, size_t n) {
    source_entry entry = *ctx->entry;
    shortstring result = {0};
    size_t i = 0;
    for (; i < n; i++) {
        result.data[i] = entry.source.data[entry.i];
        if (!inplace_advance(&entry))
            break;
    }
    result.length = i;
    return result;
}

char next_with_whitespace(context *ctx) {
    char c = peek(ctx);
    advance(&ctx->entry);
    return c;
}

void skip_whitespace(context *ctx) {
    char c;
    while (oob_safe_peek(ctx, &c) && isspace(c)) {
        next_with_whitespace(ctx);
    }
}

string get_identifier(context *ctx);
void expect(context *ctx, char c);

string get_multientry_header_file(context *ctx, char delimiter, char *start,
                                  size_t length) {
    vector str = {0};
    for (size_t i = 0; i < length; i++) {
        vector_push(char, &str, start[i]);
    }

    while (peek(ctx) != delimiter && peek(ctx) != '\n') {
        vector_push(char, &str, next_with_whitespace(ctx));
    }
    if (peek(ctx) == '\n') {
        // unterminated header file
        longjmp(ctx->error_jump, 1);
    }

    return (string){str.data, str.size};
}

string get_header_file(context *ctx, char delimiter) {
    source_entry *entry = ctx->entry;
    char *start = entry->source.data + entry->i;
    size_t length = 0;
    while (peek(ctx) != delimiter && peek(ctx) != '\n') {
        next_with_whitespace(ctx);
        length++;
        if (ctx->entry != entry)
            return get_multientry_header_file(ctx, delimiter, start, length);
    }
    if (peek(ctx) == '\n') {
        // unterminated header file
        longjmp(ctx->error_jump, 1);
    }

    return (string){start, length};
}

string resolve_file(context *ctx, string filename, bool resolve_local) {
    if (resolve_local) {
        // resolve local file
        char path[1024] = {0};
        memcpy(path, filename.data, filename.length);
        FILE *file = fopen(path, "r");
        if (file) {
            fseek(file, 0, SEEK_END);
            size_t length = ftell(file);
            fseek(file, 0, SEEK_SET);

            char *buffer = malloc(length);
            fread(buffer, 1, length, file);
            fclose(file);

            return (string){buffer, length};
        }
    }
    // todo: resolve from more places

    // file not found
    longjmp(ctx->error_jump, 1);
}

void insert_entry(context *ctx, string contents) {
    source_entry *new_entry = malloc(sizeof(source_entry));
    *new_entry = (source_entry){contents, 0, ctx->entry};
    ctx->entry = new_entry;
}

void skip_fluff(context *ctx) {
    bool preprocessor_eligible = false;
    char c;
    while (oob_safe_peek(ctx, &c)) {
        if (isspace(c)) {
            if (c == '\n')
                preprocessor_eligible = true;
            next_with_whitespace(ctx);
        } else if (preprocessor_eligible && c == '#') {
            next_with_whitespace(ctx);
            while (oob_safe_peek(ctx, &c) && c == ' ') {
                next_with_whitespace(ctx);
            }
            string ident = get_identifier(ctx);
            if (string_equal(ident, string_literal("include"))) {
                string filename;
                bool resolve_local;
                if (peek(ctx) == '<') {
                    next_with_whitespace(ctx);
                    filename = get_header_file(ctx, '>');
                    expect(ctx, '>');
                    resolve_local = false;
                } else if (peek(ctx) == '"') {
                    next_with_whitespace(ctx);
                    filename = get_header_file(ctx, '"');
                    expect(ctx, '"');
                    resolve_local = true;
                } else {
                    // expected include directive
                    longjmp(ctx->error_jump, 1);
                }
                string contents = resolve_file(ctx, filename, resolve_local);
                insert_entry(ctx, contents);
            } else if (string_equal(ident, string_literal("define"))) {
                // handle #define directive
            } else if (string_equal(ident, string_literal("undef"))) {
                // handle #undef directive
            } else if (string_equal(ident, string_literal("if"))) {
                // handle #if directive
            } else if (string_equal(ident, string_literal("ifdef"))) {
                // handle #ifdef directive
            } else if (string_equal(ident, string_literal("ifndef"))) {
                // handle conditional directives
            } else if (string_equal(ident, string_literal("else"))) {
                // handle #else directive
            } else if (string_equal(ident, string_literal("elif"))) {
                // handle #elif directive
            } else if (string_equal(ident, string_literal("endif"))) {
                // handle #endif directive
            } else if (string_equal(ident, string_literal("error"))) {
                // handle error directive
            } else if (string_equal(ident, string_literal("pragma"))) {
                // handle pragma directive
            } else {
                // unknown preprocessor directive
                longjmp(ctx->error_jump, 1);
            }
        } else {
            break;
        }
    }
}

char next(context *ctx) {
    char c = next_with_whitespace(ctx);
    skip_fluff(ctx);
    return c;
}

string get_multientry_identifier(context *ctx, char *start, size_t length) {
    vector str = {0};
    for (size_t i = 0; i < length; i++) {
        vector_push(char, &str, start[i]);
    }

    while (isalnum(peek(ctx))) {
        vector_push(char, &str, next_with_whitespace(ctx));
    }

    skip_fluff(ctx);
    return (string){str.data, str.size};
}

string get_identifier(context *ctx) {
    if (!isalpha(peek(ctx))) {
        // expected identifier
        longjmp(ctx->error_jump, 1);
    }

    source_entry *entry = ctx->entry;
    char *start = entry->source.data + entry->i;
    size_t length = 0;
    while (isalnum(peek(ctx))) {
        next_with_whitespace(ctx);
        length++;
        if (ctx->entry != entry)
            return get_multientry_identifier(ctx, start, length);
    }

    skip_fluff(ctx);
    return (string){start, length};
}

number_literal get_number(context *ctx) {
    number_literal literal = {true, 0};
    while (isdigit(peek(ctx))) {
        char digit = next_with_whitespace(ctx);
        literal.integer *= 10;
        literal.integer += digit - '0';
    }

    if (peek(ctx) == '.') {
        literal.is_integer = false;
        literal.fp = (double)literal.integer;
        double multiplier = 0.1;
        while (isdigit(peek(ctx))) {
            char digit = next_with_whitespace(ctx);
            literal.fp += (double)(digit - '0') * multiplier;
            multiplier *= 0.1;
        }
    }

    skip_fluff(ctx);
    return literal;
}

punctuator get_punctuator(context *ctx) {
    punctuator punc;

#define CHAR(c, i)                                                             \
    case c:                                                                    \
        punc = i;                                                              \
        break;

    switch (peek(ctx)) {
        CHAR('[', BRACKET_OPEN);
        CHAR(']', BRACKET_CLOSE);
        CHAR('(', PAREN_OPEN);
        CHAR(')', PAREN_CLOSE);
        CHAR('{', CURLY_OPEN);
        CHAR('}', CURLY_CLOSE);
        CHAR('.', DOT);
        CHAR('&', AND);
        CHAR('*', STAR);
        CHAR('+', PLUS);
        CHAR('-', MINUS);
        CHAR('~', NOT);
        CHAR('!', LOGICAL_NOT);
        CHAR('/', DIV);
        CHAR('%', REM);
        CHAR('<', LESS_THAN);
        CHAR('>', GREATER_THAN);
        CHAR('^', XOR);
        CHAR('|', OR);
        CHAR('?', TERNARY_IF);
        CHAR(':', COLON);
        CHAR(';', SEMICOLON);
        CHAR('=', ASSIGN);
        CHAR(',', COMMA);
        CHAR('#', HASH);
    default:
        // unexpected character
        longjmp(ctx->error_jump, 1);
    }
#undef CHAR

    next_with_whitespace(ctx);

#define MULTI(c, i)                                                            \
    if (peek(ctx) == c) {                                                      \
        punc = i;                                                              \
        next_with_whitespace(ctx);                                             \
    }

    if (punc == MINUS) {
        MULTI('>', ARROW) else MULTI('-', DECREMENT) else MULTI('=', ASSIGN_SUB)
    } else if (punc == PLUS) {
        MULTI('+', INCREMENT) else MULTI('=', ASSIGN_ADD)
    } else if (punc == LESS_THAN) {
        MULTI('<', LEFT_SHIFT) else MULTI('=', LESS_EQUAL)
    } else if (punc == GREATER_THAN) {
        MULTI('>', RIGHT_SHIFT) else MULTI('=', GREATER_EQUAL)
    } else if (punc == ASSIGN) {
        MULTI('=', EQUAL_EQUAL)
    } else if (punc == LOGICAL_NOT) {
        MULTI('=', NOT_EQUAL)
    } else if (punc == AND) {
        MULTI('&', AND_AND) else MULTI('=', ASSIGN_AND)
    } else if (punc == OR) {
        MULTI('|', OR_OR) else MULTI('=', ASSIGN_OR)
    } else if (punc == XOR) {
        MULTI('=', ASSIGN_XOR)
    } else if (punc == DIV) {
        MULTI('=', ASSIGN_DIV)
    } else if (punc == REM) {
        MULTI('=', ASSIGN_REM)
    } else if (punc == STAR) {
        MULTI('=', ASSIGN_MUL)
    } else if (punc == HASH) {
        MULTI('#', HASH_HASH)
    } else if (punc == DOT) {
        MULTI('.', PARTIAL_ELLIPSIS)
    }

    if (punc == PARTIAL_ELLIPSIS) {
        MULTI('.', ELLIPSIS) else {
            // expected third dot for ellipsis
            longjmp(ctx->error_jump, 1);
        }
    } else if (punc == LEFT_SHIFT) {
        MULTI('=', ASSIGN_LEFT_SHIFT)
    } else if (punc == RIGHT_SHIFT) {
        MULTI('=', ASSIGN_RIGHT_SHIFT)
    }

#undef MULTI

    return punc;
}

void expect(context *ctx, char c) {
    if (next(ctx) != c) {
        // expected character
        longjmp(ctx->error_jump, 1);
    }
}

value parse_expression(context *ctx);
value parse_comma_expression(context *ctx);
value parse_cast_expression(context *ctx);

const type *parse_type_name(context *ctx) { return NULL; }

uint64_t parse_constant(context *ctx) {
    if (peek(ctx) == '0') {
        next(ctx);
        if (peek(ctx) == 'x' || peek(ctx) == 'X') {
            next(ctx);
            uint64_t value = 0;
            while (isxdigit(peek(ctx))) {
                char c = next_with_whitespace(ctx);
                if (isdigit(c)) {
                    value = value * 16 + (c - '0');
                } else if (c >= 'a' && c <= 'f') {
                    value = value * 16 + (c - 'a' + 10);
                } else if (c >= 'A' && c <= 'F') {
                    value = value * 16 + (c - 'A' + 10);
                }
            }
            skip_fluff(ctx);
            return value;
        } else {
            uint64_t value = 0;
            while ('0' <= peek(ctx) && peek(ctx) <= '7') {
                value = value * 8 + (next_with_whitespace(ctx) - '0');
            }
            skip_fluff(ctx);
            return value;
        }
    } else {
        uint64_t value = 0;
        while (isdigit(peek(ctx))) {
            value = value * 10 + (next_with_whitespace(ctx) - '0');
        }
        skip_fluff(ctx);
        return value;
    }
}

value parse_primary_expression(context *ctx) {
    if (isalpha(peek(ctx))) {
        // identifier / enum constant / generic
        string ident = get_identifier(ctx);
        if (string_equal(ident, string_literal("_Generic"))) {
            // todo: handle _Generic
        } else {
            // todo: turn into value
            // don't forget enums
        }
        return (value){};
    } else if ('0' <= peek(ctx) && peek(ctx) <= '9') {
        // integer / floating constant
        uint64_t constant = parse_constant(ctx);
        return (value){};
    } else if (peek(ctx) == '"') {
        // string literal
        // todo: put string literal into readonly, this is just a pointer
        return (value){};
    } else if (peek(ctx) == '(') {
        // parenthesized expression
        expect(ctx, '(');
        value expr = parse_expression(ctx);
        expect(ctx, ')');
        return expr;
    } else if (peek(ctx) == '\'') {
        // character literal
        expect(ctx, '\'');
        char c = next(ctx);
        expect(ctx, '\'');
        return (value){/* character literal */};
    } else {
        // expected primary expression
        longjmp(ctx->error_jump, 1);
    }
}

value parse_postfix_expression(context *ctx) {
    // todo: support compound literals
    value base = parse_primary_expression(ctx);
    if (peek(ctx) == '[') {
        // array subscript
        expect(ctx, '[');
        value index = parse_expression(ctx);
        expect(ctx, ']');
        // todo: handle array subscript
        return (value){/* array subscript */};
    } else if (peek(ctx) == '(') {
        // function call
        expect(ctx, '(');
        while (peek(ctx) != ')') {
            value arg = parse_expression(ctx);
            if (peek(ctx) != ')') {
                expect(ctx, ',');
            }
        }
        expect(ctx, ')');
        return (value){/* function call */};
    } else if (peek(ctx) == '.') {
        // member access
        next(ctx);
        string member = get_identifier(ctx);
        // todo: handle member access
        return (value){/* member access */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("->"))) {
        // pointer member access
        next(ctx);
        next(ctx);
        string member = get_identifier(ctx);
        return (value){/* pointer member access */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("++"))) {
        // unary increment
        next(ctx);
        next(ctx);
        // do something with the unary increment
        return (value){/* unary increment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("--"))) {
        // unary decrement
        next(ctx);
        next(ctx);
        // do something with the unary decrement
        return (value){/* unary decrement */};
    }
    return base;
}

value parse_unary_expression(context *ctx) {
    if (short_string_equal(multipeek(ctx, 2), shortstring_literal("++"))) {
        next(ctx);
        next(ctx);
        value operand = parse_unary_expression(ctx);
        // do something with the unary plus
        return (value){/* unary plus */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("--"))) {
        next(ctx);
        next(ctx);
        value operand = parse_unary_expression(ctx);
        // do something with the unary minus
        return (value){/* unary minus */};
    } else if (peek(ctx) == '&') {
        next(ctx);
        value operand = parse_cast_expression(ctx);
        // do something with the unary plus
        return (value){/* unary plus */};
    } else if (peek(ctx) == '*') {
        next(ctx);
        value operand = parse_cast_expression(ctx);
        // do something with the unary minus
        return (value){/* unary minus */};
    } else if (peek(ctx) == '+') {
        next(ctx);
        value operand = parse_cast_expression(ctx);
        // do something with the logical not
        return (value){/* logical not */};
    } else if (peek(ctx) == '-') {
        next(ctx);
        value operand = parse_cast_expression(ctx);
        // do something with the bitwise not
        return (value){/* bitwise not */};
    } else if (peek(ctx) == '~') {
        next(ctx);
        value operand = parse_cast_expression(ctx);
        // do something with the address of
        return (value){/* address of */};
    } else if (peek(ctx) == '!') {
        next(ctx);
        value operand = parse_cast_expression(ctx);
        // do something with dereferencing
        return (value){/* dereference */};
    } else if (short_string_equal(multipeek(ctx, 6),
                                  shortstring_literal("sizeof"))) {
        next(ctx);
        const type *ty = NULL;
        if (peek(ctx) == '(') {
            expect(ctx, '(');
            ty = parse_type_name(ctx);
            if (!ty) {
                ty = parse_comma_expression(ctx).ty;
            }
            expect(ctx, ')');
        } else {
            ty = parse_unary_expression(ctx).ty;
        }
        // todo: use ty->size
        return (value){/* sizeof expression */};
    } else {
        // todo: support _Alignof
        return parse_postfix_expression(ctx);
    }
}

value parse_cast_expression(context *ctx) {
    if (peek(ctx) == '(') {
        next(ctx);
        const type *ty = parse_type_name(ctx);
        if (ty) {

        }
        expect(ctx, ')');
        return (value){/* handle cast to type */};
    }
    return parse_unary_expression(ctx);
}

value parse_multiplicative_expression(context *ctx) {
    value left = parse_cast_expression(ctx);

    while (true) {
        if (peek(ctx) == '*') {
            next(ctx);
            value right = parse_cast_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (peek(ctx) == '/') {
            next(ctx);
            value right = parse_cast_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (peek(ctx) == '%') {
            next(ctx);
            value right = parse_cast_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else {
            break;
        }
    }

    return left;
}

value parse_additive_expression(context *ctx) {
    value left = parse_multiplicative_expression(ctx);

    while (true) {
        if (peek(ctx) == '+') {
            next(ctx);
            value right = parse_multiplicative_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (peek(ctx) == '-') {
            next(ctx);
            value right = parse_multiplicative_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else {
            break;
        }
    }

    return left;
}

value parse_shift_expression(context *ctx) {
    value left = parse_additive_expression(ctx);

    while (true) {
        shortstring op = multipeek(ctx, 2);
        if (short_string_equal(op, shortstring_literal("<<"))) {
            next(ctx);
            next(ctx);
            value right = parse_additive_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (short_string_equal(op, shortstring_literal(">>"))) {
            next(ctx);
            next(ctx);
            value right = parse_additive_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else {
            break;
        }
    }

    return left;
}

value parse_relational_expression(context *ctx) {
    value left = parse_shift_expression(ctx);

    while (true) {
        shortstring op = multipeek(ctx, 2);
        if (peek(ctx) == '<') {
            next(ctx);
            value right = parse_shift_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (peek(ctx) == '>') {
            next(ctx);
            value right = parse_shift_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (short_string_equal(op, shortstring_literal("<="))) {
            next(ctx);
            next(ctx);
            value right = parse_shift_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (short_string_equal(op, shortstring_literal(">="))) {
            next(ctx);
            next(ctx);
            value right = parse_shift_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else {
            break;
        }
    }

    return left;
}

value parse_equality_expression(context *ctx) {
    value left = parse_relational_expression(ctx);

    while (true) {
        shortstring op = multipeek(ctx, 2);
        if (short_string_equal(op, shortstring_literal("=="))) {
            next(ctx);
            next(ctx);
            value right = parse_relational_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (short_string_equal(op, shortstring_literal("!="))) {
            next(ctx);
            next(ctx);
            value right = parse_relational_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else {
            break;
        }
    }

    return left;
}

value parse_and_expression(context *ctx) {
    value left = parse_equality_expression(ctx);

    while (peek(ctx) == '&') {
        next(ctx);
        value right = parse_equality_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_exclusive_or_expression(context *ctx) {
    value left = parse_and_expression(ctx);

    while (peek(ctx) == '^') {
        next(ctx);
        value right = parse_and_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_inclusive_or_expression(context *ctx) {
    value left = parse_exclusive_or_expression(ctx);

    while (peek(ctx) == '|') {
        next(ctx);
        value right = parse_exclusive_or_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_logical_and_expression(context *ctx) {
    value left = parse_inclusive_or_expression(ctx);

    while (short_string_equal(multipeek(ctx, 2), shortstring_literal("&&"))) {
        next(ctx);
        next(ctx);
        value right = parse_inclusive_or_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_logical_or_expression(context *ctx) {
    value left = parse_logical_and_expression(ctx);

    while (short_string_equal(multipeek(ctx, 2), shortstring_literal("||"))) {
        next(ctx);
        next(ctx);
        value right = parse_logical_and_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_conditional_expression(context *ctx) {
    value left = parse_logical_or_expression(ctx);

    if (peek(ctx) == '?') {
        next(ctx);
        value true_branch = parse_comma_expression(ctx);
        expect(ctx, ':');
        value false_branch = parse_conditional_expression(ctx);

        // todo: handle conditional expression
        return (value){};
    }

    return left;
}

value parse_assignment_expression(context *ctx) {
    value left = parse_conditional_expression(ctx);

    if (peek(ctx) == '=') {
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("+="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("-="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("*="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("/="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("%="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("&="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("^="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 2),
                                  shortstring_literal("|="))) {
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 3),
                                  shortstring_literal("<<="))) {
        next(ctx);
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (short_string_equal(multipeek(ctx, 3),
                                  shortstring_literal(">>="))) {
        next(ctx);
        next(ctx);
        next(ctx);
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    }

    return left;
}

value parse_expression(context *ctx) {
    return parse_assignment_expression(ctx);
}

value parse_comma_expression(context *ctx) {
    value v = parse_expression(ctx);
    while (peek(ctx) == ',') {
        expect(ctx, ',');
        v = parse_expression(ctx);
    }
    return v;
}

uint64_t execute_constant_expression(context *ctx) {
    if (!isdigit(peek(ctx))) {
        // expected constant expression
        longjmp(ctx->error_jump, 1);
    }

    uint64_t value = 0;
    while (isdigit(peek(ctx))) {
        value = value * 10 + (next_with_whitespace(ctx) - '0');
    }
    skip_fluff(ctx);

    return value;
}

bool parse_storage_class_specifier(context *ctx, string ident,
                                   storage_class_specifier *spec) {
    const string storage_classes[] = {
        string_literal("typedef"), string_literal("extern"),
        string_literal("static"), /* string_literal("_Thread_local"), */
        string_literal("auto"),    string_literal("register"),
    };

    for (int i = 0; i < array_length(storage_classes); i++) {
        if (string_equal(ident, storage_classes[i])) {
            *spec = i;
            return true;
        }
    }
    return false;
}

bool parse_type_specifier(context *ctx, string ident, const type **ty) {
    struct primitive_type_specifier {
        primitive_type type;
        string name;
    };

    const struct primitive_type_specifier primitive_type_specifiers[] = {
        {void_, string_literal("void")},     {char_, string_literal("char")},
        {short_, string_literal("short")},   {int_, string_literal("int")},
        {long_, string_literal("long")},     {float_, string_literal("float")},
        {double_, string_literal("double")}, {bool_, string_literal("_Bool")},
    };

    for (int i = 0; i < array_length(primitive_type_specifiers); i++) {
        if (string_equal(ident, primitive_type_specifiers[i].name)) {
            *ty = &vector_at(type, &ctx->types,
                             primitive_type_specifiers[i].type);
            return true;
        }
    }

    return false;
}

bool parse_type_qualifier(context *ctx, string ident, type_qualifier *mask) {
    const string type_qualifiers[] = {
        string_literal("const"), string_literal("volatile"),
        string_literal("restrict"),
        // string_literal("_Atomic"),
    };

    for (int i = 0; i < array_length(type_qualifiers); i++) {
        if (string_equal(ident, type_qualifiers[i])) {
            *mask |= (1 << i);
            return true;
        }
    }
    return false;
}

bool parse_function_specifier(context *ctx, string ident,
                              function_specifier *mask) {
    const string function_specifiers[] = {
        string_literal("inline"),
        string_literal("_Noreturn"),
    };

    for (int i = 0; i < array_length(function_specifiers); i++) {
        if (string_equal(ident, function_specifiers[i])) {
            *mask |= (1 << i);
            return true;
        }
    }
    return false;
}

bool parse_alignment_specifier(context *ctx, string ident, size_t *align) {
    if (string_equal(ident, string_literal("_Alignas"))) {
        expect(ctx, '(');
        *align = execute_constant_expression(ctx);
        expect(ctx, ')');
        return true;
    }

    return false;
}

type *finalize_type(context *ctx, partial_type *partial_ty) {
    type *ty = calloc(1, sizeof(type));
    if (!ty) {
        longjmp(ctx->error_jump, 1);
    }

    *ty = *partial_ty->ty;
    ty->const_ = (partial_ty->type_qualifier_spec_mask & const_) != 0;
    ty->volatile_ = (partial_ty->type_qualifier_spec_mask & volatile_) != 0;
    if (partial_ty->type_qualifier_spec_mask & restrict_) {
        if (ty->tag != pointer) {
            // restrict is only valid for pointers
            longjmp(ctx->error_jump, 1);
        } else {
            ty->pointer.restrict_ = true;
        }
    }

    return ty;
}

string parse_pointer_opt(context *ctx, const type **ty) {
    if (peek(ctx) != '*') {
        return invalid_str;
    }
    expect(ctx, '*');

    string ident = invalid_str;
    type_qualifier mask = 0;
    while (1) {
        if (!isalpha(peek(ctx))) {
            ident = invalid_str;
            break;
        }

        ident = get_identifier(ctx);
        if (!parse_type_qualifier(ctx, ident, &mask)) {
            break;
        }
    }

    type *pointer_ty = calloc(1, sizeof(type));
    if (!pointer_ty) {
        longjmp(ctx->error_jump, 1);
    }

    pointer_ty->tag = pointer;
    pointer_ty->pointer.restrict_ = false; // default to false
    pointer_ty->pointer.ty = *ty;

    *ty = pointer_ty;

    if (mask & restrict_)
        pointer_ty->pointer.restrict_ = true;
    if (mask & const_)
        pointer_ty->const_ = true;
    if (mask & volatile_)
        pointer_ty->volatile_ = true;

    if (!string_is_valid(ident)) {
        return parse_pointer_opt(ctx, ty);
    } else {
        return ident;
    }
}

string parse_declaration_specifiers(context *ctx, partial_type *partial_ty) {
    string ident = invalid_str;

    while (1) {
        if (!isalpha(peek(ctx))) {
            ident = invalid_str;
            break;
        } else {
            ident = get_identifier(ctx);
        }

        if (parse_alignment_specifier(ctx, ident, &partial_ty->alignment))
            continue;
        if (parse_function_specifier(ctx, ident,
                                     &partial_ty->function_spec_mask))
            continue;
        if (parse_storage_class_specifier(ctx, ident,
                                          &partial_ty->storage_class_spec))
            continue;
        if (parse_type_qualifier(ctx, ident,
                                 &partial_ty->type_qualifier_spec_mask))
            continue;
        if (parse_type_specifier(ctx, ident, &partial_ty->ty))
            continue;

        break;
    }

    return ident;
}

declarator parse_declarator(context *ctx, const type *ty, string ident);

declarator parse_parameter_declaration(context *ctx) {
    partial_type partial_ty = {0};
    string ident = parse_declaration_specifiers(ctx, &partial_ty);
    type *ty = finalize_type(ctx, &partial_ty);

    // todo: support abstract declarators (no identifier)
    return parse_declarator(ctx, ty, ident);
}

owned_span parse_parameter_list(context *ctx) {
    vector params = {0};

    while (peek(ctx) != ')') {
        declarator decl = parse_parameter_declaration(ctx);
        vector_push(declarator, &params, decl);
        if (peek(ctx) != ')') {
            expect(ctx, ',');
        }
    }

    return owned_span_from_vector(params);
}

declarator parse_direct_declarator(context *ctx, const type *ty, string ident) {
    if (!string_is_valid(ident) && peek(ctx) == '(') {
        expect(ctx, '(');
        declarator decl = parse_declarator(ctx, ty, invalid_str);
        expect(ctx, ')');
        ident = decl.name;
        ty = decl.ty;
    }

    ident = string_is_valid(ident) ? ident : get_identifier(ctx);

    if (peek(ctx) == '[') {
        expect(ctx, '[');
        uint64_t length = execute_constant_expression(ctx);
        expect(ctx, ']');

        type *array_ty = calloc(1, sizeof(type));
        if (!array_ty) {
            longjmp(ctx->error_jump, 1);
        }

        array_ty->tag = array;
        array_ty->array.element_type = ty;
        array_ty->array.length = length;

        ty = array_ty;
    } else if (peek(ctx) == '(') {
        expect(ctx, '(');
        owned_span params = parse_parameter_list(ctx);
        expect(ctx, ')');

        type *func_ty = calloc(1, sizeof(type));
        if (!func_ty) {
            longjmp(ctx->error_jump, 1);
        }

        func_ty->tag = function;
        func_ty->function.return_type = ty;
        func_ty->function.parameters = params;

        ty = func_ty;
    }

    return (declarator){ident, ty};
}

declarator parse_declarator(context *ctx, const type *ty, string ident) {
    if (!string_is_valid(ident))
        ident = parse_pointer_opt(ctx, &ty);
    return parse_direct_declarator(ctx, ty, ident);
}

// returns a span of declarators
owned_span parse_declaration(context *ctx) {
    partial_type partial_ty = {0};
    string ident = parse_declaration_specifiers(ctx, &partial_ty);
    type *ty = finalize_type(ctx, &partial_ty);

    vector declarators = {0};

    while (peek(ctx) != ';') {
        vector_push(declarator, &declarators, parse_declarator(ctx, ty, ident));
        if (peek(ctx) == '=') {
            expect(ctx, '=');
            value init_value = parse_expression(ctx);
            // todo: handle initialization, initializer list
        }
        if (peek(ctx) == ',') {
            expect(ctx, ',');
            ident = invalid_str;
        } else {
            break;
        }
    }

    expect(ctx, ';');

    return owned_span_from_vector(declarators);
}

string parse_statement(context *ctx);

bool parse_compound_statement(context *ctx) {
    if (peek(ctx) != '{')
        return false;
    expect(ctx, '{');

    while (peek(ctx) != '}') {
        // somehow disambiguate between declaration and statement
    }

    expect(ctx, '}');
    return true;
}

string parse_statement(context *ctx) {
    string ident = get_identifier(ctx);

    return ident;
}

void parse_external_declaration(context *ctx) {
    owned_span declarations = parse_declaration(ctx);

    if (declarations.size == 1 &&
        vector_at(declarator, &declarations, 0).ty->tag == function &&
        peek(ctx) == '{') {
        parse_compound_statement(ctx);
    }

    for (int i = 0; i < declarations.size; i++) {
        declarator decl = vector_at(declarator, &declarations, i);
        global glob = {
            .name = decl.name,
            .ty = decl.ty,
            .offset = 0, // todo: when i actually start the object file shit
        };
        vector_push(global, &ctx->globals, glob);

        printf("Declarator %d: %.*s -> ", i, (int)decl.name.length,
               decl.name.data);
        print_type(decl.ty);
        printf("\n");
    }
}

owned_span compile(const char *filepath) {
    context ctx;

    if (setjmp(ctx.error_jump)) {
        // todo: actually communicate information
        ctx.result.size = invalid_length;
    } else {
        init_context(&ctx, filepath);
        skip_fluff(&ctx);

        while (ctx.entry) {
            parse_external_declaration(&ctx);
        }
    }

    return owned_span_from_vector(ctx.result);
}
