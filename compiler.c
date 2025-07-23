#include "./compiler.h"
#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <stdio.h>
#include <unistd.h>

#define array_length(arr) (sizeof(arr) / sizeof((arr)[0]))

// instruction functions
void raw_noop(vector *code) { vector_push(uint32_t, code, 0xd503201f); }

typedef enum enctype {
    offset = 0b10,
    preidx = 0b11,
    pstidx = 0b01,
} enctype;

void raw_ldp(vector *code, bool sf, enctype enc, int16_t imm, ireg rt2, ireg rn,
             ireg rt) {
    size_t width = sf ? sizeof(uint64_t) : sizeof(uint32_t);
    assert(imm % width == 0);
    imm /= width;
    assert(imm >= -64 && imm <= 63);
    uint8_t imm7 = (uint8_t)imm & 0x7f;

    vector_push(uint32_t, code,
                0b00101000010000000000000000000000 | ((uint32_t)sf << 31) |
                    ((uint32_t)enc << 23) | ((uint32_t)imm7 << 15) |
                    ((uint32_t)rt2 << 10) | ((uint32_t)rn << 5) |
                    ((uint32_t)rt << 0));
}

void raw_stp(vector *code, bool sf, enctype enc, int16_t imm, ireg rt2, ireg rn,
             ireg rt) {
    size_t width = sf ? sizeof(uint64_t) : sizeof(uint32_t);
    assert(imm % width == 0);
    imm /= width;
    assert(imm >= -64 && imm <= 63);
    uint8_t imm7 = (uint8_t)imm & 0x7f;

    vector_push(uint32_t, code,
                0b00101000000000000000000000000000 | ((uint32_t)sf << 31) |
                    ((uint32_t)enc << 23) | ((uint32_t)imm7 << 15) |
                    ((uint32_t)rt2 << 10) | ((uint32_t)rn << 5) |
                    ((uint32_t)rt << 0));
}

typedef enum shifttype {
    lsl = 0b00,
    lsr = 0b01,
    asr = 0b10,
    ror = 0b11,
} shifttype;

void raw_addsub(vector *code, bool sf, bool sub, bool setflags, shifttype shift,
                ireg rm, uint8_t shift_n, ireg rn, ireg rd) {
    assert(shift_n < (sf ? 64 : 32));

    vector_push(uint32_t, code,
                0b00001011000000000000000000000000 | ((uint32_t)sf << 31) |
                    ((uint32_t)sub << 30) | ((uint32_t)setflags << 29) |
                    ((uint32_t)shift << 22) | ((uint32_t)rm << 16) |
                    ((uint32_t)shift_n << 10) | ((uint32_t)rn << 5) |
                    ((uint32_t)rd << 0));
}

void raw_addsub_cons(vector *code, bool sf, bool sub, bool setflags, bool shift,
                     uint16_t imm12, ireg rn, ireg rd) {
    assert(imm12 < 1 << 12);

    vector_push(uint32_t, code,
                0b00010001000000000000000000000000 | ((uint32_t)sf << 31) |
                    ((uint32_t)sub << 30) | ((uint32_t)setflags << 29) |
                    ((uint32_t)shift << 22) | ((uint32_t)imm12 << 10) |
                    ((uint32_t)rn << 5) | ((uint32_t)rd << 0));
}

void raw_add(vector *code, bool sf, ireg rm, ireg rn, ireg rd, shifttype shift,
             uint8_t shift_n) {
    raw_addsub(code, sf, false, false, shift, rm, shift_n, rn, rd);
}

void raw_sub(vector *code, bool sf, ireg rm, ireg rn, ireg rd, shifttype shift,
             uint8_t shift_n) {
    raw_addsub(code, sf, true, false, shift, rm, shift_n, rn, rd);
}

void raw_add_cons(vector *code, bool sf, uint16_t imm12, ireg rn, ireg rd,
                  bool shift) {
    raw_addsub_cons(code, sf, false, false, shift, imm12, rn, rd);
}

void raw_sub_cons(vector *code, bool sf, uint16_t imm12, ireg rn, ireg rd,
                  bool shift) {
    raw_addsub_cons(code, sf, true, false, shift, imm12, rn, rd);
}

void raw_orr(vector *code, bool sf, shifttype shift, ireg rm, uint8_t shift_imm,
             ireg rn, ireg rd) {
    assert(shift_imm < (sf ? 64 : 32));

    vector_push(uint32_t, code,
                0b00101010000000000000000000000000 | ((uint32_t)sf << 31) |
                    ((uint32_t)shift << 22) | ((uint32_t)rm << 16) |
                    ((uint32_t)shift_imm << 10) | ((uint32_t)rn << 5) |
                    ((uint32_t)rd << 0));
}

void raw_mov(vector *code, bool sf, ireg src, ireg dst) {
    raw_orr(code, sf, lsl, src, 0, xzr, dst);
}

void raw_restoring_return(vector *code) {
    raw_ldp(code, true, offset, 0, x30, sp, x29);
    vector_push(uint32_t, code, 0b11010110010111110000001111000000);
}

// regalloc functions
void force_into_specific_ireg(regallocator *regs, vector *code, struct value *v,
                              ireg dest) {
    assert(v->loc == reg);

    if (v->ireg == dest) {
        return;
    }

    raw_mov(code, true, v->ireg, dest);
}

void force_into_ireg(regallocator *regs, vector *code, struct value *v) {
    assert(v->loc == reg);
}

int current_reg = 3;

ireg get_new_ireg(regallocator *regs, vector *code) {
    return (ireg)current_reg++;
}

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

void print_token(token tok) {
    switch (tok.type) {
    case TOKEN_EOF:
        printf("EOF\n");
        break;
    case TOKEN_KEYWORD:
        printf("keyword %.*s\n", (int)keyword_names[tok.kw].length,
               keyword_names[tok.kw].data);
        break;
    case TOKEN_IDENTIFIER:
        printf("identifier %.*s\n", (int)tok.ident.length, tok.ident.data);
        break;
    case TOKEN_NUMBER:
        printf("number %lld\n", tok.num.integer);
        break;
    case TOKEN_STRING:
        printf("string %.*s\n", (int)tok.str.str.length, tok.str.str.data);
        break;
    case TOKEN_PUNCTUATOR:
        printf("punctuator %.*s\n", (int)punctuator_names[tok.punc].length,
               punctuator_names[tok.punc].data);
        break;
    }
}

token read_token(context *ctx);

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

    ctx->sysdirs = (vector){0};
    char *sysdir = calloc(1024, sizeof(char));
    if (!sysdir) {
        // Failed to allocate memory for system directory
        longjmp(ctx->error_jump, 1);
    }
    // get the system include directory
    // execute `xcrun --show-sdk-path` to get the SDK path
    FILE *fp = popen("xcrun --show-sdk-path", "r");
    if (!fp) {
        // Failed to run command
        longjmp(ctx->error_jump, 1);
    }
    fread(sysdir, 1, 1024, fp);
    pclose(fp);

    vector_push(char *, &ctx->sysdirs, sysdir);

    ctx->quotedirs = (vector){0};

    ctx->entry = malloc(sizeof(source_entry));
    *ctx->entry = (source_entry){source, 0, NULL};

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

    ctx->enum_values = (vector){0};

    ctx->macho = (macho_builder){0};

    ctx->tokens[0] = read_token(ctx);
    ctx->tokens[1] = read_token(ctx);
}

bool oob_safe_peek(context *ctx, char *c) {
    if (ctx->entry == NULL) {
        return false;
    }
    *c = ctx->entry->source.data[ctx->entry->i];
    return true;
}

char peek_char(context *ctx) {
    char c;
    if (!oob_safe_peek(ctx, &c)) {
        // unexpected end of input
        longjmp(ctx->error_jump, 1);
    }
    return c;
}

char next_char(context *ctx) {
    char c = peek_char(ctx);
    advance(&ctx->entry);
    return c;
}

void expect_char(context *ctx, char c) {
    if (next_char(ctx) != c) {
        // expected character
        longjmp(ctx->error_jump, 1);
    }
}

bool skip_whitespace(context *ctx) {
    char c;
    bool newline = false;
    while (oob_safe_peek(ctx, &c)) {
        if (isspace(c)) {
            next_char(ctx);
            newline |= c == '\n';
        } else if (c == '\\') {
            next_char(ctx);
            expect_char(ctx, '\n');
        } else {
            break;
        }
    }
    return newline;
}

void skip_line_comment(context *ctx) {
    char c;
    while (oob_safe_peek(ctx, &c)) {
        next_char(ctx);
        if (c == '\\') {
            // todo: technically a splice can go on last line of a file
            expect_char(ctx, '\n');
        } else if (c == '\n') {
            return;
        }
    }
}

void skip_multiline_comment(context *ctx) {
    while (1) {
        char c = next_char(ctx);
        if (c == '*' && peek_char(ctx) == '/') {
            next_char(ctx);
            return;
        }
    }
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
    for (size_t i = 0; i < ctx->sysdirs.size; i++) {
        char *dir = vector_at(char *, &ctx->sysdirs, i);
        char path[1024] = {0};
        snprintf(path, sizeof(path), "%s/%.*s", dir, (int)filename.length,
                 filename.data);
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

    // file not found
    longjmp(ctx->error_jump, 1);
}

void insert_entry(context *ctx, string contents) {
    source_entry *new_entry = malloc(sizeof(source_entry));
    *new_entry = (source_entry){contents, 0, ctx->entry};
    ctx->entry = new_entry;
}

bool isidentfirst(char c) { return isalpha(c) || c == '_' || c == '$'; }
bool isident(char c) { return isidentfirst(c) || isdigit(c); }

string get_multientry_identifier(context *ctx, char *start, size_t length) {
    vector str = {0};
    for (size_t i = 0; i < length; i++) {
        vector_push(char, &str, start[i]);
    }

    while (isident(peek_char(ctx))) {
        vector_push(char, &str, next_char(ctx));
    }

    return (string){str.data, str.size};
}

string get_identifier(context *ctx) {
    source_entry *entry = ctx->entry;
    char *start = entry->source.data + entry->i;
    size_t length = 0;
    while (isident(peek_char(ctx))) {
        next_char(ctx);
        length++;
        if (ctx->entry != entry)
            return get_multientry_identifier(ctx, start, length);
    }

    return (string){start, length};
}

char get_octal(context *ctx, char first) {
    char value = first - '0';
    for (int i = 0; i < 2; i++)
        if ('0' <= peek_char(ctx) && peek_char(ctx) <= '7')
            value = (value * 010) | (next_char(ctx) - '0');
    return (char)value;
}

char get_hex(context *ctx) {
    char value = 0;
    while (isxdigit(peek_char(ctx))) {
        char digit = next_char(ctx);
        if ('0' <= digit && digit <= '9') {
            value = (value * 0x10) | (digit - '0');
        } else if ('a' <= digit && digit <= 'f') {
            value = (value * 0x10) | (digit - 'a' + 10);
        } else if ('A' <= digit && digit <= 'F') {
            value = (value * 0x10) | (digit - 'A' + 10);
        } else {
            // invalid hex character
            longjmp(ctx->error_jump, 1);
        }
    }
    return value;
}

char get_quoted_char(context *ctx) {
    if (peek_char(ctx) == '\\') {
        next_char(ctx);
        char c = next_char(ctx);

        // simple escape sequences
        char escaped[] = {'\'', '"', '?', '\\', 'a', 'b',
                          'f',  'n', 'r', 't',  'v'};
        for (size_t i = 0; i < sizeof(escaped); i++) {
            if (c == escaped[i]) {
                return escaped[i];
            }
        }
        // octal escape sequence
        if ('0' <= c && c <= '7') {
            return get_octal(ctx, c);
        }
        // hex escape sequence
        if (c == 'x') {
            return get_hex(ctx);
        }

        // unknown escape sequence
        longjmp(ctx->error_jump, 1);
    } else {
        return next_char(ctx);
    }
}

int get_char_constant(context *ctx) { return get_quoted_char(ctx); }

string_literal get_string_literal(context *ctx) {
    string_literal str = {0};

    vector s = {0};
    while (peek_char(ctx) == '"') {
        next_char(ctx);
        while (peek_char(ctx) != '"' && peek_char(ctx) != '\n') {
            vector_push(char, &s, get_quoted_char(ctx));
        }
        if (next_char(ctx) != '"') {
            // expected ending bracket
            longjmp(ctx->error_jump, 1);
        }
    }

    str.w = regular;
    str.str = (string){s.data, s.size};
    return str;
}

number_literal get_number(context *ctx) {
    number_literal literal = {.is_integer = true, .integer = 0};
    while (isdigit(peek_char(ctx))) {
        char digit = next_char(ctx);
        literal.integer *= 10;
        literal.integer += digit - '0';
    }

    if (peek_char(ctx) == '.') {
        literal.is_integer = false;
        literal.fp = (double)literal.integer;
        double multiplier = 0.1;
        while (isdigit(peek_char(ctx))) {
            char digit = next_char(ctx);
            literal.fp += (double)(digit - '0') * multiplier;
            multiplier *= 0.1;
        }
    }

    return literal;
}

punctuator get_punctuator(context *ctx) {
    punctuator punc = INVALID;

#define CHAR(c, i)                                                             \
    case c:                                                                    \
        punc = i;                                                              \
        break;

    switch (peek_char(ctx)) {
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

    next_char(ctx);

#define MULTI(c, i)                                                            \
    if (peek_char(ctx) == c) {                                                 \
        punc = i;                                                              \
        next_char(ctx);                                                        \
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
        MULTI('=', EQUAL)
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

string get_multientry_header_file(context *ctx, char delimiter, char *start,
                                  size_t length) {
    vector str = {0};
    for (size_t i = 0; i < length; i++) {
        vector_push(char, &str, start[i]);
    }

    while (peek_char(ctx) != delimiter && peek_char(ctx) != '\n') {
        vector_push(char, &str, next_char(ctx));
    }
    if (peek_char(ctx) == '\n') {
        // unterminated header file
        longjmp(ctx->error_jump, 1);
    }

    return (string){str.data, str.size};
}

string get_header_file(context *ctx, char delimiter) {
    source_entry *entry = ctx->entry;
    char *start = entry->source.data + entry->i;
    size_t length = 0;
    while (peek_char(ctx) != delimiter && peek_char(ctx) != '\n') {
        next_char(ctx);
        length++;
        if (ctx->entry != entry)
            return get_multientry_header_file(ctx, delimiter, start, length);
    }
    if (peek_char(ctx) == '\n') {
        // unterminated header file
        longjmp(ctx->error_jump, 1);
    }

    return (string){start, length};
}

void do_define(context *ctx) { /* todo */
}

void do_undef(context *ctx) {
    string name = get_identifier(ctx);
    for (size_t i = 0; i < ctx->macros.size; i++) {
        macro m = vector_at(macro, &ctx->macros, i);
        if (string_equal(m.name, name)) {
            vector_remove(macro, &ctx->macros, i);
            break;
        }
    }
}

token read_token(context *ctx) {
    token t;

    bool directive_eligible = skip_whitespace(ctx);
    if (ctx->entry == NULL) {
        t.type = TOKEN_EOF;
        t.ident = invalid_str;
    } else if (directive_eligible && peek_char(ctx) == '#') {
        // preprocessor directive
        next_char(ctx);
        string directive = get_identifier(ctx);

        if (string_equal(directive, string_literal("include"))) {
            while (peek_char(ctx) == ' ')
                next_char(ctx);

            string filename;
            bool resolve_local;
            if (peek_char(ctx) == '<') {
                next_char(ctx);
                filename = get_header_file(ctx, '>');
                expect_char(ctx, '>');
                resolve_local = false;
            } else if (peek_char(ctx) == '"') {
                next_char(ctx);
                filename = get_header_file(ctx, '"');
                expect_char(ctx, '"');
                resolve_local = true;
            } else {
                // expected include directive
                longjmp(ctx->error_jump, 1);
            }
            string contents = resolve_file(ctx, filename, resolve_local);
            insert_entry(ctx, contents);
        } else if (string_equal(directive, string_literal("define"))) {
            do_define(ctx);
        } else if (string_equal(directive, string_literal("undef"))) {
            do_undef(ctx);
        } else if (string_equal(directive, string_literal("if"))) {
            // handle #if directive
        } else if (string_equal(directive, string_literal("ifdef"))) {
            // handle #ifdef directive
        } else if (string_equal(directive, string_literal("ifndef"))) {
            // handle conditional directives
        } else if (string_equal(directive, string_literal("else"))) {
            // handle #else directive
        } else if (string_equal(directive, string_literal("elif"))) {
            // handle #elif directive
        } else if (string_equal(directive, string_literal("endif"))) {
            // handle #endif directive
        } else if (string_equal(directive, string_literal("error"))) {
            // show the error message
            longjmp(ctx->error_jump, 1);
        } else if (string_equal(directive, string_literal("pragma"))) {
            // handle pragma directive
        } else {
            // unknown preprocessor directive
            longjmp(ctx->error_jump, 1);
        }

        return read_token(ctx);
    } else if (isdigit(peek_char(ctx))) {
        t.type = TOKEN_NUMBER;
        t.num = get_number(ctx);
    } else if (peek_char(ctx) == '"') {
        t.type = TOKEN_STRING;
        t.str = get_string_literal(ctx);
    } else if (peek_char(ctx) == '\'') {
        t.type = TOKEN_NUMBER;
        t.num.is_integer = true;
        t.num.integer = get_char_constant(ctx);
    } else if (isidentfirst(peek_char(ctx))) {
        string ident = get_identifier(ctx);
        for (size_t i = 0; i < ctx->macros.size; i++) {
            macro m = vector_at(macro, &ctx->macros, i);
            if (string_equal(m.name, ident)) {
                /* handle it somehow */
            }
        }
        for (size_t i = 0; i < array_length(keyword_names); i++) {
            if (string_equal(ident, keyword_names[i])) {
                t.type = TOKEN_KEYWORD;
                t.kw = i;
                return t;
            }
        }
        for (size_t i = 0; i < ctx->enum_values.size; i++) {
            enum_value ev = vector_at(enum_value, &ctx->enum_values, i);
            if (string_equal(ident, ev.name)) {
                t.type = TOKEN_NUMBER;
                t.num.is_integer = true;
                t.num.integer = ev.value;
                return t;
            }
        }
        t.type = TOKEN_IDENTIFIER;
        t.ident = ident;
    } else if (peek_char(ctx) == '/') {
        next_char(ctx);
        if (peek_char(ctx) == '/') {
            next_char(ctx);
            skip_line_comment(ctx);
            return read_token(ctx);
        } else if (peek_char(ctx) == '*') {
            next_char(ctx);
            skip_multiline_comment(ctx);
            return read_token(ctx);
        } else {
            t.type = TOKEN_PUNCTUATOR;
            t.punc = DIV;
        }
    } else {
        t.type = TOKEN_PUNCTUATOR;
        t.punc = get_punctuator(ctx);
    }

    return t;
}

token peek(context *ctx) { return ctx->tokens[0]; }
token peek2(context *ctx) { return ctx->tokens[1]; }

token next(context *ctx) {
    token t = peek(ctx);
    ctx->tokens[0] = peek2(ctx);
    ctx->tokens[1] = read_token(ctx);
    return t;
}

bool soft_check_punc(context *ctx, punctuator punc) {
    token tok = peek(ctx);
    if (tok.type == TOKEN_PUNCTUATOR && tok.punc == punc) {
        return true;
    }
    return false;
}

bool check_punc(context *ctx, punctuator punc) {
    if (soft_check_punc(ctx, punc)) {
        next(ctx);
        return true;
    }
    return false;
}

void expect_punc(context *ctx, punctuator punc) {
    if (!check_punc(ctx, punc)) {
        // expected punctuator
        longjmp(ctx->error_jump, 1);
    }
}

bool check_keyword(context *ctx, keyword kw) {
    token tok = peek(ctx);
    if (tok.type == TOKEN_KEYWORD && tok.kw == kw) {
        next(ctx);
        return true;
    }
    return false;
}

void expect_keyword(context *ctx, keyword kw) {
    if (!check_keyword(ctx, kw)) {
        // expected keyword
        longjmp(ctx->error_jump, 1);
    }
}

void undergo_integer_promotion(context *ctx, const type **ty) {
    if ((*ty)->tag != basic)
        return;

    static const primitive_type below_integers[] = {
        bool_, signed_char, char_, unsigned_char, short_, unsigned_short,
    };

    for (size_t i = 0; i < array_length(below_integers); i++) {
        if ((*ty)->primitive == below_integers[i]) {
            *ty = &vector_at(type, &ctx->types, int_);
            return;
        }
    }
}

void undergo_arithmetic_conversion(context *ctx, const type **ty1,
                                   const type **ty2) {
    if ((*ty1)->tag != basic || (*ty2)->tag != basic)
        return;

    if ((*ty1)->primitive == (*ty2)->primitive)
        return;

    const type **larger = (*ty1)->primitive > (*ty2)->primitive ? ty1 : ty2;

    primitive_type conversion = void_;

    if ((*larger)->primitive == long_double ||
        (*larger)->primitive == double_ || (*larger)->primitive == float_) {
        conversion = (*larger)->primitive;
    } else {
        undergo_integer_promotion(ctx, ty1);
        undergo_integer_promotion(ctx, ty2);

        if ((*ty1)->primitive == (*ty2)->primitive) {
            // If the types are the same, that type is the common type.
            return;
        } else if (is_signed((*ty1)->primitive) &&
                   is_signed((*ty2)->primitive)) {
            // If the types have the same signedness (both signed or both
            // unsigned), the operand whose type has the lesser conversion rank
            // is implicitly converted to the other type.
            conversion = (*larger)->primitive;
        } else {
            const type *signed_ty = is_signed((*ty1)->primitive) ? *ty1 : *ty2;
            const type *unsigned_ty = signed_ty == *ty1 ? *ty2 : *ty1;

            if (unsigned_ty->primitive > signed_ty->primitive) {
                // If the unsigned type has conversion rank greater than or
                // equal to the rank of the signed type, then the operand with
                // the signed type is implicitly converted to the unsigned type.
                conversion = unsigned_ty->primitive;
            } else if ((signed_ty->primitive == long_long ||
                        signed_ty->primitive == long_) &&
                       unsigned_ty->primitive == unsigned_int) {
                // If the signed type can represent all values of the unsigned
                // type, then the operand with the unsigned type is implicitly
                // converted to the signed type. (Since it's at least an int,
                // unsigned int is the only applicable unsigned type)
                conversion = signed_ty->primitive;
            } else {
                // Else, both operands undergo implicit conversion to the
                // unsigned type counterpart of the signed operand's type.
                conversion = to_unsigned(signed_ty->primitive);
            }
        }
    }

    if (conversion == float_ || conversion == double_ ||
        conversion == long_double) {
        // floating point conversions actually need code generation
    }

    *ty1 = &vector_at(type, &ctx->types, conversion);
    *ty2 = *ty1;
}

value parse_expression(context *ctx);
value parse_assignment_expression(context *ctx);
value parse_cast_expression(context *ctx);

const type *parse_type_name(context *ctx) { return NULL; }

value parse_primary_expression(context *ctx) {
    token tok = next(ctx);
    if (tok.type == TOKEN_IDENTIFIER) {
        // identifier
        for (size_t i = 0; i < ctx->variables.size; i++) {
            variable var = vector_at(variable, &ctx->variables, i);
            if (string_equal(var.name, tok.ident)) {
                return var.val;
            }
        }
        for (size_t i = 0; i < ctx->globals.size; i++) {
            // todo: globals
        }
        for (size_t i = 0; i < ctx->functions.size; i++) {
            // todo: function pointers
        }

        // expected expression
        longjmp(ctx->error_jump, 1);
    } else if (tok.type == TOKEN_NUMBER) {
        // integer / floating constant
        number_literal constant = tok.num;
        return (value){};
    } else if (tok.type == TOKEN_STRING) {
        // string literal
        // todo: put string literal into readonly, this is just a pointer
        return (value){};
    } else if (tok.type == TOKEN_PUNCTUATOR && tok.punc == '(') {
        // parenthesized expression
        value expr = parse_expression(ctx);
        expect_punc(ctx, PAREN_CLOSE);
        return expr;
    } else if (tok.type == TOKEN_KEYWORD && tok.kw == _Generic_kw) {
        // _Generic expression
        return (value){/* handle _Generic expression */};
    } else {
        // expected primary expression
        longjmp(ctx->error_jump, 1);
    }
}

value parse_postfix_expression(context *ctx) {
    // todo: support compound literals
    value base = parse_primary_expression(ctx);
    if (check_punc(ctx, BRACKET_OPEN)) {
        // array subscript
        value index = parse_assignment_expression(ctx);
        expect_punc(ctx, BRACKET_CLOSE);
        // todo: handle array subscript
        return (value){/* array subscript */};
    } else if (check_punc(ctx, PAREN_OPEN)) {
        // function call
        while (!check_punc(ctx, PAREN_CLOSE)) {
            value arg = parse_assignment_expression(ctx);
            if (!check_punc(ctx, PAREN_CLOSE)) {
                expect_punc(ctx, COMMA);
            }
        }
        return (value){/* function call */};
    } else if (check_punc(ctx, DOT)) {
        // member access
        string member = get_identifier(ctx);
        // todo: handle member access
        return (value){/* member access */};
    } else if (check_punc(ctx, ARROW)) {
        // pointer member access
        string member = get_identifier(ctx);
        return (value){/* pointer member access */};
    } else if (check_punc(ctx, INCREMENT)) {
        // unary increment
        // do something with the unary increment
        return (value){/* unary increment */};
    } else if (check_punc(ctx, DECREMENT)) {
        // unary decrement
        // do something with the unary decrement
        return (value){/* unary decrement */};
    }
    return base;
}

value parse_unary_expression(context *ctx) {
    if (check_punc(ctx, INCREMENT)) {
        value operand = parse_unary_expression(ctx);
        // do something with the unary plus
        return (value){/* unary plus */};
    } else if (check_punc(ctx, DECREMENT)) {
        value operand = parse_unary_expression(ctx);
        // do something with the unary minus
        return (value){/* unary minus */};
    } else if (check_punc(ctx, AND)) {
        value operand = parse_cast_expression(ctx);
        // do something with the unary plus
        return (value){/* unary plus */};
    } else if (check_punc(ctx, STAR)) {
        value operand = parse_cast_expression(ctx);
        // do something with the unary minus
        return (value){/* unary minus */};
    } else if (check_punc(ctx, PLUS)) {
        value operand = parse_cast_expression(ctx);
        // do something with the logical not
        return (value){/* logical not */};
    } else if (check_punc(ctx, MINUS)) {
        value operand = parse_cast_expression(ctx);
        // do something with the bitwise not
        return (value){/* bitwise not */};
    } else if (check_punc(ctx, NOT)) {
        value operand = parse_cast_expression(ctx);
        // do something with the address of
        return (value){/* address of */};
    } else if (check_punc(ctx, LOGICAL_NOT)) {
        value operand = parse_cast_expression(ctx);
        // do something with dereferencing
        return (value){/* dereference */};
    } else if (peek(ctx).type == TOKEN_IDENTIFIER &&
               peek(ctx).kw == sizeof_kw) {
        next(ctx);
        const type *ty = NULL;
        if (check_punc(ctx, PAREN_OPEN)) {
            ty = parse_type_name(ctx);
            if (!ty) {
                ty = parse_expression(ctx).ty;
            }
            expect_punc(ctx, PAREN_CLOSE);
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
    if (check_punc(ctx, PAREN_OPEN)) {
        const type *ty = parse_type_name(ctx);
        if (ty) {
        }
        expect_punc(ctx, PAREN_CLOSE);
        return (value){/* handle cast to type */};
    }
    return parse_unary_expression(ctx);
}

value parse_multiplicative_expression(context *ctx) {
    value left = parse_cast_expression(ctx);

    while (true) {
        if (check_punc(ctx, STAR)) {
            value right = parse_cast_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (check_punc(ctx, DIV)) {
            value right = parse_cast_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (check_punc(ctx, REM)) {
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
        if (check_punc(ctx, PLUS)) {
            value right = parse_multiplicative_expression(ctx);

            force_into_ireg(&ctx->regs, &ctx->macho.code, &left);
            force_into_ireg(&ctx->regs, &ctx->macho.code, &right);
            ireg output = get_new_ireg(&ctx->regs, &ctx->macho.code);

            raw_add(&ctx->macho.code, false, left.ireg, right.ireg, output, lsl,
                    0);

            left = (value){
                .ty = left.ty,
                .ireg = output,
                .is_constant = left.is_constant && right.is_constant,
            };
        } else if (check_punc(ctx, MINUS)) {
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
        if (check_punc(ctx, LEFT_SHIFT)) {
            value right = parse_additive_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (check_punc(ctx, RIGHT_SHIFT)) {
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
        if (check_punc(ctx, LESS_THAN)) {
            value right = parse_shift_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (check_punc(ctx, GREATER_THAN)) {
            value right = parse_shift_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (check_punc(ctx, LESS_EQUAL)) {
            value right = parse_shift_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (check_punc(ctx, GREATER_EQUAL)) {
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
        if (check_punc(ctx, EQUAL)) {
            value right = parse_relational_expression(ctx);
            // do something with left and right
            left = (value){/* combine left and right */};
        } else if (check_punc(ctx, NOT_EQUAL)) {
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

    while (check_punc(ctx, AND)) {
        value right = parse_equality_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_exclusive_or_expression(context *ctx) {
    value left = parse_and_expression(ctx);

    while (check_punc(ctx, XOR)) {
        value right = parse_and_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_inclusive_or_expression(context *ctx) {
    value left = parse_exclusive_or_expression(ctx);

    while (check_punc(ctx, OR)) {
        value right = parse_exclusive_or_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_logical_and_expression(context *ctx) {
    value left = parse_inclusive_or_expression(ctx);

    while (check_punc(ctx, AND_AND)) {
        value right = parse_inclusive_or_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_logical_or_expression(context *ctx) {
    value left = parse_logical_and_expression(ctx);

    while (check_punc(ctx, OR_OR)) {
        value right = parse_logical_and_expression(ctx);
        // do something with left and right
        left = (value){/* combine left and right */};
    }

    return left;
}

value parse_conditional_expression(context *ctx) {
    value left = parse_logical_or_expression(ctx);

    if (check_punc(ctx, TERNARY_IF)) {
        value true_branch = parse_expression(ctx);
        expect_punc(ctx, COLON);
        value false_branch = parse_conditional_expression(ctx);

        // todo: handle conditional expression
        return (value){};
    }

    return left;
}

value parse_assignment_expression(context *ctx) {
    value left = parse_conditional_expression(ctx);

    if (check_punc(ctx, ASSIGN)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_ADD)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_SUB)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_SUB)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_MUL)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_DIV)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_REM)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_AND)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_XOR)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_OR)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_LEFT_SHIFT)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    } else if (check_punc(ctx, ASSIGN_RIGHT_SHIFT)) {
        value right = parse_assignment_expression(ctx);
        // todo: handle assignment operator
        return (value){/* handle assignment */};
    }

    return left;
}

value parse_expression(context *ctx) {
    value v;
    do {
        v = parse_assignment_expression(ctx);
    } while (check_punc(ctx, COMMA));
    return v;
}

uint64_t parse_constant_expression(context *ctx) {
    value v = parse_expression(ctx);
    if (!v.is_constant || v.loc != cons) {
        // expected constant expression
        longjmp(ctx->error_jump, 1);
    }
    return v.integer;
}

bool parse_storage_class_specifier(context *ctx,
                                   storage_class_specifier *spec) {
    const keyword storage_classes[] = {
        typedef_kw, extern_kw,   static_kw, /* _Thread_local, */
        auto_kw,    register_kw,
    };

    for (size_t i = 0; i < array_length(storage_classes); i++) {
        if (peek(ctx).type == TOKEN_KEYWORD &&
            peek(ctx).kw == storage_classes[i]) {
            next(ctx);
            *spec = i;
            return true;
        }
    }
    return false;
}

bool parse_type_specifier(context *ctx, const type **ty) {
    struct primitive_type_specifier {
        primitive_type type;
        keyword name;
    };

    const struct primitive_type_specifier primitive_type_specifiers[] = {
        {void_, void_kw},     {char_, char_kw}, {short_, short_kw},
        {int_, int_kw},       {long_, long_kw}, {float_, float_kw},
        {double_, double_kw},
    };

    for (size_t i = 0; i < array_length(primitive_type_specifiers); i++) {
        if (peek(ctx).type == TOKEN_KEYWORD &&
            peek(ctx).kw == primitive_type_specifiers[i].name) {
            next(ctx);
            *ty = &vector_at(type, &ctx->types,
                             primitive_type_specifiers[i].type);
            return true;
        }
    }

    return false;
}

bool parse_type_qualifier(context *ctx, type_qualifier *mask) {
    const keyword type_qualifiers[] = {
        const_kw, volatile_kw, restrict_kw,
        // _Atomic_kw,
    };

    for (size_t i = 0; i < array_length(type_qualifiers); i++) {
        if (peek(ctx).type == TOKEN_KEYWORD &&
            peek(ctx).kw == type_qualifiers[i]) {
            next(ctx);
            *mask |= (1 << i);
            return true;
        }
    }
    return false;
}

bool parse_function_specifier(context *ctx, function_specifier *mask) {
    const keyword function_specifiers[] = {
        inline_kw,
        _Noreturn_kw,
    };

    for (size_t i = 0; i < array_length(function_specifiers); i++) {
        if (peek(ctx).type == TOKEN_KEYWORD &&
            peek(ctx).kw == function_specifiers[i]) {
            next(ctx);
            *mask |= (1 << i);
            return true;
        }
    }
    return false;
}

bool parse_alignment_specifier(context *ctx, size_t *align) {
    if (peek(ctx).type == TOKEN_KEYWORD && peek(ctx).kw == _Alignas_kw) {
        next(ctx);
        expect_punc(ctx, PAREN_OPEN);
        *align = parse_constant_expression(ctx);
        expect_punc(ctx, PAREN_CLOSE);
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

void parse_pointer_opt(context *ctx, const type **ty) {
    // assume guarded by check_punc(ctx, STAR)

    type_qualifier mask = 0;
    while (parse_type_qualifier(ctx, &mask)) {
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

    if (check_punc(ctx, STAR)) {
        return parse_pointer_opt(ctx, ty);
    }
}

bool parse_declaration_specifiers(context *ctx, partial_type *partial_ty) {
    bool has_specifiers = false;
    for (;; has_specifiers = true) {
        if (parse_alignment_specifier(ctx, &partial_ty->alignment))
            continue;
        if (parse_function_specifier(ctx, &partial_ty->function_spec_mask))
            continue;
        if (parse_storage_class_specifier(ctx, &partial_ty->storage_class_spec))
            continue;
        if (parse_type_qualifier(ctx, &partial_ty->type_qualifier_spec_mask))
            continue;
        if (parse_type_specifier(ctx, &partial_ty->ty))
            continue;

        break;
    }
    return has_specifiers;
}

declarator parse_declarator(context *ctx, const type *ty);

declarator parse_parameter_declaration(context *ctx) {
    partial_type partial_ty = {0};
    if (!parse_declaration_specifiers(ctx, &partial_ty)) {
        // expected declaration specifiers
        longjmp(ctx->error_jump, 1);
    }
    type *ty = finalize_type(ctx, &partial_ty);

    // todo: support abstract declarators (no identifier)
    return parse_declarator(ctx, ty);
}

owned_span parse_parameter_list(context *ctx) {
    vector params = {0};

    while (!check_punc(ctx, PAREN_CLOSE)) {
        declarator decl = parse_parameter_declaration(ctx);
        if (decl.initializer.ty != NULL) {
            // default argument values are not supported
            longjmp(ctx->error_jump, 1);
        }

        vector_push(declarator, &params, decl);
        if (!soft_check_punc(ctx, PAREN_CLOSE)) {
            expect_punc(ctx, COMMA);
        }
    }

    return owned_span_from_vector(params);
}

declarator parse_direct_declarator(context *ctx, const type *ty) {
    string ident;
    if (check_punc(ctx, PAREN_OPEN)) {
        declarator decl = parse_declarator(ctx, ty);
        expect_punc(ctx, PAREN_CLOSE);
        ident = decl.name;
        ty = decl.ty;
    } else {
        token tok = next(ctx);
        if (tok.type != TOKEN_IDENTIFIER) {
            // expected identifier
            longjmp(ctx->error_jump, 1);
        }

        ident = tok.ident;
    }

    if (check_punc(ctx, BRACKET_OPEN)) {
        uint64_t length = parse_constant_expression(ctx);
        expect_punc(ctx, BRACKET_CLOSE);

        type *array_ty = calloc(1, sizeof(type));
        if (!array_ty) {
            longjmp(ctx->error_jump, 1);
        }

        array_ty->tag = array;
        array_ty->array.element_type = ty;
        array_ty->array.length = length;

        ty = array_ty;
    } else if (check_punc(ctx, PAREN_OPEN)) {
        owned_span params = parse_parameter_list(ctx);

        type *func_ty = calloc(1, sizeof(type));
        if (!func_ty) {
            longjmp(ctx->error_jump, 1);
        }

        func_ty->tag = function;
        func_ty->function.return_type = ty;
        func_ty->function.parameters = params;

        ty = func_ty;
    }

    return (declarator){.name = ident, .ty = ty, .initializer = {0}};
}

declarator parse_declarator(context *ctx, const type *ty) {
    if (check_punc(ctx, STAR))
        parse_pointer_opt(ctx, &ty);
    return parse_direct_declarator(ctx, ty);
}

// returns a span of declarators
bool parse_declaration(context *ctx, owned_span *decls_out) {
    partial_type partial_ty = {0};
    if (!parse_declaration_specifiers(ctx, &partial_ty)) {
        return false;
    }
    type *ty = finalize_type(ctx, &partial_ty);

    vector declarators = {0};

    if (!soft_check_punc(ctx, SEMICOLON)) {
        while (1) {
            vector_push(declarator, &declarators, parse_declarator(ctx, ty));
            if (check_punc(ctx, ASSIGN)) {
                // todo: handle initializer list
                value init_value = parse_assignment_expression(ctx);
                vector_last(declarator, &declarators).initializer = init_value;
            }
            if (!check_punc(ctx, COMMA)) {
                break;
            }
        }
    }

    *decls_out = owned_span_from_vector(declarators);
    return true;
}

void parse_statement(context *ctx);

void parse_labeled_statement(context *ctx) {
    string label_name = next(ctx).ident;
    expect_punc(ctx, COLON);
    parse_statement(ctx);
}

void parse_case_statement(context *ctx) {
    value v = parse_expression(ctx);
    expect_punc(ctx, COLON);
    parse_statement(ctx);
}

void parse_default_case_statement(context *ctx) {
    expect_punc(ctx, COLON);
    parse_statement(ctx);
}

void parse_if_statement(context *ctx) {
    expect_punc(ctx, PAREN_OPEN);
    value condition = parse_expression(ctx);
    expect_punc(ctx, PAREN_CLOSE);
    parse_statement(ctx);
    if (check_keyword(ctx, else_kw)) {
        parse_statement(ctx);
    }
}

void parse_switch_statement(context *ctx) {
    expect_punc(ctx, PAREN_OPEN);
    value condition = parse_expression(ctx);
    expect_punc(ctx, PAREN_CLOSE);
    parse_statement(ctx);
}

void parse_while_statement(context *ctx) {
    expect_punc(ctx, PAREN_OPEN);
    value condition = parse_expression(ctx);
    expect_punc(ctx, PAREN_CLOSE);
    parse_statement(ctx);
}

void parse_do_while_statement(context *ctx) {
    parse_statement(ctx);
    expect_punc(ctx, PAREN_OPEN);
    value condition = parse_expression(ctx);
    expect_punc(ctx, PAREN_CLOSE);
    expect_keyword(ctx, while_kw);
    expect_punc(ctx, SEMICOLON);
}

void parse_for_statement(context *ctx) {
    expect_punc(ctx, PAREN_OPEN);
    owned_span declarations;
    if (!parse_declaration(ctx, &declarations)) {
        parse_expression(ctx);
    }
    expect_punc(ctx, SEMICOLON);
    if (!check_punc(ctx, SEMICOLON)) {
        value condition = parse_expression(ctx);
        expect_punc(ctx, SEMICOLON);
    }
    if (!check_punc(ctx, PAREN_CLOSE)) {
        value increment = parse_expression(ctx);
        expect_punc(ctx, PAREN_CLOSE);
    }
    parse_statement(ctx);
}

void parse_goto_statement(context *ctx) {
    string label_name = next(ctx).ident;
    expect_punc(ctx, SEMICOLON);
}

void parse_continue_statement(context *ctx) { expect_punc(ctx, SEMICOLON); }

void parse_break_statement(context *ctx) { expect_punc(ctx, SEMICOLON); }

void move_to_return_convention(context *ctx, value return_value) {
    // todo: properly conform to ABI
    force_into_specific_ireg(&ctx->regs, &ctx->macho.code, &return_value, x0);
}

void parse_return_statement(context *ctx) {
    if (!check_punc(ctx, SEMICOLON)) {
        value return_value = parse_expression(ctx);
        expect_punc(ctx, SEMICOLON);

        // check type

        move_to_return_convention(ctx, return_value);
    }

    raw_restoring_return(&ctx->macho.code);
}

void parse_compound_statement(context *ctx) {
    // assume guarded by check_punc(ctx, CURLY_OPEN)

    while (!check_punc(ctx, CURLY_CLOSE)) {
        owned_span declarations;
        if (!parse_declaration(ctx, &declarations)) {
            parse_statement(ctx);
        } else {
            expect_punc(ctx, SEMICOLON);
        }
    }
}

void parse_statement(context *ctx) {
    token tok1 = peek(ctx), tok2 = peek2(ctx);
    if (tok1.type == TOKEN_IDENTIFIER && tok2.type == TOKEN_PUNCTUATOR &&
        tok2.punc == COLON) {
        parse_labeled_statement(ctx);
    } else if (check_keyword(ctx, case_kw)) {
        parse_case_statement(ctx);
    } else if (check_keyword(ctx, default_kw)) {
        parse_default_case_statement(ctx);
    } else if (check_punc(ctx, CURLY_OPEN)) {
        parse_compound_statement(ctx);
    } else if (check_keyword(ctx, if_kw)) {
        parse_if_statement(ctx);
    } else if (check_keyword(ctx, switch_kw)) {
        parse_switch_statement(ctx);
    } else if (check_keyword(ctx, while_kw)) {
        parse_while_statement(ctx);
    } else if (check_keyword(ctx, do_kw)) {
        parse_do_while_statement(ctx);
    } else if (check_keyword(ctx, for_kw)) {
        parse_for_statement(ctx);
    } else if (check_keyword(ctx, goto_kw)) {
        parse_goto_statement(ctx);
    } else if (check_keyword(ctx, continue_kw)) {
        parse_continue_statement(ctx);
    } else if (check_keyword(ctx, break_kw)) {
        parse_break_statement(ctx);
    } else if (check_keyword(ctx, return_kw)) {
        parse_return_statement(ctx);
    } else if (check_punc(ctx, SEMICOLON)) {
        // empty statement
    } else {
        parse_expression(ctx);
        expect_punc(ctx, SEMICOLON);
    }
}

void add_function_definition(context *ctx, const declarator *decl) {
    vector_push(function_defn, &ctx->functions,
                ((function_defn){
                    .name = decl->name,
                    .ty = decl->ty,
                    .is_defined = true,
                }));

    vector_push(char, &ctx->macho.symbol_names, '\0');

    size_t symbol_start = ctx->macho.symbol_names.size;
    vector_push(char, &ctx->macho.symbol_names, '_');
    for (size_t i = 0; i < decl->name.length; i++) {
        vector_push(char, &ctx->macho.symbol_names, decl->name.data[i]);
    }

    vector_push(struct nlist_64, &ctx->macho.symbols,
                ((struct nlist_64){.n_un.n_strx = symbol_start,
                                   .n_type = N_SECT | N_EXT,
                                   .n_sect = 1,
                                   .n_desc = 0,
                                   .n_value = ctx->macho.code.size *
                                              sizeof(uint32_t)}));
}

void parse_external_declaration(context *ctx) {
    owned_span declarations;
    if (!parse_declaration(ctx, &declarations)) {
        longjmp(ctx->error_jump, 1);
    }

    const declarator *first = &vector_at(declarator, &declarations, 0);
    if (declarations.size == 1 && first->ty->tag == function) {
        // function definition
        if (check_punc(ctx, CURLY_OPEN)) {
            add_function_definition(ctx, first);

            // function prelude
            raw_stp(&ctx->macho.code, true, offset, 0, x30, sp, x29);
            raw_add_cons(&ctx->macho.code, true, 0, sp, x29, false);
            // todo: handle stack size
            raw_sub_cons(&ctx->macho.code, true, 0, sp, sp, false);
            raw_sub_cons(&ctx->macho.code, true, 0, sp, sp, true);

            for (size_t i = 0; i < first->ty->function.parameters.size; i++) {
                const declarator *param =
                    &vector_at(declarator, &first->ty->function.parameters, i);

                vector_push(variable, &ctx->variables,
                            ((variable){
                                .name = param->name,
                                .ty = param->ty,
                                .val.ty = param->ty,
                                .val.ireg = (ireg)i,
                            }));
            }

            // parse body
            parse_compound_statement(ctx);

            // postlude
            if (first->ty->function.return_type->id == void_) {
                // if it's a void function, there's an implicit return
                raw_restoring_return(&ctx->macho.code);
            } else if (string_equal(first->name, string_literal("main"))) {
                // if it's the main function, there's an implicit return 0
                const type *intty = &vector_at(type, &ctx->types, int_);
                move_to_return_convention(ctx,
                                          (value){.ty = intty, .integer = 0});
                raw_restoring_return(&ctx->macho.code);
            }
        } else {
            expect_punc(ctx, SEMICOLON);

            // function declaration
            vector_push(function_defn, &ctx->functions,
                        ((function_defn){
                            .name = first->name,
                            .ty = first->ty,
                            .is_defined = false,
                        }));
        }
    } else {
        expect_punc(ctx, SEMICOLON);

        for (size_t i = 0; i < declarations.size; i++) {
            declarator decl = vector_at(declarator, &declarations, i);
            vector_push(global, &ctx->globals,
                        ((global){
                            .name = decl.name,
                            .ty = decl.ty,
                        }));
        }
    }
}

void objectify(macho_builder *macho, FILE *output_file) {
    vector_push(char, &macho->symbol_names, '\0');

    size_t segment_start = sizeof(struct mach_header_64) +
                           sizeof(struct segment_command_64) +
                           3 * sizeof(struct section_64) +
                           sizeof(struct symtab_command),
           code_start = segment_start,
           data_start = code_start + macho->code.size * sizeof(uint32_t),
           string_start = data_start + macho->data.size * sizeof(uint8_t),
           relocs_start = string_start + macho->strings.size * sizeof(char),
           symbols_start = relocs_start +
                           macho->relocs.size * sizeof(struct relocation_info),
           symbol_names_start =
               symbols_start + macho->symbols.size * sizeof(struct nlist_64);

    struct mach_header_64 header = {
        .magic = MH_MAGIC_64,
        .cputype = CPU_TYPE_ARM64,
        .cpusubtype = CPU_SUBTYPE_ARM64_ALL,
        .filetype = MH_OBJECT,
        .ncmds = 2,
        .sizeofcmds = sizeof(struct segment_command_64) +
                      3 * sizeof(struct section_64) +
                      sizeof(struct symtab_command),
        .flags = MH_SUBSECTIONS_VIA_SYMBOLS,
    };

    int nsects = 3; // code section, data section, string section
    struct segment_command_64 segment = {
        .cmd = LC_SEGMENT_64,
        .cmdsize = sizeof(struct segment_command_64) +
                   nsects * sizeof(struct section_64),
        .segname = "__TEXT",
        .vmaddr = 0,
        .vmsize = 0,
        .fileoff = segment_start,
        .filesize = macho->code.size * sizeof(uint32_t) +
                    macho->data.size * sizeof(uint8_t) +
                    macho->strings.size * sizeof(char),
        .maxprot = VM_PROT_READ | VM_PROT_EXECUTE,
        .initprot = VM_PROT_READ | VM_PROT_EXECUTE,
        .nsects = nsects,
        .flags = 0,
    };

    struct section_64 code_section = {
        .sectname = "__text",
        .segname = "__TEXT",
        .addr = 0,
        .size = macho->code.size * sizeof(uint32_t),
        .offset = code_start,
        // instructions are 4 bytes
        .align = sizeof(uint32_t),
        .reloff = relocs_start,
        .nreloc = macho->relocs.size,
        .flags =
            S_REGULAR | S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS,
    };

    struct section_64 data_section = {
        .sectname = "__data",
        .segname = "__DATA",
        .addr = code_section.addr + code_section.size,
        .size = macho->data.size * sizeof(uint8_t),
        .offset = data_start,
        .align = sizeof(uint64_t),
        .reloff = 0,
        .nreloc = 0,
        .flags = S_REGULAR,
    };

    struct section_64 string_section = {
        .sectname = "__cstring",
        .segname = "__TEXT",
        .addr = data_section.addr + data_section.size,
        .size = macho->strings.size * sizeof(char),
        .offset = string_start,
        // does this impact anything? maybe wide strings?
        .align = 0,
        .reloff = 0,
        .nreloc = 0,
        .flags = S_CSTRING_LITERALS,
    };

    struct symtab_command symbol_table_cmd = {
        .cmd = LC_SYMTAB,
        .cmdsize = sizeof(struct symtab_command),
        .symoff = symbols_start,
        .nsyms = macho->symbols.size,
        .stroff = symbol_names_start,
        .strsize = macho->symbol_names.size * sizeof(char)};

    fwrite(&header, sizeof(header), 1, output_file);
    fwrite(&segment, sizeof(segment), 1, output_file);
    fwrite(&code_section, sizeof(code_section), 1, output_file);
    fwrite(&data_section, sizeof(data_section), 1, output_file);
    fwrite(&string_section, sizeof(string_section), 1, output_file);
    fwrite(&symbol_table_cmd, sizeof(symbol_table_cmd), 1, output_file);

    fwrite(macho->code.data, sizeof(uint32_t), macho->code.size, output_file);
    fwrite(macho->data.data, sizeof(uint8_t), macho->data.size, output_file);
    fwrite(macho->strings.data, sizeof(char), macho->strings.size, output_file);
    fwrite(macho->relocs.data, sizeof(struct relocation_info),
           macho->relocs.size, output_file);
    fwrite(macho->symbols.data, sizeof(struct nlist_64), macho->symbols.size,
           output_file);
    fwrite(macho->symbol_names.data, sizeof(char), macho->symbol_names.size,
           output_file);
}

bool compile(const char *filepath, FILE *output_file) {
    context ctx;

    if (setjmp(ctx.error_jump)) {
        // todo: actually communicate information
        return false;
    } else {
        init_context(&ctx, filepath);

        while (peek(&ctx).type != TOKEN_EOF) {
            parse_external_declaration(&ctx);
        }

        objectify(&ctx.macho, output_file);

        return true;
    }
}
