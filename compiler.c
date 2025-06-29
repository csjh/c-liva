#include "./compiler.h"
#include <ctype.h>
#include <stdio.h>

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

void init_context(context *ctx, string source) {
    ctx->source = source;
    ctx->i = 0;
    ctx->result = (vector){0};

    ctx->types = (vector){0};
    for (int i = 0; i < n_primitive_types; i++) {
        type ty = {0};
        ty.id = i;
        ty.tag = basic;
        ty.primitive = i;
        vector_push(type, &ctx->types, ty);
    }
}

bool oob_safe_peek(context *ctx, char *c) {
    if (ctx->i >= ctx->source.length) {
        // unexpected end of input
        return false;
    }
    *c = ctx->source.data[ctx->i];
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

char next_with_whitespace(context *ctx) {
    char c = peek(ctx);
    ctx->i++;
    return c;
}

void skip_whitespace(context *ctx) {
    char c;
    while (oob_safe_peek(ctx, &c) && isspace(c)) {
        next_with_whitespace(ctx);
    }
}

char next(context *ctx) {
    char c = peek(ctx);
    ctx->i++;
    skip_whitespace(ctx);
    return c;
}

string get_identifier(context *ctx) {
    if (!isalpha(peek(ctx))) {
        // expected identifier
        longjmp(ctx->error_jump, 1);
    }

    size_t start = ctx->i;
    while (isalnum(peek(ctx))) {
        next_with_whitespace(ctx);
    }
    size_t end = ctx->i;

    skip_whitespace(ctx);

    return (string){ctx->source.data + start, end - start};
}

void expect(context *ctx, char c) {
    if (next(ctx) != c) {
        // expected character
        longjmp(ctx->error_jump, 1);
    }
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
    skip_whitespace(ctx);

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
    vector_push(declarator, &declarators, parse_declarator(ctx, ty, ident));
    while (peek(ctx) == ',') {
        expect(ctx, ',');
        vector_push(declarator, &declarators,
                    parse_declarator(ctx, ty, invalid_str));
    }

    return owned_span_from_vector(declarators);
}

void parse_external_declaration(context *ctx) {
    owned_span declarations = parse_declaration(ctx);

    if (declarations.size == 1 &&
        vector_at(declarator, &declarations, 0).ty->tag == function &&
        peek(ctx) == '{') {
        expect(ctx, '{');
        // function definition, todo: handle
        expect(ctx, '}');
    } else {
        expect(ctx, ';');
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

owned_span compile(string source) {
    context ctx;
    init_context(&ctx, source);

    if (setjmp(ctx.error_jump)) {
        // todo: actually communicate information
        ctx.result.size = -1;
    } else {
        skip_whitespace(&ctx);

        while (ctx.i < ctx.source.length) {
            parse_external_declaration(&ctx);
        }
    }

    return owned_span_from_vector(ctx.result);
}
