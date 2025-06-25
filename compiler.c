#include "./compiler.h"
#include <ctype.h>
#include <stdio.h>

#define array_length(arr) (sizeof(arr) / sizeof((arr)[0]))

void init_context(context *ctx, span source) {
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

char peek(context *ctx) {
    if (ctx->i >= ctx->source.size) {
        // unexpected end of input
        longjmp(ctx->error_jump, 1);
    }
    return ctx->source.data[ctx->i];
}

char next(context *ctx) {
    char c = peek(ctx);
    ctx->i++;
    return c;
}

void skip_whitespace(context *ctx) {
    while (isspace(peek(ctx))) {
        next(ctx);
    }
}

string get_identifier(context *ctx) {
    if (!isalpha(peek(ctx))) {
        // expected identifier
        longjmp(ctx->error_jump, 1);
    }

    size_t start = ctx->i;
    while (isalnum(peek(ctx))) {
        next(ctx);
    }

    return (string){ctx->source.data + start, ctx->i - start};
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
        value = value * 10 + (next(ctx) - '0');
    }
    return value;
}

bool parse_declaration_specifiers(context *ctx, string ident,
                                  declaration *decl) {
    const string storage_classes[] = {
        string_literal("typedef"), string_literal("extern"),
        string_literal("static"), /* string_literal("_Thread_local"), */
        string_literal("auto"),    string_literal("register"),
    };

    for (int i = 0; i < array_length(storage_classes); i++) {
        if (string_equal(ident, storage_classes[i])) {
            decl->storage_class_spec = i;
            return true;
        }
    }
    return false;
}

bool parse_type_specifier(context *ctx, string ident, declaration *decl) {
    struct primitive_type_specifier {
        primitive_type type;
        string name;
    };

    const struct primitive_type_specifier primitive_type_specifiers[] = {
        {void_, string_literal("void")},
        {char_, string_literal("char")},
        {short_, string_literal("short")},
        {int_, string_literal("int")},
        {long_, string_literal("long")},
        {float_, string_literal("float")},
        {double_, string_literal("double")},
        {signed_char, string_literal("signed")},
        {unsigned_char, string_literal("unsigned")},
        {bool_, string_literal("_Bool")},
    };

    for (int i = 0; i < array_length(primitive_type_specifiers); i++) {
        if (string_equal(ident, primitive_type_specifiers[i].name)) {
            decl->ty =
                vector_at(type, &ctx->types, primitive_type_specifiers[i].type);
            return true;
        }
    }

    return false;
}

bool parse_type_qualifier(context *ctx, string ident, declaration *decl) {
    const string type_qualifiers[] = {
        string_literal("const"), string_literal("volatile"),
        string_literal("restrict"),
        // string_literal("_Atomic"),
    };

    for (int i = 0; i < array_length(type_qualifiers); i++) {
        if (string_equal(ident, type_qualifiers[i])) {
            decl->type_qualifier_spec_mask |= (1 << i);
            return true;
        }
    }
    return false;
}

bool parse_function_specifier(context *ctx, string ident, declaration *decl) {
    const string function_specifiers[] = {
        string_literal("inline"),
        string_literal("_Noreturn"),
    };

    for (int i = 0; i < array_length(function_specifiers); i++) {
        if (string_equal(ident, function_specifiers[i])) {
            decl->function_spec_mask |= (1 << i);
            return true;
        }
    }
    return false;
}

bool parse_alignment_specifier(context *ctx, string ident, declaration *decl) {
    const string alignment_specifier = string_literal("_Alignas");

    if (string_equal(ident, alignment_specifier)) {
        skip_whitespace(ctx);
        expect(ctx, '(');
        skip_whitespace(ctx);
        decl->alignment = execute_constant_expression(ctx);
        skip_whitespace(ctx);
        expect(ctx, ')');
        return true;
    }

    return false;
}

void parse_declarator(context *ctx) {

}

void parse_declaration(context *ctx) {
    declaration decl = {0};

    string ident;
    do {
        skip_whitespace(ctx);
        ident = get_identifier(ctx);
        skip_whitespace(ctx);

        if (parse_alignment_specifier(ctx, ident, &decl))
            continue;
        if (parse_function_specifier(ctx, ident, &decl))
            continue;
        if (parse_declaration_specifiers(ctx, ident, &decl))
            continue;
        if (parse_type_qualifier(ctx, ident, &decl))
            continue;
        if (parse_type_specifier(ctx, ident, &decl))
            continue;
    } while (0);

    
}

void parse_external_declaration(context *ctx) {
    parse_declaration(ctx);
}

vector compile(span source) {
    context ctx;
    init_context(&ctx, source);

    if (setjmp(ctx.error_jump)) {
        // todo: actually communicate information
        ctx.result.size = -1;
    } else {
        skip_whitespace(&ctx);

        while (ctx.i < ctx.source.size) {
            parse_external_declaration(&ctx);
            skip_whitespace(&ctx);
        }
    }

    return ctx.result;
}
