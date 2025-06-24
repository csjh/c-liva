#include "./compiler.h"
#include <ctype.h>

#define array_length(arr) (sizeof(arr) / sizeof((arr)[0]))

void init_context(context *ctx, span source) {
    ctx->source = source;
    ctx->i = 0;
    ctx->result = (vector){0};
}

void skip_whitespace(context *ctx) {
    while (ctx->i < ctx->source.size && isspace(ctx->source.data[ctx->i])) {
        ctx->i++;
    }
}

string get_identifier(context *ctx) {
    if (ctx->i >= ctx->source.size || !isalpha(ctx->source.data[ctx->i])) {
        // expected identifier
        longjmp(ctx->error_jump, 1);
    }

    size_t start = ctx->i;
    while (ctx->i < ctx->source.size && isalnum(ctx->source.data[ctx->i])) {
        ctx->i++;
    }

    return (string){ctx->source.data + start, ctx->i - start};
}

void expect(context *ctx, char c) {
    if (ctx->i >= ctx->source.size || ctx->source.data[ctx->i] != c) {
        // expected character
        longjmp(ctx->error_jump, 1);
    }
    ctx->i++;
}

uint64_t execute_constant_expression(context *ctx) {
    if (!isdigit(ctx->source.data[ctx->i])) {
        // expected constant expression
        longjmp(ctx->error_jump, 1);
    }

    uint64_t value = 0;
    while (ctx->i < ctx->source.size && isdigit(ctx->source.data[ctx->i])) {
        value = value * 10 + (ctx->source.data[ctx->i] - '0');
        ctx->i++;
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
    const string primitive_type_specifiers[] = {
        string_literal("void"),     string_literal("char"),
        string_literal("short"),    string_literal("int"),
        string_literal("long"),     string_literal("float"),
        string_literal("double"),   string_literal("signed"),
        string_literal("unsigned"), string_literal("_Bool")};

    for (int i = 0; i < array_length(primitive_type_specifiers); i++) {
        if (string_equal(ident, primitive_type_specifiers[i])) {
            // handle
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

    int long unsigned x = 0;

    int volatile unsigned long const _Atomic long typedef y;

    int typedef z;

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

void parse_declaration(context *ctx) {}

type *parse_type(context *ctx, string *maybe_identifier) {
    // types can span over an identifier, i.e. int arr[5];
}

void parse_top_level_declaration(context *ctx);

vector compile(span source) {
    context ctx;
    init_context(&ctx, source);

    if (setjmp(ctx.error_jump)) {
        // todo: actually communicate information
        ctx.result.size = -1;
    } else {
        skip_whitespace(&ctx);

        while (ctx.i < ctx.source.size) {
            parse_top_level_declaration(&ctx);
            skip_whitespace(&ctx);
        }
    }

    return ctx.result;
}
