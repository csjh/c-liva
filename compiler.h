#pragma once

#include "./containers.h"
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

bool compile(const char *filepath, FILE *output_file);

typedef enum primitive {
    void_,
    bool_,
    char_,
    signed_char,
    unsigned_char,
    short_,
    unsigned_short,
    int_,
    unsigned_int,
    long_,
    unsigned_long,
    long_long,
    unsigned_long_long,
    float_,
    double_,
    long_double,
    // todo: complex, imaginary

    n_primitive_types
} primitive_type;

static inline bool is_signed(primitive_type type) {
    switch (type) {
    case char_:
    case signed_char:
    case short_:
    case int_:
    case long_:
    case long_long:
        return true;
    default:
        return false;
    }
}

static inline bool to_unsigned(primitive_type type) {
    switch (type) {
    case char_:
        return unsigned_char;
    case signed_char:
        return unsigned_char;
    case short_:
        return unsigned_short;
    case int_:
        return unsigned_int;
    case long_:
        return unsigned_long;
    case long_long:
        return unsigned_long_long;
    default:
        return type;
    }
}

typedef enum type_classification {
    basic,
    enumerated,
    array,
    structure, // struct or union
    function,
    pointer,
    // todo: atomic
} type_classification;

typedef struct enumerated_type {
    string name;
} enumerated_type;

typedef struct array_type {
    const struct type *element_type;
    size_t length;
} array_type;

typedef struct member {
    string name;
    const struct type *ty;
    size_t offset;
    uint8_t bitwidth; // 0 if not a bitfield
} member;

typedef struct structure_type {
    string name;
    owned_span members;
} structure_type;

typedef struct function_type {
    const struct type *return_type;
    owned_span parameters;
} function_type;

typedef struct pointer_type {
    const struct type *ty;
    bool restrict_;
} pointer_type;

// possible optimization down the line:
// make any type pointer into an index into a type table
typedef struct type {
    uint32_t id;
    uint32_t size;
    // todo: this is very poorly handled rn
    uint32_t alignment;

    bool const_;
    bool volatile_;

    type_classification tag;
    union {
        primitive_type primitive;
        enumerated_type enumerated;
        array_type array;
        structure_type structure;
        function_type function;
        pointer_type pointer;
    };
} type;

typedef enum location { reg, stack, cons, flags } location;
typedef enum value_category {
    rvalue,
    lvalue,
    function_designator
} value_category;

// clang-format off
typedef enum ireg {
    x0,  x1,  x2,  x3,  x4,  x5,  x6,  x7,  x8,  x9, x10, x11, x12, x13, x14, x15,
    x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, xzr, sp = xzr
} ireg;

typedef enum freg {
    d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7,  d8,  d9,  d10, d11, d12, d13, d14, d15,
    d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31,
} freg;

typedef enum cond {
    eq = 0b0000, // Equal.
    ne = 0b0001, // Not equal.
    cs = 0b0010, // Unsigned higher or same (or carry set).
    cc = 0b0011, // Unsigned lower (or carry clear).
    mi = 0b0100, // Negative.
    pl = 0b0101, // Positive or zero.
    vs = 0b0110, // Signed overflow.
    vc = 0b0111, // No signed overflow.
    hi = 0b1000, // Unsigned higher.
    ls = 0b1001, // Unsigned lower or same.
    ge = 0b1010, // Signed greater than or equal.
    lt = 0b1011, // Signed less than.
    gt = 0b1100, // Signed greater than.
    le = 0b1101, // Signed less than or equal.
    al = 0b1110, // Always executed.
    nv = 0b1111, // Never executed.
} cond;
// clang-format on

typedef struct value {
    value_category category;
    const type *ty;
    bool is_constant;
    uint32_t alignment;

    location loc;
    union {
        ireg ireg;
        freg freg;
        uint32_t offset;
        cond flags;

        // for constant expressions
        int64_t integer;
        double fp;
        void *data;
    };
} value;

typedef struct variable {
    string name;
    const type *ty;
    value val;
} variable;

typedef struct global {
    string name;
    const type *ty;
} global;

typedef enum storage_class_specifier {
    typedef_,
    extern_,
    static_,
    // thread_local,
    auto_,
    register_,
} storage_class_specifier;

typedef enum type_qualifier {
    none = 0,
    const_ = 1,
    volatile_ = 2,
    restrict_ = 4,
} type_qualifier;

typedef enum function_specifier {
    /* none = 0 */
    inline_ = 1,
    noreturn = 2,
} function_specifier;

typedef struct type_specifier {
    const type *ty;

    bool basic_is_set;
    primitive_type basic_ty;
    bool is_long;
    bool is_longlong;
    bool is_short;
    bool is_signed;
    bool is_unsigned;
} type_specifier;

typedef struct partial_type {
    storage_class_specifier storage_class_spec;
    type_qualifier type_qualifier_spec_mask;
    function_specifier function_spec_mask;
    size_t alignment;
    type_specifier specifier;
} partial_type;

typedef struct declarator {
    string name;
    const type *ty;
    value initializer;
} declarator;

typedef struct alias {
    string name;
    const type *ty;
} alias;

typedef struct macro {
    string name;
    size_t n_parameters;
    // these compose a linked list of the macro body
    /*
    for something like #define MACRO(a, b) (a) + (b) * (a##b)
    "(" -> parameters[0] -> ") + (" -> parameters[1] -> ") * (" -> parameters[0]
    -> parameters[1] -> ")"
    */
    source_entry *chunks_head;
    source_entry *chunks_tail;
    owned_span parameters;
} macro;

typedef struct macho_builder {
    size_t n_strings;
    vector strings;
    vector data;
    vector code;
    vector relocs;
    vector symbols;
    vector symbol_names;
} macho_builder;

static const string keyword_names[] = {
    string_literal("auto"),           string_literal("if"),
    string_literal("unsigned"),       string_literal("break"),
    string_literal("inline"),         string_literal("void"),
    string_literal("case"),           string_literal("int"),
    string_literal("volatile"),       string_literal("char"),
    string_literal("long"),           string_literal("while"),
    string_literal("const"),          string_literal("register"),
    string_literal("_Alignas"),       string_literal("continue"),
    string_literal("restrict"),       string_literal("_Alignof"),
    string_literal("default"),        string_literal("return"),
    string_literal("_Atomic"),        string_literal("do"),
    string_literal("short"),          string_literal("_Bool"),
    string_literal("double"),         string_literal("signed"),
    string_literal("_Complex"),       string_literal("else"),
    string_literal("sizeof"),         string_literal("_Generic"),
    string_literal("enum"),           string_literal("static"),
    string_literal("_Imaginary"),     string_literal("extern"),
    string_literal("struct"),         string_literal("_Noreturn"),
    string_literal("float"),          string_literal("switch"),
    string_literal("_Static_assert"), string_literal("for"),
    string_literal("typedef"),        string_literal("_Thread_local"),
    string_literal("goto"),           string_literal("union")};

typedef enum keyword {
    auto_kw,
    if_kw,
    unsigned_kw,
    break_kw,
    inline_kw,
    void_kw,
    case_kw,
    int_kw,
    volatile_kw,
    char_kw,
    long_kw,
    while_kw,
    const_kw,
    register_kw,
    _Alignas_kw,
    continue_kw,
    restrict_kw,
    _Alignof_kw,
    default_kw,
    return_kw,
    _Atomic_kw,
    do_kw,
    short_kw,
    _Bool_kw,
    double_kw,
    signed_kw,
    _Complex_kw,
    else_kw,
    sizeof_kw,
    _Generic_kw,
    enum_kw,
    static_kw,
    _Imaginary_kw,
    extern_kw,
    struct_kw,
    _Noreturn_kw,
    float_kw,
    switch_kw,
    _Static_assert_kw,
    for_kw,
    typedef_kw,
    _Thread_local_kw,
    goto_kw,
    union_kw,
} keyword;

typedef struct number_literal {
    bool is_integer;
    union {
        uint64_t integer;
        double fp;
    };
} number_literal;

typedef enum width {
    regular,
    utf8,
    utf16,
    utf32,
    wchar,
} width;

typedef struct string_literal {
    string str;
    width w;
} string_literal;

static const string punctuator_names[] = {
    invalid_str,           string_literal("["),   string_literal("]"),
    string_literal("("),   string_literal(")"),   string_literal("{"),
    string_literal("}"),   string_literal("."),   string_literal("->"),
    string_literal("++"),  string_literal("--"),  string_literal("&"),
    string_literal("*"),   string_literal("+"),   string_literal("-"),
    string_literal("!"),   string_literal("~"),   string_literal("/"),
    string_literal("%"),   string_literal("<<"),  string_literal(">>"),
    string_literal("<"),   string_literal(">"),   string_literal("<="),
    string_literal(">="),  string_literal("=="),  string_literal("!="),
    string_literal("^"),   string_literal("|"),   string_literal("&&"),
    string_literal("||"),  string_literal("?"),   string_literal(":"),
    string_literal(";"),   string_literal(".."),  string_literal("..."),
    string_literal("="),   string_literal("*="),  string_literal("/="),
    string_literal("%="),  string_literal("+="),  string_literal("-="),
    string_literal("<<="), string_literal(">>="), string_literal("&="),
    string_literal("^="),  string_literal("|="),  string_literal(","),
    string_literal("#"),   string_literal("##"),
};

typedef enum punctuator {
    INVALID,
    BRACKET_OPEN,
    BRACKET_CLOSE,
    PAREN_OPEN,
    PAREN_CLOSE,
    CURLY_OPEN,
    CURLY_CLOSE,
    DOT,
    ARROW,
    INCREMENT,
    DECREMENT,
    AND,
    STAR,
    PLUS,
    MINUS,
    NOT,
    LOGICAL_NOT,
    DIV,
    REM,
    LEFT_SHIFT,
    RIGHT_SHIFT,
    LESS_THAN,
    GREATER_THAN,
    LESS_EQUAL,
    GREATER_EQUAL,
    EQUAL,
    NOT_EQUAL,
    XOR,
    OR,
    AND_AND,
    OR_OR,
    TERNARY_IF,
    COLON,
    SEMICOLON,
    PARTIAL_ELLIPSIS,
    ELLIPSIS,
    ASSIGN,
    ASSIGN_MUL,
    ASSIGN_DIV,
    ASSIGN_REM,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_LEFT_SHIFT,
    ASSIGN_RIGHT_SHIFT,
    ASSIGN_AND,
    ASSIGN_XOR,
    ASSIGN_OR,
    COMMA,
    HASH,
    HASH_HASH,
    // support digraphs?
} punctuator;

typedef struct token {
    enum {
        TOKEN_EOF,
        TOKEN_KEYWORD,
        TOKEN_IDENTIFIER,
        TOKEN_NUMBER,
        TOKEN_STRING,
        TOKEN_PUNCTUATOR,
    } type;
    union {
        keyword kw;
        string ident;
        number_literal num;
        string_literal str;
        punctuator punc;
    };
} token;

typedef struct enum_value {
    string name;
    uint64_t value;
} enum_value;

typedef struct regallocator {

} regallocator;

typedef struct function_defn {
    string name;
    const type *ty;
    bool is_defined;
} function_defn;

typedef struct context {
    const char *filedir;
    const char *filename;

    vector sysdirs;
    vector quotedirs;

    jmp_buf error_jump;

    source_entry *entry;
    token tokens[2];

    regallocator regs;

    vector types;
    vector structs;
    vector unions;
    vector enums;
    vector typedefs;

    vector macros;

    vector globals;
    vector functions;
    vector variables;

    vector enum_values;

    macho_builder macho;
} context;
