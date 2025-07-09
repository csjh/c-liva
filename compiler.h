#include "./containers.h"
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

bool compile(const char *filepath, FILE *output_file);

typedef enum primitive {
    void_,
    char_,
    signed_char,
    short_,
    int_,
    long_,
    long_long,
    bool_,
    unsigned_char,
    unsigned_short,
    unsigned_int,
    unsigned_long,
    unsigned_long_long,
    float_,
    double_,
    long_double,
    // todo: complex, imaginary

    n_primitive_types
} primitive_type;

typedef enum type_classification {
    basic,
    array,
    structure, // struct or union
    function,
    pointer,
    // todo: atomic
} type_classification;

typedef struct array_type {
    const struct type *element_type;
    size_t length;
} array_type;

typedef struct member {
    string name;
    const struct type *ty;
    size_t offset;
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

    bool const_;
    bool volatile_;

    type_classification tag;
    union {
        primitive_type primitive;
        array_type array;
        structure_type structure;
        function_type function;
        pointer_type pointer;
    };
} type;

enum location { reg, stack, imm, flags };
enum value_category { lvalue, rvalue, function_designator };

typedef struct value {
    enum value_category category;
    const type *ty;
    bool is_constant;

    enum location loc;
    union {
        uint32_t u32;
        uint64_t u64;
        int32_t i32;
        int64_t i64;
        float f32;
        double f64;
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
    uint32_t offset;
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

typedef struct partial_type {
    storage_class_specifier storage_class_spec;
    type_qualifier type_qualifier_spec_mask;
    function_specifier function_spec_mask;
    size_t alignment;

    const type *ty;
} partial_type;

typedef struct declarator {
    string name;
    const type *ty;
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

typedef struct context {
    const char *filedir;
    const char *filename;

    vector sysdirs;
    vector quotedirs;

    jmp_buf error_jump;

    source_entry *entry;
    token tokens[2];

    vector types;
    vector structs;
    vector unions;
    vector enums;
    vector typedefs;

    vector macros;

    vector globals;
    vector functions;

    vector enum_values;

    macho_builder macho;
} context;
