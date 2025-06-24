#include "./string-span.h"
#include "./vector.h"
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct span {
    char *data;
    size_t size;
} span;

vector compile(span source);

const string KEYWORDS[] = {
    string_literal("alignof"),       string_literal("sizeof"),
    string_literal("char"),          string_literal("double"),
    string_literal("enum"),          string_literal("float"),
    string_literal("int"),           string_literal("long"),
    string_literal("short"),         string_literal("struct"),
    string_literal("typedef"),       string_literal("union"),
    string_literal("void"),          string_literal("_Bool"),
    string_literal("_Complex"),      string_literal("_Imaginary"),
    string_literal("auto"),          string_literal("const"),
    string_literal("extern"),        string_literal("inline"),
    string_literal("register"),      string_literal("restrict"),
    string_literal("signed"),        string_literal("static"),
    string_literal("unsigned"),      string_literal("volatile"),
    string_literal("_Alignas"),      string_literal("_Atomic"),
    string_literal("_Thread_local"), string_literal("break"),
    string_literal("case"),          string_literal("continue"),
    string_literal("default"),       string_literal("do"),
    string_literal("else"),          string_literal("for"),
    string_literal("goto"),          string_literal("if"),
    string_literal("return"),        string_literal("switch"),
    string_literal("while"),         string_literal("_Generic"),
    string_literal("_Noreturn"),     string_literal("_Static_assert"),
};

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
} primitive_type;

typedef enum type_classification {
    basic,
    array,
    structure, // struct or union
    function,
    pointer,
    // todo: atomic
} type_classification;

// possible optimization down the line:
// make any type pointer into an index into a type table
typedef struct type {
    string name;

    bool const_;
    bool volatile_;
    bool restrict_;

    type_classification tag;
    union {
        primitive_type primitive;
        struct array_type *array;
        struct structure_type *structure;
        struct function_type *function;
        struct type *pointer;
    };
} type;

typedef struct array_type {
    size_t size;
    type element_type;
} array_type;

typedef struct member {
    string name;
    type *ty;
    size_t offset;
} member;

typedef struct structure_type {
    string name;
    size_t size;
    vector members;
} structure_type;

typedef struct function_type {
    struct type *return_type;
    vector parameters;
} function_type;

typedef struct value {
    enum location { reg, stack, imm, flags };

    enum location loc;
    uint32_t val;
} value;

typedef struct variable {
    string name;
    type *ty;
    value val;
} variable;

typedef struct global {
    string name;
    type *ty;
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
    const_ = 0b1,
    volatile_ = 0b10,
    restrict_ = 0b100,
} type_qualifier;

typedef enum function_specifier {
    /* none = 0 */
    inline_ = 0b1,
    noreturn = 0b10,
} function_specifier;

typedef struct declaration {
    string name;

    storage_class_specifier storage_class_spec;
    type_qualifier type_qualifier_spec_mask;
    function_specifier function_spec_mask;
    size_t alignment;

    type *ty;
} declaration;

typedef struct context {
    jmp_buf error_jump;

    span source;
    size_t i;

    type *structs;
    type *unions;
    type *enums;
    type *typedefs;

    vector result;
} context;
