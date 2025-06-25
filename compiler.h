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
        struct array_type *array;
        struct structure_type *structure;
        struct function_type *function;
        struct pointer_type *pointer;
    };
} type;

typedef struct array_type {
    type element_type;
    size_t length;
} array_type;

typedef struct member {
    string name;
    type ty;
    size_t offset;
} member;

typedef struct structure_type {
    string name;
    vector members;
} structure_type;

typedef struct function_type {
    type return_type;
    vector parameters;
} function_type;

typedef struct pointer_type {
    type ty;
    bool restrict_;
} pointer_type;

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

    type ty;
} declaration;

typedef struct declarator {
    declaration decl;

    string name;
} declarator;

typedef struct alias {
    string name;
    type ty;
} alias;

typedef struct context {
    jmp_buf error_jump;

    span source;
    size_t i;

    vector types;
    alias *structs;
    alias *unions;
    alias *enums;
    alias *typedefs;

    vector result;
} context;
