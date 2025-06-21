#include "./vector.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef union byte {
    uint8_t _;
} byte;

typedef struct span {
    byte *data;
    size_t size;
} span;

span compile(span source);

typedef struct string {
    char *data;
    size_t length;
} string;

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

typedef struct context {

} context;
