#include "./containers.h"
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

owned_span compile(string source);

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

typedef struct context {
    jmp_buf error_jump;

    string source;
    size_t i;

    vector types;
    alias *structs;
    alias *unions;
    alias *enums;
    alias *typedefs;

    vector globals;
    vector functions;

    vector result;
} context;
