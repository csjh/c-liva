#include <memory.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

typedef struct vector {
    void *data;
    size_t size;
    size_t capacity;
} vector;

#define vector_at(type, vec, index) ((type *)((vec)->data))[index]
#define vector_push(type, vec, value)                                          \
    do {                                                                       \
        if ((vec)->size >= (vec)->capacity) {                                  \
            size_t new_capacity = (vec)->capacity ? (vec)->capacity * 2 : 1;   \
            (vec)->data = realloc((vec)->data, new_capacity * sizeof(type));   \
            (vec)->capacity = new_capacity;                                    \
        }                                                                      \
        vector_at(type, vec, (vec)->size++) = value;                           \
    } while (0)

typedef struct string {
    char *data;
    size_t length;
} string;

#define string_literal(str) ((string){.data = str, .length = sizeof(str) - 1})

static inline bool string_equal(string a, string b) {
    if (a.length != b.length)
        return false;

    return memcmp(a.data, b.data, a.length) == 0;
}

typedef struct span {
    void *data;
    size_t size;
} span;

typedef struct owned_span {
    void *data;
    size_t size;
} owned_span;

#define make_owned_span(ty, size)                                              \
    ((owned_span){.data = malloc((size) * sizeof(ty)), .size = (size)})

#define owned_span_from_vector(vec)                                            \
    ((owned_span){.data = (vec).data, .size = (vec).size})
