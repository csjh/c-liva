#include <memory.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

typedef struct vector {
    void *data;
    size_t size;
    size_t capacity;
} vector;

static void vector_grow(vector *vec, size_t type_size) {
    size_t new_capacity = (vec)->capacity ? (vec)->capacity * 2 : 1;
    (vec)->data = realloc((vec)->data, new_capacity * type_size);
    (vec)->capacity = new_capacity;
}

#define vector_at(type, vec, index) ((type *)((vec)->data))[index]
#define vector_push(type, vec, value)                                          \
    do {                                                                       \
        if ((vec)->size >= (vec)->capacity) {                                  \
            vector_grow((vec), sizeof(type));                                  \
        }                                                                      \
        vector_at(type, vec, (vec)->size++) = value;                           \
    } while (0)
#define vector_remove(type, vec, index)                                        \
    do {                                                                       \
        memmove(&vector_at(type, (vec), (index)),                              \
                &vector_at(type, (vec), (index) + 1),                          \
                sizeof(type) * ((vec)->size - (index)-1));                     \
        (vec)->size--;                                                         \
    } while (0)

typedef struct string {
    char *data;
    size_t length;
} string;

#define string_literal(str) ((string){.data = str, .length = sizeof(str) - 1})
#define invalid_length ((size_t)-1)
#define invalid_str ((string){.data = NULL, .length = invalid_length})

static inline bool string_equal(string a, string b) {
    if (a.length != b.length)
        return false;

    return memcmp(a.data, b.data, a.length) == 0;
}

static inline bool string_is_valid(string str) {
    return str.data != NULL && str.length != invalid_length;
}

typedef struct source_entry {
    string source;
    size_t i;

    struct source_entry *next;
} source_entry;

static inline void advance(source_entry **entry) {
    (*entry)->i++;
    if ((*entry)->i >= (*entry)->source.length) {
        *entry = (*entry)->next;
    }
}

static inline bool inplace_advance(source_entry *_entry) {
    source_entry *entry = _entry;
    advance(&_entry);
    if (_entry != NULL)
        return false;
    *entry = *_entry;
    return true;
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

// could be worth realloc'ing down to the final size
#define owned_span_from_vector(vec)                                            \
    ((owned_span){.data = (vec).data, .size = (vec).size})
