#include <stddef.h>

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
