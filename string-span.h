#include <memory.h>
#include <stddef.h>
#include <stdbool.h>

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
