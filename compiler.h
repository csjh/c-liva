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
