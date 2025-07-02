#include "./compiler.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("Usage: %s <source C> <output object>\n", argv[0]);
        return 1;
    }

    const char *source_file = argv[1];
    char path[1024] = {0};
    realpath(source_file, path);

    owned_span object_code = compile(path);
    if (object_code.size == invalid_length) {
        fprintf(stderr, "Compilation failed\n");
        return 1;
    }

    const char *output_file = argv[2];
    FILE *output_fp = fopen(output_file, "w");
    if (!output_fp) {
        perror("Failed to open output file");
        return 1;
    }

    fwrite(object_code.data, object_code.size, 1, output_fp);

    fclose(output_fp);
    free(object_code.data);

    return 0;
}
