#include "./compiler.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("Usage: %s <source C> <output object>\n", argv[0]);
        return 1;
    }

    const char *source_file = argv[1];
    FILE *source_fp = fopen(source_file, "r");
    if (!source_fp) {
        perror("Failed to open source file");
        return 1;
    }

    string source;

    fseek(source_fp, 0, SEEK_END);
    source.length = ftell(source_fp);
    fseek(source_fp, 0, SEEK_SET);

    source.data = malloc(source.length);
    if (!source.data) {
        perror("Failed to allocate memory for source file");
        fclose(source_fp);
        return 1;
    }

    fread(source.data, 1, source.length, source_fp);
    fclose(source_fp);

    owned_span object_code = compile(source);
    if (object_code.size == -1) {
        fprintf(stderr, "Compilation failed\n");
        free(source.data);
        return 1;
    }

    const char *output_file = argv[2];
    FILE *output_fp = fopen(output_file, "w");
    if (!output_fp) {
        perror("Failed to open output file");
        free(source.data);
        return 1;
    }

    fwrite(object_code.data, object_code.size, 1, output_fp);

    fclose(output_fp);
    free(source.data);
    free(object_code.data);

    return 0;
}
