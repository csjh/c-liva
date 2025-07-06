#include "./compiler.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("Usage: %s <source C> <output object>\n", argv[0]);
        return 1;
    }

    const char *source_file = argv[1];
    char path[1024] = {0};
    realpath(source_file, path);

    const char *output_file = argv[2];
    FILE *output_fp = fopen(output_file, "w");
    if (!output_fp) {
        perror("Failed to open output file");
        return 1;
    }

    if (!compile(path, output_fp)) {
        fprintf(stderr, "Compilation failed\n");
        fclose(output_fp);
        return 1;
    }

    printf("Compilation successful, output written to %s\n", output_file);
    fclose(output_fp);
    return 0;
}
