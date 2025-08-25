# c-liva

c-liva is aiming to be a self-hosted single-pass C11 compiler targeting ARM64 MacOS machines. Currently it can parse a large chunk of C, but there's still much to be desired in the codegen department.

You may note the logic is almost exclusively contained within `compiler.c`. This is mainly to make it easier to compile itself, and it'll be split up more properly in the future.
