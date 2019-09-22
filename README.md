# Small Portable Forth

Based on [Jones Forth](git.annexia.org/?p=jonesforth.git). The number
of natively defined words is kept very low on purpose to ease porting.

A C compiler (GCC and MSVC tested) is needed to build the C version.

The x86/x64 assembly versions require [NASM](https://nasm.us/).

It's easiest to build using [CMake](https://cmake.org/). In the source
directory run:

    mkdir build
    cd build
    cmake ..
    cmake --build .

To cross-compile for ARM (for debian install `gcc-arm-linux-gnueabihf`
and `libc6-dev-armhf-cross`):

    cmake -DCMAKE_TOOLCHAIN_FILE:PATH=../toolchain-arm.cmake ..

The interpreter(s) read from standard input by default, but a filename
can be supplied on the command line.
