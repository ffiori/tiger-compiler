# Tiger compiler in Moscow ML for RISC-V

This repository contains a Tiger compiler written in Moscow ML. This work
is being done as a course assignment for the Compilers course, as part of
the Computer Science degree at Universidad Nacional de Rosario.

# Building

To compile the Tiger compiler, use `make`. You will need Moscow ML
available on your system to do this.

# Usage

You will need a riscv64 toolchain to produce binaries. You can download
a precompiled one from [Bootlin](https://toolchains.bootlin.com/). Make
sure it is available on the system `PATH`

    $ export PATH="/path/to/toolchain/bin:$PATH"

You can use the compiler to generate RISC-V assembler. It will then use
`riscv64-linux-gcc` to assemble it and link it into an ELF binary.

    $ ./tiger program.tig

To run this program without a RISC-V system running a GNU/Linux system,
you can use [rv8](https://rv8.io/) (see "Building rv8" for build and
installation instructions). To use without installing, you can use the
scripts available in `bin/` in the rv8 repository.

    $ bin/rv8-sim a.out

These scripts have a small bug, causing them not to pass the arguments
to rv8. To fix it, you can apply the following patch.

    $ diff --git a/bin/rv-jit b/bin/rv-jit
    index d45663e..acb0ad8 100755
    --- a/bin/rv-jit
    +++ b/bin/rv-jit
    @@ -3,4 +3,4 @@ OS=$(uname -s | sed 's/ /_/' | tr A-Z a-z)
     CPU=$(uname -m | sed 's/ /_/' | tr A-Z a-z)
     test "$OS" = "darwin" &&  export DYLD_LIBRARY_PATH=build/${OS}_${CPU}/lib
     test "$OS" = "linux" &&  export LD_LIBRARY_PATH=build/${OS}_${CPU}/lib
    -exec build/${OS}_${CPU}/bin/rv-jit
    +exec build/${OS}_${CPU}/bin/rv-jit "$@"
    
    $ diff --git a/bin/rv-sim b/bin/rv-sim
    index d7f8f36..c16c3d5 100755
    --- a/bin/rv-sim
    +++ b/bin/rv-sim
    @@ -3,4 +3,4 @@ OS=$(uname -s | sed 's/ /_/' | tr A-Z a-z)
     CPU=$(uname -m | sed 's/ /_/' | tr A-Z a-z)
     test "$OS" = "darwin" &&  export DYLD_LIBRARY_PATH=build/${OS}_${CPU}/lib
     test "$OS" = "linux" &&  export LD_LIBRARY_PATH=build/${OS}_${CPU}/lib
    -exec build/${OS}_${CPU}/bin/rv-sim
    +exec build/${OS}_${CPU}/bin/rv-sim "$@"