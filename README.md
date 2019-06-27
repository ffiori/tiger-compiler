# Tiger compiler in Moscow ML for RISC-V

This repository contains a Tiger compiler written in Moscow ML. This work
is being done as a course assignment for the Compilers course, as part of
the Computer Science degree at Universidad Nacional de Rosario.

# How to use

Bajar toolchain riscv64 https://toolchains.bootlin.com/
$ export PATH="/donde/este/la/toolchain/bin:$PATH"

$ ./tiger coso.tig (* esto genera un prog.s en la carpeta *)
$ riscv64-linux-gcc prog.s runtime.c -static

simulador https://rv8.io/ (ver "building rv8"). Para usar sin instalar, usar los scripts de bin/ [*]

Se usa $ bin/rv8-sim a.out

[*] tienen un bugcito esos scripts, hay que agregarles "$@" o si no no pasan los args

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
