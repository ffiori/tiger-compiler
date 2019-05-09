# Tiger compiler in Moscow ML for <target>

This repository contains a Tiger compiler written in Moscow ML. This work
is being done as a course assignment for the Compilers course, as part of
the Computer Science degree at Universidad Nacional de Rosario.

TODO

- tests/tipado/co2.tig y co4.tig no andan. Si se setean los campos de un
record en otro orden que el que se define el tipo se rompe.
- tests/tipado/in3.tig no anda. Toma a R1 como TNil, entonces cuando lo
compara con R2 que es TRecord da que son iguales.
