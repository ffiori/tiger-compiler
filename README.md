# Tiger compiler in Moscow ML for <target>

This repository contains a Tiger compiler written in Moscow ML. This work
is being done as a course assignment for the Compilers course, as part of
the Computer Science degree at Universidad Nacional de Rosario.

TODO

- Hacer script para automatizar testeos.
- Asegurarse de que todos los tests devuelvan lo que deben.
- Implementar expresiones además del tipado. tigerseman.sml.
- Testear código intermedio. Tenemos que encontrar una forma de hacer esto.
- Sacarse de encima el tipo 'pirulo'. Sacarse de encima SCAF.

DONE

- tests/tipado/in3.tig no anda. Toma a R1 como TNil, entonces cuando lo
compara con R2 que es TRecord da que son iguales.
