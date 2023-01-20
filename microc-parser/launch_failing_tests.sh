#! /bin/bash

cd test/samples/fail

for FILE in *;
do echo -----------------------------------------------------------
echo $FILE:
dune exec ../../codegen_test.exe -- $FILE
echo -----------------------------------------------------------
echo press any char to "continue"
read -n 1 -s
done