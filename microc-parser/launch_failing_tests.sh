#! /bin/bash

cd test/samples/fail

for FILE in *; 
do dune exec ../../codegen_test.exe -- $FILE
echo this was $FILE
read -n 1 -s
done