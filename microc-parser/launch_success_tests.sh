#! /bin/bash

cd test/samples/success

for FILE in *; 
do dune exec --profile release ../../codegen_test.exe -- $FILE
echo this was $FILE
read -n 1 -s
done