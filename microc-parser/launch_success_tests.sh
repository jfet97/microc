#! /bin/bash

# do dune exec ./test/codegen_test.exe -- $FILE

for FILE in test/samples/success/*;
do echo -----------------------------------------------------------
echo $FILE:
source "compile.sh" $FILE
echo -----------------------------------------------------------
echo press any char to "continue"
read -n 1 -s
done