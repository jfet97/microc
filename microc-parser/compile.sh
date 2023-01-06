if ! [ -z "$2" ]
  then
    echo "Found second argument: deleting build dir" &&
    rm -rf _mc_build 
fi



mkdir -p _mc_build

cd _mc_build

clang -emit-llvm -c ../bin/rt-support.c &&

dune exec --profile release ../bin/microcc.exe -- ../$1 &&

llvm-link rt-support.bc a.bc -o output.bc &&

llc -filetype=obj output.bc &&

clang output.o -o a.out &&

./a.out