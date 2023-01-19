rm -rf _mc_build

FILE=$1

shift 1

CURR_DIR=$PWD

mkdir -p _mc_build

cd _mc_build

clang -emit-llvm -c ../bin/rt-support.c &&

dune exec ../bin/microcc.exe -- ../$FILE $@ &&

llvm-link rt-support.bc a.bc -o output.bc &&

llc -filetype=obj output.bc &&

clang output.o -o a.out &&

timeout 15s ./a.out

cd $CURR_DIR