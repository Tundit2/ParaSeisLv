gfortran -o mesh mesh.f
./prep.sh
./mesh
./testcase_cpcaldat.sh
cp ../ddm ../../testcase