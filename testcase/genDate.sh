MR=64

cd ../MeshPartition
echo $MR > ./part
make clean
makeas
./testcase.sh
./MeshPartition

cd ../3dpartition
gfortran -o Partition3d Partition3d.f
./testcase.sh
./Partition3d

cd ../src/mesh
sed -i "/numblk =/c\      numblk = $MR" mesh.f
gfortran -o mesh mesh.f
./prep.sh
./mesh
./testcase_cpcaldat.sh
cp ../ddm ../../testcase