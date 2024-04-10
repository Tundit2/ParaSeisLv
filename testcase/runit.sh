date
mpirun -hostfile ./hostfile -np 64 -bind-to core ./ddm
date 
#nohup sh ./runit.sh > 040724 2>&1 &
#pkill -9 ddm