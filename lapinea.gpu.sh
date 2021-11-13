#!/bin/bash
#SBATCH --export=NONE
#SBATCH --account=hun@gpu
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --time 00:05:00
#SBATCH --exclusive

module load nvidia-compilers/21.5
module load valgrind/3.15.0

set -x
set -e

cd /gpfswork/rech/jau/ufh62jk/lapinea/openacc-vector-stack


list="gpu"

if [ 0 -eq 1 ]
then

for arch in $list
do
  ./scripts/compile.pl --update --arch $arch --bin wrap_lapinea.x --compile
done

fi

for arch in $list
do

#xport PGI_ACC_NOTIFY=1

#nvprof ./compile.$arch/wrap_lapinea.x --case      data.8 --heapsize 4000 --diff 
 nvprof ./compile.$arch/wrap_lapinea.x --case lapinea.2Gb --heapsize 4000 --diff --diff-block-list 1  

 #set +e
 #diff diff.ref.txt diff.$arch.txt
 #set -e

done

exit


# ./wrap_lapinea.x --case data.8 --diff
#nvprof --print-gpu-trace ./wrap_lapinea.x --case data.8 
#./wrap_lapinea.x --case lapinea.2Gb --count 200 --heapsize 100 --diff --diff-block-list 2
nvprof --print-gpu-trace ./wrap_lapinea.x --case lapinea.2Gb --count 200 --heapsize 100  --times 8
#nvprof --print-gpu-trace ./wrap_lapinea.x --case lapinea.2Gb --heapsize 100

