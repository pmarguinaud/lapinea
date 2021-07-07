#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --partition=gpu_p2,gpu_p1
#SBATCH --time 00:05:00
#SBATCH --exclusive

module load nvidia-compilers/21.5
module load valgrind/3.15.0

set -x
set -e

cd /gpfswork/rech/jau/ufh62jk/lapinea/openacc-kernels

./scripts/compile.pl --update --arch cpu --bin wrap_lapinea.x --compile

 ./compile.cpu/wrap_lapinea.x --case data.8 --diff > diff.mb.txt

set +e
diff diff.ref.txt diff.mb.txt
set -e

 ./compile.cpu/wrap_lapinea.x --case data.8 --diff --single-block > diff.sb.txt
#gdb --ex run --args  ./compile.cpu/wrap_lapinea.x --case data.8 --single-block #--diff 
#valgrind  --track-origins=yes ./compile.cpu/wrap_lapinea.x --case data.8 --single-block #--diff 

set +e
#diff diff.ref.txt diff.sb.txt
set -e

exit


# ./wrap_lapinea.x --case data.8 --diff
#nvprof --print-gpu-trace ./wrap_lapinea.x --case data.8 
#./wrap_lapinea.x --case lapinea.2Gb --count 200 --heapsize 100 --diff --diff-block-list 2
nvprof --print-gpu-trace ./wrap_lapinea.x --case lapinea.2Gb --count 200 --heapsize 100  --times 8
#nvprof --print-gpu-trace ./wrap_lapinea.x --case lapinea.2Gb --heapsize 100

