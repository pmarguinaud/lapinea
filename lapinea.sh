#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --account=hun@gpu
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --gres=gpu:2

module load nvidia-compilers/20.11

set -x

cd /gpfswork/rech/jau/ufh62jk/lapinea/notmanaged


# ./wrap_lapinea.x --case data.8 --diff
#nvprof --print-gpu-trace ./wrap_lapinea.x --case data.8 
#./wrap_lapinea.x --case lapinea.2Gb --count 200 --heapsize 100 --diff --diff-block-list 2


 nvprof --print-gpu-trace              ./wrap_lapinea.x --case lapinea.2Gb --count 200 --heapsize 100  --times 8


 nsys profile -f true -o lapinea.qdrep ./wrap_lapinea.x --case lapinea.2Gb --count 200 --heapsize 100  --times 8


#nvprof --print-gpu-trace ./wrap_lapinea.x --case lapinea.2Gb --heapsize 100

