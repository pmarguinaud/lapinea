#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --partition=gpu_p2,gpu_p1
#SBATCH --time 00:05:00
#SBATCH --exclusive

module load nvidia-compilers/20.11

set -x

cd /gpfswork/rech/jau/ufh62jk/lapinea/


# ./wrap_lapinea.x --case data.8 --diff
#nvprof --print-gpu-trace ./wrap_lapinea.x --case data.8 
#./wrap_lapinea.x --case lapinea.2Gb --heapsize 100
nvprof --print-gpu-trace ./wrap_lapinea.x --case lapinea.2Gb --heapsize 100

