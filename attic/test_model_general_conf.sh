#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --partition=gpu_p2,gpu_p1
#SBATCH --time 00:05:00
#SBATCH --exclusive

set -x

ulimit -c 0

cd /gpfswork/rech/jau/ufh62jk/lapinea/

./test_model_general_conf.x

