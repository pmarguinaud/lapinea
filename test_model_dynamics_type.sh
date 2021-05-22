#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --partition=gpu_p2,gpu_p1
#SBATCH --time 00:05:00
#SBATCH --exclusive

set -x

cd /gpfswork/rech/jau/ufh62jk/lapinea/

./test_model_dynamics_type.x

