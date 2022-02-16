#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --time 00:05:00
#SBATCH --exclusive
#SBATCH -p ndl

set -x
set -e

cd /scratch/work/marguina/lapinea/test-character


./compile.gpu/wrap_lapinea.x --case data.8 

