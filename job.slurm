#!/bin/bash
#SBATCH -p debug
#SBATCH -J clj_algo_test
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -c 20
#SBATCH -C barbun
#SBATCH -A btezergil
#SBATCH --time=0:30:00

echo "SLURM_NODELIST $SLURM_NODELIST"
echo "NUMBER OF CORES $SLURM_NTASKS"

apptainer run --pwd /app clj-scraps-app_latest.sif
