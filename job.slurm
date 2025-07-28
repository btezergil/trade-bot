#!/bin/bash
#SBATCH -p debug
#SBATCH -J clj_algo_test
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -c 40
#SBATCH -C orfoz
#SBATCH -A btezergil
#SBATCH --time=0:30:00

echo "SLURM_NODELIST $SLURM_NODELIST"
echo "NUMBER OF CORES $SLURM_CPUS_ON_NODE"

apptainer run --pwd /app --bind .:/app/out clj-scraps-app_latest.sif
