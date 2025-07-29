#!/bin/bash
#SBATCH -p orfoz
#SBATCH -J clj_algo_test
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -c 110
#SBATCH -C weka
#SBATCH -A btezergil
#SBATCH --time=8:00:00

echo "SLURM_NODELIST $SLURM_NODELIST"
echo "NUMBER OF CORES $SLURM_CPUS_ON_NODE"

apptainer run --pwd /app --bind .:/app/out clj-scraps-app_latest.sif
