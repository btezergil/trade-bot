#!/bin/bash
#SBATCH -p orfoz
#SBATCH -J clj_algo
#SBATCH -N 1
#SBATCH -n 110
#SBATCH -c 1
#SBATCH -C weka
#SBATCH --time=3-00:00:00
#SBATCH -A btezergil
#SBATCH --threads 2

echo "SLURM_NODELIST $SLURM_NODELIST"
echo "NUMBER OF CORES $SLURM_NTASKS"

apptainer run --pwd /app clj-scraps-app_latest.sif

