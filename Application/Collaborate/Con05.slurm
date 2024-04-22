#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --time=60:00:00
#SBATCH --partition=msismall
#SBATCH --mem=400g
#SBATCH --account=tianxili
#SBATCH --output=/home/tianxili/qim/Mingyu_network/Collaborate/out/Con05.out
#SBATCH -M agate


module load R
Rscript Con05.R