#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --time=50:00:00
#SBATCH --partition=msismall
#SBATCH --mem=400g
#SBATCH --account=tianxili
#SBATCH --mail-type=END,FAIL     # Mail events (can use any combination of the following: ALL, NONE, BEGIN, END, FAIL)
#SBATCH --mail-user=mq3sq@virginia.edu
#SBATCH --output=/home/tianxili/qim/Mingyu_network/Guppy/out/guppysign.out
#SBATCH -M mesabi


module load R
Rscript guppysign.R