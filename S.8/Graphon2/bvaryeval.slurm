#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=51
#SBATCH --time=10:00:00
#SBATCH --partition=msismall,agsmall,ag2tb
#SBATCH --mem=200g
#SBATCH --account=tianxili
#SBATCH --output=out/eval.out
#SBATCH -M agate


module load R
Rscript bvaryeval.R



