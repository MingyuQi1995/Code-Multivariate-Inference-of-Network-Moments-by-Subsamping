#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=102
#SBATCH --time=40:00:00
#SBATCH --partition=msismall,preempt,agsmall,ag2tb
#SBATCH --mem=490g
#SBATCH --account=tianxili
#SBATCH --output=out/bvaryG.out
#SBATCH -M agate


module load R
Rscript bvaryG.R
