#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --time=95:00:00
#SBATCH --partition=msismall,preempt,agsmall,ag2tb
#SBATCH --mem=490g
#SBATCH --account=tianxili
#SBATCH --output=out/bvarySS.out
#SBATCH -M agate


module load R
Rscript bvarySS.R

