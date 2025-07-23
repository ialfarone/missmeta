#!/bin/bash
#SBATCH --job-name="missmeta"
#SBATCH --partition="queue1"
#SBATCH --mem-per-cpu=5G
#SBATCH --output=long-job.out
#SBATCH --mail-user=Irene.Alfarone@uibk.ac.at
#SBATCH --mail-type ALL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=75
#SBATCH --time=00:00:00
# rispetto alla lezione hpc, ho visto che il comando
# funziona anche senza mettere srun Rscript ...
srun Rscript --vanilla mnar-logse-maha.R
