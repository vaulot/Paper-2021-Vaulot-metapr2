#!/bin/bash
#SBATCH -p long                      # partition
#SBATCH -N 1                         # nombre de nœuds
#SBATCH -n 1                         # nombre de cœurs
#SBATCH --cpus-per-task 16
#SBATCH --mem-per-cpu 8GB                    # mémoire vive pour l'ensemble des cœurs
##SBATCH -t 6-0:00                    # durée maximum du travail (D-HH:MM)
#SBATCH -o slurm.%N.%j.out           # STDOUT
#SBATCH -e slurm.%N.%j.err           # STDERR
#SBATCH --mail-user=vaulot@gmail.com
#SBATCH --mail-type=BEGIN,END


# Partition can be also fast, long, bigmem





# Submitted with 
# sbatch sbatch_dada2.sh


# Print information about the current job
# ---------------------------------------

# Replace by the directory where the script resides
DIR= "/projet/umr7144/dipo/vaulot/metabarcodes/R"

DATE=`date +%Y-%m-%d`

cd $DIR

module load cutadapt/2.8

module load r/3.5.1

Rscript --no-save --no-restore script_dada2.R > script_dada2_$DATE.out

