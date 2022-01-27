# Script to process fastq files for metaPR2 database

## Files

* script_dada2.R: Processing files with parameters
* sbatch_dada2.sh: An example of sbatch file to launch script on a server using the [SLURM](https://slurm.schedmd.com/quickstart.html) workload manager

## Data directory structure

### Data set
PATH_DATA_SET (for example /home/metabarcoding/my_dataset)
    /fastq: fastq files (compressed with gz)
    /cutadapt: files after cutadapt
    /fastq_filtered: filtered fastq files
    /qual_pdf: plots of quality after cutadadapt
    /dada2: output files from dada2
    /blast: output files from BLAST analysis

### PR2 database
PATH_PR2_DATABASE
    contains pr2_version_4.xx.0_18S_dada2.fasta.gz (should be the dada2 formatted PR2 file)

## Parameters

### Swtiches to adjust the processing
  remove_primers <- FALSE
  do_cutadapt     <- FALSE
  do_summary      <- FALSE
  do_plot_quality <- FALSE
  do_filtering <- FALSE # If TRUE and primers are present must also do_cutadapt = TRUE
  do_dada2    <- FALSE
  do_taxo <- TRUE
  multithread <- TRUE
  multithread_filter <- FALSE
  bigdata <- FALSE
  
### Data set 
    dataset_code <- "My dataset" # CHANGE
    dataset_id <- 1 # CHANGE
    dataset_path <- "DATA_SET_PATH" # CHANGE to dataset path on your server our computer. Fastq files should be in DATA_SET_PATH/fastq

### PR2 database for assignation
    pr2_file <- "PR2_DATABASE_PATH" # CHANGE

### File structure
  paired_reads = TRUE
  sequencer = "Illumina"
  
  file_identifier = ".fastq"  # String to identify the files to be processed.
  R1_identifier = "_R1"
  R2_identifier = "_R2"
  file_name_separator = "_L001"

  sample.names_first_character =  1 # This the first character of the file name to consider to extract the sample name (usually = 1)


### Target gene
  gene = "18S rRNA"
  gene_region = "V4"
  organelle = "nucleus"


### Primer sets
  FWD = "CCAGCASCYGCGGTAATTCC"
  REV = "ACTTTCGTTCTTGAT"

  anchor = ""  # Put in front of primer to anchor primer at start of sequence
  
### Parameters for filterAndTrim
  truncLen = c(260,220) # This influences the number of ASVs and the percent of asv recovered (need to remove 20 and 21)
  minLen = c(260,220)
  truncQ = 2         
  maxEE = c(10, 10) 
  maxLen = 400  # This is for 454 to remove long and bad reads

### Reduce the number of asvs for problematic cases
  max_number_asvs = 0

### Parameters for removeBimeraDenovo
  method_chimera = "pooled"
