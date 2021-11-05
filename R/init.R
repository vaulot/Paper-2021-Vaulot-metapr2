
suppressPackageStartupMessages({
# Bioconductor
  library(Biostrings)
  library(phyloseq)

# tidyr libraries
  library(stringr)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(rio)
  
  library(ggplot2)
  library(patchwork)
  
  library(here)
  
  
  library(maps)
  
# to format tables
  library(knitr)
  library(kableExtra)


# Database libraries
  library(DBI)
  library(RMySQL)

# Libraries dvutils and pr2database -------------------------------------------------------
  if(any(grepl("package:dvutils", search()))) detach("package:dvutils", unload=TRUE)
  library("dvutils")

})

# Load functions ----------------------------------------------------

source(here("R", "fct_latex.R"))

source(here("R", "fct_treemap.R"))


# source("fn_database.R")

# paths -------------------------------------------------------------------


path_table <- function(file) {str_c("../../paper-metapr2-overleaf/tables/", file)}

path_fig <- function (file) str_c("../../paper-metapr2-overleaf/figs/", file)




# Load data ----------------------------------------------------

asv_set <- qs::qread(here("data", "asv_set.qs"))
global <- qs::qread(here("data", "global.qs"))


# Merge the data ----------------------------------------------------------

asv_set$df <- asv_set$df %>%
  left_join(asv_set$samples) %>% 
  left_join(select(asv_set$fasta, asv_code, kingdom:species, sum_reads_asv)) %>%
  filter(!is.na(kingdom)) 




