
suppressPackageStartupMessages({
# Bioconductor
  library(Biostrings)
  library(phyloseq)

# tidyr libraries
  library(stringr)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(forcats)  
  
  library(rio)
  
  library(ggplot2)
  library(patchwork)
  library(ggcharts)
  
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

source(here("R", "fct_barplot.R"))

source(here("R", "fct_phyloseq.R"))



# source("fn_database.R")

# paths -------------------------------------------------------------------


path_table <- function(file) {str_c("../../paper-metapr2-overleaf/tables/", file)}

path_fig <- function (file) str_c("../../paper-metapr2-overleaf/figs/", file)




# Load data ----------------------------------------------------

asv_set <- qs::qread(here("data", "asv_set.qs"))
global <- qs::qread(here("data", "global.qs"))



#  Merge the data with clusters ------------------------------------------


  metapr2_db <- dvutils::db_info("metapr2_google")
  metapr2_db_con <- dvutils::db_connect(metapr2_db)
  
  clusters <- tbl(metapr2_db_con, "metapr2_asv_clusters_version_1.0") %>% 
    collect() 
  
  dvutils::db_disconnect(metapr2_db_con)

# Only get 100% clusters.
  clusters <- clusters %>% 
    filter(record_type == "H",
           pct_sim == 100) %>%
    rename(asv_code = hash_value,
           asv_code_centroid = hash_value_centroid) %>% 
    mutate(asv_code = str_sub(asv_code,1,10),
           asv_code_centroid = str_sub(asv_code_centroid,1,10)) %>% 
    select(-record_type, -pct_sim)
    
# Check whether any duplicate ASVs
  clusters %>% 
    count(asv_code) %>% 
    filter(n> 1)

  asv_set$fasta <- asv_set$fasta %>% 
    left_join(clusters)

# Create a new df where the clusters are merged 

  asv_set$df_cluster <- asv_set$df %>% 
    left_join(clusters) %>% 
    mutate(asv_code = case_when(!is.na(asv_code_centroid )~ asv_code_centroid,
                                TRUE ~asv_code)) %>% 
    group_by(file_code, asv_code) %>% 
    summarize(n_reads = sum(n_reads),
              n_reads_pct = sum(n_reads_pct)) %>% 
    ungroup()
  
  cat("Are the 2 sums identical: ", sum(asv_set$df_cluster$n_reads) == sum(asv_set$df$n_reads))

# Merge the data ----------------------------------------------------------

asv_set$df <- asv_set$df %>%
  left_join(asv_set$samples) %>% 
  left_join(select(asv_set$fasta, asv_code, kingdom:species, ecological_function, trophic_group, sum_reads_asv, asv_code_centroid)) %>%
  filter(!is.na(kingdom))

asv_set$df_cluster <- asv_set$df_cluster %>%
  left_join(asv_set$samples) %>% 
  left_join(select(asv_set$fasta, asv_code, kingdom:species, ecological_function, trophic_group, sum_reads_asv, asv_code_centroid)) %>%
  filter(!is.na(kingdom))

# Check that no centroid left in cluster

message("Number of rows with centroid in clustered (should be 0): ", nrow(filter(asv_set$df_cluster, !is.na(asv_code_centroid))))

message("Are the 2 sums identical: ", sum(asv_set$df_cluster$n_reads) == sum(asv_set$df$n_reads))

message("No line with missing taxo: ", nrow(asv_set$df_cluster %>%  filter(is.na(kingdom))))
    

