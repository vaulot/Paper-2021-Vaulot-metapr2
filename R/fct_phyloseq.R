# Phyloseq filtration -----------------------------------------------------

# - Cannot the function select_samples because it is based on the base function select which is limited 


ps_select <- function(ps, gene_region, DNA_RNA, ecosystem, depth_level, fraction_name, substrate, datasets_selected_id,
                      ps_reads_min, 
                      taxo_level, taxo_name) {
  
  # This function returns an error message ("character") if no samples selected and an "S4" object if selection is OK
  
  # if((length(DNA_RNA) == 0)) {
  #   validate("No DNA_RNA selected. Will use the whole dataset.")
  #   return(ps)
  # }
  
  keepSamples = 
    (phyloseq::get_variable(ps, "gene_region") %in% gene_region ) &
    (phyloseq::get_variable(ps, "DNA_RNA") %in% DNA_RNA ) & 
    (phyloseq::get_variable(ps, "ecosystem") %in% ecosystem)  & 
    (phyloseq::get_variable(ps, "depth_level") %in% depth_level) & 
    (phyloseq::get_variable(ps, "fraction_name") %in% fraction_name)& 
    (phyloseq::get_variable(ps, "substrate") %in% substrate)& 
    (phyloseq::get_variable(ps, "dataset_id") %in% datasets_selected_id)
  
  # Check some samples are left...
  if(sum(keepSamples[keepSamples]) > 1 ) {  
    ps1 <- phyloseq::prune_samples(keepSamples, ps) %>%  
      # Need to remove the taxa that do not have minimum number of reads
      phyloseq::filter_taxa(function(x) sum(x) >= ps_reads_min, prune = TRUE)
  }
  else {
    # validate("No samples selected. Will use the whole dataset.")
    return("No sample selected")
  }
  
  TT = as(phyloseq::tax_table(ps1), "matrix")
  keepTaxa = TT[, taxo_level] %in% taxo_name
  
  if(sum(keepTaxa) > 1){
    ps1 <- phyloseq::prune_taxa(keepTaxa, ps1) 
    # Need to remove the samples that do not have the taxon
    ps1 <- phyloseq::prune_samples(phyloseq::sample_sums(ps1)>0, ps1)
  } else {
    # validate("One or Zero taxon left - Will use all taxa.")
    return("No taxa selected")
  }
  
  return(ps1)
  
}


# Alpha diversity ---------------------------------------------------------

ps_alpha <- function(ps, measures = c("Shannon"), 
                     x="latitude" , color="depth", shape="fraction_name",
                     discretize = FALSE) {
  
  variable_discrete <- c("depth_level", "fraction_name", "DNA_RNA", "ecosystem", "substrate")
  
  phyloseq_discretize <- function(ps, x, width, boundary, reverse = TRUE){
    if (length(unique(phyloseq::get_variable(ps, x))) > 1) {
      if (reverse) phyloseq::sample_data(ps)[[x]] =  fct_rev(cut_width(phyloseq::get_variable(ps, x), width=width, boundary=boundary))
      else phyloseq::sample_data(ps)[[x]] =  cut_width(phyloseq::get_variable(ps, x), width=width, boundary=boundary)
    } else{
      phyloseq::sample_data(ps)[[x]] =  as.factor(phyloseq::get_variable(ps, x))
      }
      return(ps)
  }  
  
  if (discretize) {
    if (x == "depth") {
      df <- df %>% 
        filter(depth <=250)
      if (length(unique(df$depth)) > 1) {
        df <- df %>%
          mutate(depth =  cut_width(depth, width=25, boundary=0))
      } else {
        df <- df %>%
          mutate(depth =  as.factor(depth))
      }
    }  
    
    # Discretize the data (must make sure that there is more than one value)
    
    
    if (x == "depth") {
      ps <- phyloseq::subset_samples(ps, depth <= 250)
      ps <- phyloseq_discretize(ps, x = x , width=5, boundary=0, reverse = TRUE)
    }
    if (x %in%  c("temperature", "salinity")) ps <- phyloseq_discretize(ps, x = x , width=5, boundary=0, reverse = TRUE)
    if (x == "latitude")  ps <- phyloseq_discretize(ps, x = x , width=20, boundary=0, reverse = FALSE) 
    
  }
  
  diversity <- phyloseq::estimate_richness(ps, split = TRUE, measures = measures) %>% 
    tibble::rownames_to_column(var = "file_code")
  
  samples <- data.frame(phyloseq::sample_data(ps)) 
  
  samples <- samples %>% 
    tibble::rownames_to_column(var = "file_code") %>% 
    left_join(diversity) %>% 
    pivot_longer(cols = measures, values_to = "diversity", names_to = "measures")
  
  # print(head(samples))
  
  # x = "ecosystem"
  # shape= NULL
  # color= "ecosystem"
  
  gg <- ggplot(data = samples, aes(x= .data[[x]], y = diversity)) +
    theme_bw() +
    # xlim(xmin,xmax) +
    # Next line remove samples with diversity = 0 (single species)
    scale_y_continuous(limits = c(0.01, NA)) +
    facet_wrap(vars(measures)) +
    coord_flip()
  if ((x %in% variable_discrete) || discretize){
    gg <- gg +
      geom_violin() +
      # ggforce::geom_sina(aes(color=.data[[color]], shape =.data[[shape]] )) +
      ggforce::geom_sina() +
      geom_boxplot(color = "grey", alpha = 0, width=0.5) 
      # geom_jitter(size=2, alpha=0.85, aes(shape = .data[[shape]]))
  } else {
    gg <- gg +
      geom_smooth(method = "gam", alpha = 0) +
      geom_point(size=2, alpha=0.85, color = .data[[color]], shape= .data[[shape]]) 
  } 
  
  if (color %in% variable_discrete){
    # gg <- gg +
    #   scale_color_viridis_d()
  } else {
    gg <- gg +
      # scale_color_gradient(high = "darkblue", low = "lightblue1") +
      scale_color_viridis_c(option = "turbo")
  }
  

  return(gg) # To test graphics
  
}


# Beta diversity ----------------------------------------------------------

ps_beta <- function(ps, ps_ordinate, 
                    color_samples="ecosystem", 
                    shape_samples = NULL,
                    color_taxa = "supergroup") {
  

      gg_samples <- phyloseq::plot_ordination(ps, ps_ordinate,
                                type="samples",
                                color=color_samples, 
                                shape = shape_samples) +
        geom_point(size=2, alpha=0.8) +
        scale_color_viridis_d() +
        theme_bw()
        # xlim(-90,90) +
        # scale_color_gradient2(high = "darkblue", mid= "white", low = "darkred") +
      
      print(gg_samples)
        
        
        
      gg_taxa <- phyloseq::plot_ordination(ps, ps_ordinate,
                                  type="taxa",
                                  color=color_taxa) +
        geom_point(size=2, alpha=0.8) +
        scale_color_viridis_d() +
        theme_bw()
      
      print(gg_taxa)
        
      return(list(gg_samples = gg_samples, gg_taxa = gg_taxa))
        
}

# Create phyloseq file ----------------------------------------------------------

make_phyloseq <- function(samples, df, fasta){
  
  # samples = 1000, 192 Mb
  # samples = 2000, 822 Mb
  
  cols_to_include <- c("latitude", "ecosystem", "substrate", 
                       "depth_level", "depth", "fraction_name", 
                       "DNA_RNA", "temperature", "salinity")
  
  # 1. samples table : row names are labeled by file_code
  samples_df <- samples %>%
    select(file_code, any_of(cols_to_include)) %>%
    tibble::column_to_rownames(var = "file_code")
  
  
  # 2. otu table :
  otu <- df %>%
    select(asv_code, file_code, n_reads) %>%
    tidyr::pivot_wider(names_from=file_code,
                       values_from = n_reads,
                       values_fill=list(n_reads=0),
                       values_fn = mean) %>%
    tibble::column_to_rownames(var = "asv_code")
  
  # 3. Taxonomy table
  
  tax <-  fasta %>%
    select(asv_code, kingdom:species) %>%
    distinct(asv_code, .keep_all = TRUE) %>%
    tibble::column_to_rownames(var = "asv_code")
  
  ## Create and save to phyloseq object
  
  # Transform into matrixes
  otu_mat <- as.matrix(otu)
  tax_mat <- as.matrix(tax)
  
  # Transform to phyloseq object and save to Rdata file
  OTU = phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE)
  TAX = phyloseq::tax_table(tax_mat)
  samples = phyloseq::sample_data(samples_df)
  
  
  cat("Make phyloseq done \n")
  
  ps <- phyloseq::phyloseq(OTU, TAX, samples)
  
}