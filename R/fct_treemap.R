
# Fn to draw treemaps -----------------------------------------------------


treemap <- function(df, fasta, taxo_level, taxo_colors) {
  
    taxo_level_number = which(global$taxo_levels == taxo_level)
  
  # Do not go beyond ASV level (taxo_level_number = 9)
  if(taxo_level_number < 8 ){
    taxo_level_1 = global$taxo_levels[taxo_level_number + 1] 
    taxo_level_2 = global$taxo_levels[taxo_level_number + 2] 
  } else {
    taxo_level_1 = global$taxo_levels[taxo_level_number] 
    taxo_level_2 = global$taxo_levels[taxo_level_number + 1]   
  }
  
  # Group
  # df <- df %>%
  #   count(!!as.symbol(taxo_level_1), !!as.symbol(taxo_level_2), wt=n_reads) %>% 
  #   ungroup()
  
  df <- df %>%
    count(across(all_of(c(taxo_level_1, taxo_level_2))), wt=n_reads_pct) %>% 
    ungroup()
  
  fasta <- fasta %>%
    count(across(all_of(c(taxo_level_1, taxo_level_2)))) %>% 
    ungroup()
  
  
  
  
  # Do a treemap
  
  # ggplot(df, aes(area = n, fill = {{level2}}, subgroup = {{level1}}, label = {{level2}})) +
  #   treemapify::geom_treemap()
  
  treemap_plot <- function(df, title){
    
    ggplot(df, aes(area = n, 
                 fill = .data[[taxo_level_1]],
                 subgroup = .data[[taxo_level_1]], 
                 label = .data[[taxo_level_2]])) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
    treemapify::geom_treemap_subgroup_border() +
    treemapify::geom_treemap_subgroup_text(place = "topleft", grow = F, 
                                           alpha = 0.5, colour = "black", 
                                           min.size = 0) +
    theme_bw() +
    scale_fill_manual(values = taxo_colors) +
    guides(fill = "none") +
    ggtitle(title)
    }
  
  # cat("Treemap: ")
  # print(pryr::mem_used())
  
  g1 <- treemap_plot(df, "Reads")
  g2 <- treemap_plot(fasta, "cASVs")
  
  g <- g1 + g2
  
  return(list(g = g, df=df))
  
}