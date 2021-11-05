
# summarize asv_set -------------------------------------------------------


summarise_asv_set <- function(asv_set) {
  
  
  # Map of number of samples    
  asv_samples <- asv_set %>% 
    group_by(file_code, longitude, latitude) %>% 
    summarize(n = n()) %>% 
    group_by(longitude, latitude)%>% 
    summarize(n_samples = n()) %>% 
    ungroup()
  
  
  
  world <- map_data("world")
  
  g <- ggplot() + 
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey") + 
    coord_fixed(1.3) +
    geom_point(data=asv_samples, aes(x=longitude, y=latitude, size=n_samples), fill="blue", shape=21) + 
    scale_size_area(trans="log10") +
    ggtitle("Number of samples")
  print(g)
  
  phyloseq_long_treemap(asv_set, 
                        division, class,
                        "Class - Number of reads" )
  phyloseq_long_treemap(asv_set, 
                        class, genus, 
                        "Genus - Number of reads" )
  
  asv_set_auto <- asv_set %>% 
    filter(division %in% c("Chlorophyta", "Cryptophyta", "Rhodophyta",
                           "Haptophyta", "Ochrophyta") |
             ((division ==  "Dinoflagellata") & (class != "Syndiniales")) |
             (class == "Filosa-Chlorarachnea"))
  
  phyloseq_long_treemap(asv_set_auto, 
                        division, class,
                        "Photosynthetic - Class - Number of reads" )
  phyloseq_long_treemap(asv_set_auto, 
                        class, genus, 
                        "Photosynthetic - Genus - Number of reads" )
  
  return(asv_samples)
  
}


  return(asv_samples)
  
}


# Summarize genus or species ----------------------------------------------

## Specific genus or species


summarise_asv_set_species <- function(asv_set) {
  
  samples <- asv_set$samples
  df <- asv_set$df
  
  samples_species_absent <- samples %>% 
    filter(!(file_code %in% df$file_code)) %>% 
    select(latitude, longitude) %>% 
    distinct()  
  
  
  # Map of number of samples where genus found   
  asv_summary <- df %>% 
    distinct(file_code, species, longitude, latitude, .keep_all = TRUE) %>% 
    count(species, longitude, latitude, name="n_samples") 
  
  print(knitr::kable(asv_summary))
  
  
  world <- map_data("world")
  
  g <- ggplot() + 
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey") + 
    coord_fixed(1.3) +
    geom_point (data=samples_species_absent,
                aes(x=longitude, y=latitude), color="grey40", shape=3, size=1) +
    geom_point(data=asv_summary, aes(x=longitude, y=latitude, size=n_samples), fill="blue", shape=21) + 
    # scale_size_area(trans="log10") +
    ggtitle("Number of samples where species found") +
    facet_wrap(vars(species))
  print(g)
  
  # Number of reads per data set, substate    
  asv_summary <- df %>% 
    mutate(sample_label = str_c(dataset_code,substrate, sep="-")) %>% 
    group_by(species, sample_label, longitude, latitude) %>% 
    summarize(n = sum(n_reads)) 
  
  print(knitr::kable(asv_summary))
  
  g <- ggplot(asv_summary, aes(x=sample_label, y=n)) +
    geom_col() +
    coord_flip() +
    # scale_y_log10() +
    ylab("Number of reads") + 
    facet_wrap(vars(species))
  # facet_wrap(vars(species), scales="free_x") 
  print(g)
  
  # Number of reads per data set, station, and substate    
  asv_summary <- df %>% 
    mutate(sample_label = str_c(dataset_code,station_id, substrate, sep="-")) %>% 
    group_by(species, sample_label, longitude, latitude) %>% 
    summarize(n = sum(n_reads)) 
  
  print(knitr::kable(asv_summary))
  
  # Do a map  
  g <- ggplot() + 
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey") + 
    coord_fixed(1.3) +
    geom_point (data=samples_species_absent,
                aes(x=longitude, y=latitude), color="grey40", shape=3, size=1) +
    geom_jitter(data=asv_summary, 
                aes(x=longitude, y=latitude, size=n), 
                fill="blue", shape=21,
                height = 1, width=1) + 
    scale_size_area(trans="log10") +
    ggtitle("Number of reads per sample (if different depths merged together)") +
    facet_wrap(vars(species))
  print(g)
  
  
  g <- ggplot(asv_summary, aes(x=sample_label, y=n)) +
    geom_col() +
    coord_flip() +
    scale_y_log10()+
    ylab("Number of reads") + 
    facet_wrap(vars(species))
  # facet_wrap(vars(species), scales="free_x") 
  print(g)
  
  # Timing per station and species  
  
  asv_summary <- df %>% 
    # filter(dataset_id == 21) %>% 
    mutate(sample_label = str_c(dataset_code,station_id, substrate, sep="-")) %>% 
    group_by(species, dataset_id, sample_label, date) %>% 
    summarize(n = sum(n_reads))   
  
  
  
  g <- ggplot(asv_summary, aes(x=date, y=n)) +
    geom_col() +
    ylab("Number of reads")+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
    facet_grid(cols= vars(species), rows = vars(dataset_id), scales="free_y" ) 
  print(g)   
  
  return(asv_summary)
  
}