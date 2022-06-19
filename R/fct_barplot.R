

# Barplots for taxa ----------------------------------------------------


barplot <- function(df, variable, color_coding, taxo_level) {
  
  variable_date <- c("year", "month", "day")
  
  if (variable == "year") {
    date_breaks = "1 year"
    date_labels = "%Y"
    date_angle = 0
  }
  if (variable %in%c( "month", "day")) { 
    date_breaks = "1 month"
    date_labels = "%Y-%m"
    date_angle = 45
  }
  
  
  
  if(color_coding == "taxonomy")  {
    color_col <- taxo_level }
  else {
    color_col <- color_coding
  }
  
  variable_to_use <- variable
  
  if(variable %in% variable_date) {
    variable_to_use <- "date"
  } 
  
  # https://statisticsglobe.com/aggregate-daily-data-to-month-year-intervals-in-r
  
  
  
  
  
  df <- df %>% 
    select(file_code, any_of(c(color_col, variable_to_use)), n_reads_pct)
  
  # For depth only use the first 250 m
  
  if (variable == "depth") {
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
  
  # floor_date: Round date-times down.
  
  if(variable %in% variable_date) {
    df <- df %>%
      filter(!is.na(date)) %>% 
      mutate(date =  lubridate::floor_date(as.Date(date), variable))
  } 
  
  # Discretize the data (must make sure that there is more than one value)
  
  
  if (variable == "temperature") {
    if (length(unique(df$temperature)) > 1) {
      df <- df %>%
        mutate(temperature =  fct_rev(cut_width(temperature, width=5, boundary=0)))
    } else {
      df <- df %>%
        mutate(temperature =  as.factor(temperature))
    }
  }
  
  if (variable == "salinity") {
    if (length(unique(df$salinity)) > 1) {
      df <- df %>%
        mutate(salinity =  fct_rev(cut_width(salinity, width=5, boundary=0)))
    } else {
      df <- df %>%
        mutate(salinity =  as.factor(salinity))
    }
  }
  
  if (variable == "latitude") {  
    if (length(unique(df$latitude)) > 1) {
      df <- df %>%
        mutate(latitude =  fct_rev(cut_width(latitude, width=20, boundary=0)))
    } else {
      df <- df %>%
        mutate(latitude =  as.factor(latitude))
    }
  }
  
  # variable_to_use = "ecosystem" # For testing
  samples <- df %>% 
    select(file_code, any_of(c(variable_to_use))) %>% 
    distinct() %>% 
    group_by(across(any_of(variable_to_use))) %>% 
    count() 
    
  df <- df %>% 
    group_by(across(any_of(c(color_col, variable_to_use)))) %>%
    summarize(n_reads_pct = sum(n_reads_pct)) %>%
    group_by(across(any_of(variable_to_use))) %>% 
    mutate(n_reads_pct = n_reads_pct/sum(n_reads_pct)*100) 

    
  
  
  # cat(variable_to_use, "\n")
  # print(df)
  
  gg <- df  %>% 
    ggplot() +
    xlab("% of reads") + ylab("") +
    theme_classic() 
  
  if(variable_to_use == "date") {
    gg <- gg +
      geom_col(aes (x= .data[[variable_to_use]],
                    y=n_reads_pct, 
                    fill=.data[[color_col]])) +
      geom_text(data = samples, 
                aes(y = 100, 
                    x = .data[[variable_to_use]], 
                    label = glue::glue("n = {n}"),
                ),
                nudge_y = 10)  +
      scale_x_date(date_breaks = date_breaks,
                   date_labels = date_labels) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))  +
      scale_y_continuous(breaks = seq(0,100,20)) 
  }
  else {
    gg <- gg +
      geom_col(aes(y= fct_rev(.data[[variable_to_use]]),
                   x=n_reads_pct, 
                   fill=.data[[color_col]])) +
      geom_text(data = samples, 
                aes(x = 100, 
                    y = fct_rev(.data[[variable_to_use]]), 
                    label = glue::glue("n = {n}"),
                ),
                nudge_x = 10, 
                size = 3) +
      scale_x_continuous(breaks = seq(0,100,20))
  }
  
  if(variable %in% c("month", "day")) {   # Add vertical limits
    gg <- gg +
      geom_vline(xintercept= as.numeric(as.Date(str_c(c(2000:2030), "-01-01"))))
  }
  
  if(color_coding == "taxonomy"){ 
    if( taxo_level == "supergroup") gg <- gg + scale_fill_manual(values = supergroup_colors)
    else gg <- gg + scale_fill_viridis_d()
    
  }
  if(color_coding == "ecological_function"){ 
    gg <- gg + scale_fill_manual(values = ecological_function_colors)
  }
  
  if(color_coding == "trophic_group"){ 
    gg <- gg + scale_fill_manual(values = trophic_group_colors)
  }
  
  return(gg)
  
}



# Barplots for samples ----------------------------------------------------

barplot_samples <- function(df, variable) {
  
  variable_to_use <- variable
  
  # variable_to_use = "ecosystem" # For testing
  # df <- samples
  
  # For depth only use the first 250 m
  
  if (variable == "depth") {
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
  
  
  if (variable == "temperature") {
    if (length(unique(df$temperature)) > 1) {
      df <- df %>%
        mutate(temperature =  fct_rev(cut_width(temperature, width=5, boundary=0)))
    } else {
      df <- df %>%
        mutate(temperature =  as.factor(temperature))
    }
  }
  
  if (variable == "salinity") {
    if (length(unique(df$salinity)) > 1) {
      df <- df %>%
        mutate(salinity =  fct_rev(cut_width(salinity, width=5, boundary=0)))
    } else {
      df <- df %>%
        mutate(salinity =  as.factor(salinity))
    }
  }
  
  if (variable == "latitude") {  
    if (length(unique(df$latitude)) > 1) {
      df <- df %>%
        mutate(latitude =  fct_rev(cut_width(latitude, width=20, boundary=0)))
    } else {
      df <- df %>%
        mutate(latitude =  as.factor(latitude))
    }
  }
  

  df <- df %>% 
    select(file_code, any_of(c(variable_to_use))) %>% 
    distinct() %>% 
    group_by(across(any_of(variable_to_use))) %>% 
    count() %>% 
    ungroup()
  

  # cat(variable_to_use, "\n")
  # print(df)
  
  gg <- df  %>% 
    ggplot(aes(y= fct_rev(.data[[variable_to_use]]),
               x=n)) +
    xlab("Number of samples")   + ylab(variable_to_use) +
      geom_col(fill = "grey50")+
    # geom_text(aes( label = ifelse(n > 200, n, "")),
    #           nudge_x = - 20, 
    #           hjust = 1,
    #           size = 5,
    #           color = "white") +
    geom_text(aes( label = n),
              nudge_x = + 0.015* max(df$n), 
              hjust = 0,
              size = 3.5,
              color = "black") +
    
    theme_classic()  +
    labs(title = str_replace(variable_to_use, "_", " ")) +
    scale_x_continuous(expand = expansion(mult = c(0,0.15)))
             
  
  print(gg)
  

  return(gg)
  
}



