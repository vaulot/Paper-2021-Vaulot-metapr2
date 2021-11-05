# Table Latex using Kable Extra -------------------------------------------------


kable_latex <- function(table, 
                        caption, 
                        label, 
                        colnames, 
                        align, 
                        digits = 2, 
                        scale_down = FALSE,
                        longtable = FALSE) {
  
  options(knitr.kable.NA = "")
  
  if(longtable) scale_down = FALSE
  
  latex_options = c("hold_position")
  if(scale_down) latex_options = c("scale_down", latex_options)
  if(longtable) latex_options = c("repeat_header", latex_options)

# Remove problematic characters  
  table <- table %>% 
    mutate_all(~ str_replace_all(.x, "_", "\\\\_"))
  
  
  kbl( table, 
       caption = caption, 
       label=label, 
       col.names = colnames,
       align = align,
       digits = digits,
       linesep = "",
       booktabs = TRUE, 
       format = "latex", 
       escape = F,
       longtable = longtable) %>% 
    kable_styling(latex_options = latex_options,
                  position="center",
                  repeat_header_method = "replace")  
  
}


# Statistics function to apply --------------------------------------------


stats <- list(
  min = ~min(.x, na.rm = TRUE), 
  max = ~max(.x, na.rm = TRUE),
  mean =~mean(.x, na.rm = TRUE),
  sd = ~sd(.x, na.rm = TRUE),
  n = ~n()
)
