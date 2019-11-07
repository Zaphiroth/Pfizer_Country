
quadrant <- function(data, seg, chc) {
  
  data1 <- data %>% 
    filter(segment == seg)
  
  if (chc == "no") {
    data2 <- data.frame(
      "key" = c("City#", "Terminal#", "Current Potential Con%", "Current TTH Con%", "Share% (TTH/Potential)", "Share% (TTH/Mol)"),
      "value" = c(length(unique(data1$city)),
                  sum(data1$terminal, na.rm = TRUE),
                  sum(data1$potential_con_2018, na.rm = TRUE),
                  sum(data1$internal_con, na.rm = TRUE),
                  sum(data1$internal, na.rm = TRUE)/sum(data1$potential_2018, na.rm = TRUE),
                  sum(data1$internal, na.rm = TRUE)/sum(data1$molecule_2018, na.rm = TRUE)),
      stringsAsFactors = FALSE
    ) %>% 
      mutate_all(function(x) {ifelse(is.na(x) | is.nan(x),
                                     0,
                                     x)}) %>% 
      mutate(value = ifelse(key %in% c("Current Potential Con%", "Current TTH Con%", "Share% (TTH/Potential)", "Share% (TTH/Mol)"),
                            paste0(round(value*100, 1), "%"),
                            format(round(value), big.interval = 3, big.mark = ",")))
    
  } else if (chc == "yes") {
    data2 <- data.frame(
      "key" = c("City#", "Terminal#", "Current Potential Con%", "Current TTH Con%", "Share% (TTH/Potential)", "Share% (TTH/Mol)"),
      "value" = c(length(unique(data1$city)),
                  sum(data1$terminal, na.rm = TRUE),
                  sum(data1$potential_chc_con_2018, na.rm = TRUE),
                  sum(data1$internal_con, na.rm = TRUE),
                  sum(data1$internal, na.rm = TRUE)/sum(data1$potential_chc_2018, na.rm = TRUE),
                  sum(data1$internal, na.rm = TRUE)/sum(data1$molecule_2018, na.rm = TRUE)),
      stringsAsFactors = FALSE
    ) %>% 
      mutate_all(function(x) {ifelse(is.na(x) | is.nan(x),
                                     0,
                                     x)}) %>% 
      mutate(value = ifelse(key %in% c("Current Potential Con%", "Current TTH Con%", "Share% (TTH/Potential)", "Share% (TTH/Mol)"),
                            paste0(round(value*100, 1), "%"),
                            format(round(value), big.interval = 3, big.mark = ",")))
    
  } else {
    stop("Function CHC Error.")
  }
  
  data2[data2 == 0 | data2 == "0" | data2 == "0%"] <- "-"
  
  return(data2)
}
