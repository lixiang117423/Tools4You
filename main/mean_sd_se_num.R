mean_sd_se_num <- function(df){
  #colnames(df)[1] <- 'value'
  # mean, sd, se and number
  df_sub <- dplyr::select(df,c('value', 'group'))
  #colnames(df_sub) <- c('value','group')
  
  number <- as.data.frame(table(df_sub$group))
  colnames(number) <- c('group','number')
  
  df_sub <- merge(df_sub, number, by = 'group')
  
  sum_value <- aggregate(df_sub$value, by = list(df_sub$group), FUN = sum)
  colnames(sum_value) <- c('group','sum')
  
  mean_value <- aggregate(df_sub$value, by = list(df_sub$group), FUN = mean)
  colnames(mean_value) <- c('group','mean')
  
  sd_value <- aggregate(df_sub$value, by = list(df_sub$group), FUN = sd)
  colnames(sd_value) <- c('group','SD')
  
  temp_df <- merge(sum_value, mean_value, by = 'group')
  temp_df <- merge(temp_df, sd_value, by = 'group')
  df_sub <- merge(df_sub, temp_df, by = 'group')
  df_sub$SE <- df_sub$SD / sqrt(df_sub$number)
  
  df_sub$mean <- round(df_sub$mean, 4)
  df_sub$SD <- round(df_sub$SD, 4)
  df_sub$SE <- round(df_sub$SE, 4)
  
  
  return(df_sub)
  
}
