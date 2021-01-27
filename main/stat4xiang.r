#' @name stat4xiang
#' @author Xiang LI <lixiang117423@@gmail.com>
#'
#' @title Statistical only for XiangLi
#' @description
#' \code{RiceIDConverter} Statistical only for Xiang LI
#'
#' @param df input data.frame
#' @param value Colnames of value
#' @param group Colnames of group
#' @param method Method of statistics
#' @param level Statistical inspection level
#
#' @examples
#' res <- stat4xiang(df = iris,
#'                   value = 'Sepal.Length', 
#'                   group = 'Species', 
#'                   method = 'anova', 
#'                   level = 0.99)
#
#' @export
#'
#' @return Return a vector or a datafram
library(multcomp)
library(pgirmess)

stat4xiang <- function(df, value, group, method, level){
  
  df_sub <- df[,c(value, group)]
  colnames(df_sub) <- c('value','group')
  
  if (length(unique(df_sub$group)) == 2 & method == 'anova') {
    warning('Method should be t.test or wilcox')
  }
  if (length(unique(df_sub$group)) == 2 & method == 'kruskal') {
    warning('Method should be t.test or wilcox')
  }
  if (length(unique(df_sub$group)) > 2 & method == 't.test') {
    warning('Method should be anova or kruskal')
  }
  if (length(unique(df_sub$group)) > 2 & method == 'wilcox') {
    warning('Method should be anova or kruskal')
  }

  # mean, sd, se and number
  number <- as.data.frame(table(df_sub$group))
  colnames(number) <- c('group','number')
  df_sub <- merge(df_sub, number, by = 'group')
  
  mean_value <- aggregate(df_sub$value, by = list(df_sub$group), FUN = mean)
  colnames(mean_value) <- c('group','mean')
  
  sd_value <- aggregate(df_sub$value, by = list(df_sub$group), FUN = sd)
  colnames(sd_value) <- c('group','SD')
  
  temp_df <- merge(mean_value, sd_value, by = 'group')
  df_sub <- merge(df_sub, temp_df, by = 'group')
  df_sub$SE <- df_sub$SD / sqrt(df_sub$number)

  df_sub$mean <- round(df_sub$mean, 4)
  df_sub$SD <- round(df_sub$SD, 4)
  df_sub$SE <- round(df_sub$SE, 4)
  

  # statistical analysis
  if (length(unique(df_sub$group)) == 2) {
    if (method == 't.test') {
      fit <- t.test(value ~ group, data = df_sub)
      pvalue <- fit[["p.value"]]
      pvalue <- ifelse(pvalue < 0.001,'<0.001',round(pvalue,4))
      signif <- ifelse(pvalue < 0.001,'***',
                       ifelse(pvalue > 0.001 & pvalue < 0.01, '**',
                              ifelse(pvalue > 0.05, 'NS','*')))
    }
    if (method == 'wilcox') {
      fit <- wilcox.test(value ~ group, data = df_sub)
      pvalue <- fit[["p.value"]]
      pvalue <- ifelse(pvalue < 0.001,'<0.001',round(pvalue,4))
      signif <- ifelse(pvalue < 0.001,'***',
                       ifelse(pvalue > 0.001 & pvalue < 0.01, '**',
                              ifelse(pvalue > 0.05, 'NS','*')))
    }
    # dataframe for statistical
    sig <- data.frame(group = unique(df_sub$group),
                      method = method,
                      level = level,
                      pvalue = pvalue,
                      signif = c(signif,''))

  }
  if (length(unique(df_sub$group)) > 2) {
    if (method == 'anova') {
      fit <- aov(value ~ group, data = df_sub)
      pvalue <- summary(fit)[[1]][["Pr(>F)"]][1]
      pvalue <- ifelse(pvalue < 0.001,'<0.001',round(pvalue,4))
      tuk <- glht(fit, linfct = mcp(group = 'Tukey'))
      signif <- cld(tuk, level = level, ddecreasing = TRUE)[["mcletters"]][["Letters"]]
      signif <- as.data.frame(signif)
      colnames(signif) = 'signif'
      signif$group <- rownames(signif)
      signif$method <- method
      signif$pvalue <- pvalue
      signif$level <- level

      sig <- signif[,c('group','method','level','pvalue','signif')]

    }
    if (method == 'kruskal') {
      fit <- kruskal.test(value ~ group, data = df_sub)
      pvalue <- fit[["p.value"]]
      if (pvalue < 0.05) {
        fit_2 <- as.data.frame(kruskalmc(df_sub$value, df_sub$group, probs = 1-level))
        signif <- as.data.frame(fit_2)
        signif$statistic <- rownames(signif)
        colnames(signif)[2] <- 'group_comp'
        signif$group <- unique(df_sub$group)

        sig <- data.frame(group = unique(df_sub$group),
                          method = method,
                          level = level,
                          pvalue = ifelse(pvalue < 0.001,'<0.001',round(pvalue,4)))
        sig <- merge(signif, sig, by = 'group')
      }else{
        sig <- data.frame(group = unique(df_sub$group),
                          method = method,
                          level = level,
                          pvalue = ifelse(pvalue < 0.001,'<0.001',round(pvalue,4)),
                          signif = 'NS')
      }
    }
  }
  results <- merge(df_sub,sig, by = 'group', all.x = TRUE)
  return(results)
}
