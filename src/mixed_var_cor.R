mixed_var_cor <- function(df){
  require(lsr)
  require(reshape2)
  require(dplyr)
  
  cor_mixed <- function(var1, var2){
    if(class(var1) %in% c("integer", "numeric") && class(var2) %in% c("integer", "numeric")){
      cor_val <- stats::cor(var1, var2, use = "pairwise.complete.obs")}
    if(class(var1) %in% c("integer", "numeric") && class(var2) %in% c("factor", "character")){
      cor_val <- sqrt(summary(stats::lm(var1~as.factor(var2)))[["r.squared"]])}
    if(class(var2) %in% c("integer", "numeric") && class(var1) %in% c("factor", "character")){
      cor_val <- sqrt(summary(stats::lm(var2~as.factor(var1)))[["r.squared"]])}
    if(class(var1) %in% c("factor", "character") && class(var2) %in% c("factor", "character")){
      cor_val <- lsr::cramersV(var1, var2, simulate.p.value = TRUE)}
    return(cor_val)
  }
  
  cor_table <- as.data.frame(matrix(ncol = 3,nrow = 0))
  names(cor_table) <- c("Var1", "Var2", "Correlation")
  for(i in 1:ncol(df)){
    for(j in i:ncol(df)){
      cor_table_temp <- c(names(df)[i], names(df)[j], cor_mixed(df[[i]], df[[j]]))
      cor_table_temp <- as.data.frame(t(as.data.frame(cor_table_temp)))
      rownames(cor_table_temp) <- NULL
      names(cor_table_temp) <- c("Var1", "Var2", "Correlation")
      cor_table <- rbind(cor_table, cor_table_temp)
    }
  }
  
  cor_table <- as.data.frame(t(dcast(cor_table, Var1~Var2, value.var = "Correlation")))
  cor_table <- mutate_all(cor_table, as.character)
  var_names <- as.character(unname(cor_table[1,]))
  names(cor_table) <- var_names
  cor_table <- cor_table[-1,]
  for(i in 1:ncol(cor_table)){
    cor_table[i,] <- cor_table[,i]
  }
  
  cor_table <- mutate_all(cor_table, as.numeric)
  rownames(cor_table) <- var_names
  cor_table <- as.matrix(cor_table)
  
  return(cor_table)
}
cor2Pval <- function(cor_val,n){
  t_val <- cor_val/sqrt((1-(cor_val^2))/(n-2))
  p_val <- 2*pt(t_val, n-2, lower.tail = F)
  # p_val <- round(p_val, 10)
  return(p_val)
}