library(tercen)
library(dplyr)

source("./utils.R")

ctx <- tercenCtx()

n_bins <- 30
if(!is.null(ctx$op.value("n_bins"))) n_bins <- as.integer(ctx$op.value("n_bins"))

if(length(ctx$labels) > 0) {
  
  if(length(ctx$labels) > 1) stop("A single label factor must be provided.")

  if(ctx$hasXAxis | ctx$isPairwise) {
    df <- ctx$select(c(".x", ".y", ".ci", ".ri", ctx$labels))
    
    df2 <- df %>%
      group_by(.ci, .ri) %>%
      do(get2D_labs(., n_bins))

  } else {
    df <- ctx$select(c(".y", ".ci", ".ri", ctx$labels))
    
    df2 <- df %>%
      group_by(.ci, .ri) %>%
      do(get1D_labs(., n_bins))
  }
  
  df2 %>% 
    ctx$addNamespace()  %>%
    ctx$save()
  
} else {
  if(ctx$hasXAxis | ctx$isPairwise) {
    df <- ctx %>% 
      select(.x, .y, .ci, .ri) %>% 
      group_by(.ci, .ri) %>%
      do(get2D(., n_bins)) %>%
      ctx$addNamespace() %>%
      ctx$save()
  } else {
    df <- ctx %>% 
      select(.y, .ci, .ri) %>%
      group_by(.ci, .ri) %>%
      do(get1D(., n_bins)) %>%
      ctx$addNamespace() %>%
      ctx$save()
  }
}


