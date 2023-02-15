library(tercen)
library(dplyr)

get1D <- function(df, n_bins) {
  x <- df$.y
  hs <- hist(x, breaks = n_bins, plot = FALSE)
  df_out <- as.data.frame(hs[c("counts", "density", "mids")])
  df_out$bins <- levels(cut(hs$breaks, nrow(df_out)))
  df_out$.ci <- df$.ci[1]
  df_out$.ri <- df$.ri[1]
  df_out$counts <- as.numeric(df_out$counts)
  return(df_out)
}

get2D <- function(df, n_bins) {
  x <- df$.x
  y <- df$.y
  
  x.bin <- pretty(x, n_bins)
  y.bin <- pretty(y, n_bins)
  
  freq <-  as.data.frame(table(findInterval(x, x.bin), findInterval(y, y.bin)))
  freq[, 1] <- as.numeric(freq[, 1])
  freq$x_bin <- cut(x.bin, length(x.bin) - 1)[freq[, 2]]
  freq[, 2] <- as.numeric(freq[, 2])
  freq$y_bin <- cut(y.bin, length(y.bin) - 1)[freq[, 2]]
  colnames(freq)[1:3] <- c("x_bin_id", "y_bin_id", "count")
  freq$count <- as.numeric(freq$count)
  freq$density <- freq$count / sum(freq$count, na.rm = TRUE)
  
  df_out <- freq
  df_out$.ci <- df$.ci[1]
  df_out$.ri <- df$.ri[1]
  
  return(df_out)
}


ctx <- tercenCtx()

n_bins <- 30
if(!is.null(ctx$op.value("n_bins"))) n_bins <- as.integer(ctx$op.value("n_bins"))

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
