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


get1D_labs <- function(df, n_bins) {
  x <- df$.y
  hs <- hist(x, breaks = n_bins, plot = FALSE)
  
  df_out <- as.data.frame(hs[c("counts", "density", "mids")]) %>% 
    mutate(.y_bin = 1:nrow(.))
  
  df_out$bins <- levels(cut(hs$breaks, nrow(df_out)))
  df$.y_bin <- cut(df$.y, breaks = hs$breaks, labels = FALSE, include.lowest = TRUE)
  
  df_out$.ci <- df$.ci[1]
  df_out$.ri <- df$.ri[1]
  df_out$counts <- as.numeric(df_out$counts)
  df_t = merge(df_out, df[, unlist(c(".ci", ".ri", ctx$labels, ".y_bin"))])
  
  return(df_t)
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

get2D_labs <- function(df, n_bins) {
  x <- df$.x
  y <- df$.y
  
  # Define bin intervals for x and y
  x.bin <- pretty(x, n_bins)
  y.bin <- pretty(y, n_bins)
  
  # Get frequency counts for each bin combination
  freq <- as.data.frame(table(findInterval(x, x.bin), findInterval(y, y.bin)))
  freq[, 1] <- as.numeric(freq[, 1])
  freq[, 2] <- as.numeric(freq[, 2])
  
  # Assign labels for each bin for easy identification
  freq$x_bin <- cut(x.bin, length(x.bin) - 1)[freq[, 1]]
  freq$y_bin <- cut(y.bin, length(y.bin) - 1)[freq[, 2]]
  
  # Rename and add density information
  colnames(freq)[1:3] <- c("x_bin_id", "y_bin_id", "count")
  freq$count <- as.numeric(freq$count)
  freq$density <- freq$count / sum(freq$count, na.rm = TRUE)
  
  # Assign each point in df to a bin
  df$x_bin_id <- findInterval(df$.x, x.bin, all.inside = TRUE)
  df$y_bin_id <- findInterval(df$.y, y.bin, all.inside = TRUE)
  
  # Merge df with bin information
  df_out <- merge(freq, df, by = c("x_bin_id", "y_bin_id"))
  
  # Add metadata columns
  df_out$.ci <- df$.ci[1]
  df_out$.ri <- df$.ri[1]
  
  return(df_out)
}
