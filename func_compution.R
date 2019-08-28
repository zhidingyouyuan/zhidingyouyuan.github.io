


split_col <- function(dat, sep = '-') {
  n <- seq(1, ncol(dat) * 2, 2)
  
  for (i in n) {
    # i=3
    x <-  separate(dat, col = i,
                   c(
                     paste('T', (i + 1) / 2, '_', 1, sep = ''),
                     paste('T', (i + 1) / 2, '_', 2, sep = '')
                   ),
                   sep = sep)
    dat <- x
  }
  return(dat)
}


dim1_MAP <- function(dat,
                     data_tran,
                     type = 1,
                     item = 1) {
  if (item == 1) {
    cacul_dim <- data_tran$partial
  } else{
    cacul_dim <- data_tran$allin
  }
  
  dimension <-
    na.omit(data_tran$wdrank) # 维度顺序为导出分数表格的维度顺序，需在题维对应文件里预先设置
  res_dim <- as.data.frame(matrix(0, nrow(dat), length(dimension)))
  colnames(res_dim) <- dimension
  for (i in 1:length(dimension)) {
    coldim <- which(cacul_dim == dimension[i])
    res_dim[, dimension[i]] <- rowMeans(dat[, coldim])
  }
  
  prr <- res_dim
  prr[prr >= 0.8] <- 10
  prr[prr >= 0.6 & prr < 0.8] <- 9
  prr[prr >= 0.4 & prr < 0.6] <- 8
  prr[prr >= 0.2 & prr < 0.4] <- 7
  prr[prr >= 0 & prr < 0.2] <- 6
  prr[prr >= -0.2 & prr < 0] <- 5
  prr[prr >= -0.4 & prr < -0.2] <- 4
  prr[prr >= -0.6 & prr < -0.4] <- 3
  prr[prr >= -0.8 & prr < -0.6] <- 2
  prr[prr < -0.8] <- 1
  
  if (type == 10) {
    return(prr)
  } else{
    return(res_dim)
  }
  
}

to_zsocre <- function(res_dim, norm, data_tran) {
  name <- norm %>% colnames() %>% grep("24", .)
  trait_name <- na.omit(data_tran$wdrank)
  trait_norm <-
    data.frame(na.omit(norm[, name]), row.names = na.omit(norm[, name[1]]))
  trait_M <- na.omit(trait_norm[trait_name, 2])
  trait_SD <- na.omit(trait_norm[trait_name, 3])
  dim1_zscore <-
    as.data.frame(scale(res_dim, center = trait_M, scale = trait_SD))
  return(dim1_zscore)
}

tran_map <- function(dat, tran = NULL) {
  n <- seq(1, 214, 2)
  res1 <- matrix(NA, nrow(dat), 3)
  for (i in n) {
    res <- matrix(NA, nrow(dat), 3)
    res[which(dat[, i] == 'A'), 1] <- 1
    res[which(dat[, i] == 'B'), 2] <- 1
    res[which(dat[, i] == 'C'), 3] <- 1
    
    z = i + 1
    res[which(dat[, z] == 'A'), 1] <- -1
    res[which(dat[, z] == 'B'), 2] <- -1
    res[which(dat[, z] == 'C'), 3] <- -1
    
    res[which(is.na(res))] <- 0
    res1 <- cbind(res1, res)
  }
  res1 <- as.data.frame(res1[, -c(1:3)])
  colnames(res1) <- rep(c('A', 'B', 'C'), 107)
  
  if (is.null(tran) == FALSE)
    res1[, tran] <- res1[, tran] * -1
  return(res1)
  
}
