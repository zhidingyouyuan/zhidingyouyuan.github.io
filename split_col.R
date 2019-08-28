#' split data by '-'.
#'
#' @param dat 仅题目数据文件，A-B
#' @return 分列后数据，A B
#' @examples
split_col <- function(dat,sep='-') {
  n <- seq(1,ncol(dat)*2,2)

  for (i in n) {
    x <-  separate(dat,col = i,
                   c(paste('T',(i+1)/2,'_',1,sep = ''),paste('T',(i+1)/2,'_',2,sep = '')),
                   sep = sep)
    dat <- x
  }
 return(dat)
}
