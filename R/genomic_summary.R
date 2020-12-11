#' Genomic Summary
#'
#' This function provides some basic information on the data set. It first
#' provides a table of total number of significantly deferentially expressed
#' genes. Then in runs a binomial exact test on those numbers. Lastly, it
#' returns a basic histogram of log2 (fold change) values in order for the
#' user to gain an understanding of the skew.
#'
#' @param data name of data set
#' @param log2 name of column with log2 values
#' @param sig column name in data set that states changes were significant
#' @import huxtable
#' @export
#' @return NULL
#' @examples
#' genomic_summary(data = gendata, log2 = "log2.fold_change.", sig = "significant")
genomic_summary <- function(data, log2, sig){
  sm <- subset.data.frame(data, data[[sig]] == "yes",
                          select = log2)

  sm$diffexp[sm[[log2]] > 0 ] <- "up"
  sm$diffexp[sm[[log2]] < 0 ] <- "down"

  counts <- c(count(sm)[[1]], table(sm$diffexp)[[1]], table(sm$diffexp)[[2]])

  summary <- data.frame("total"=counts[1],"upregulated"=counts[3],"downregulated"=counts[2])
  h.summary <- hux(summary)
  bold(h.summary)[1,]           <- TRUE
  bottom_border(h.summary)[1,]  <- 0.4
  align(h.summary)[,2]          <- 'right'
  right_padding(h.summary)      <- 10
  left_padding(h.summary)       <- 10
  width(h.summary)              <- 0.35

  print(h.summary)

  bin <- binom.test(counts[2], counts[1])
  print(bin)

  hist(sm[[log2]],
       xlab = "log2(fold change)",
       main = "Histogram of log2(fold_change) Gene Expression",
       col = "blue")

}


