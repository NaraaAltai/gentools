#' Genomic Tables
#'
#' Generates tables of gene names, log2 values, p-values. One can filter for genes
#' that were up or down regulated, or the default is neither. Function will prompt
#' for the number of rows you wish to view.
#'
#' @param data name of data set
#' @param U.D.diffexp pick either "up" or "down" to filter for genes in those categories
#' @param gene.n column of data set with gene names
#' @param log2 column of data set with log2 values
#' @param pval column of data set with p-values
#' @param sig column name in data set that states changes were significant
#'
#' @import dplyr huxtable
#' @export
#'
#' @return returns a huxtable
#' @examples
#' genomic_tables(data = gendata, U.D.diffexp = "up", gene.n = "gene",
#'        log2 = "log2.fold_change.", pval = "p_value", sig = "significant")
#' genomic_tables(data = gendata, U.D.diffexp = "down", gene.n = "gene",
#'        log2 = "log2.fold_change.", pval = "p_value", sig = "significant")
#' genomic_tables(data = gendata, U.D.diffexp = "neither", gene.n = "gene",
#'        log2 = "log2.fold_change.", pval = "p_value", sig = "significant")
#' genomic_tables(data = gendata, gene.n = "gene", log2 = "log2.fold_change.",
#'         pval = "p_value", sig = "significant")
genomic_tables <- function(data, U.D.diffexp = c("neither","up","down"), gene.n, log2, pval, sig){

  sub.test <- subset.data.frame(data, data[[sig]] != "no")

  sub.test$diffexpressed[sub.test[[log2]] > 0] <- "up"
  sub.test$diffexpressed[sub.test[[log2]] < 0] <- "down"


  num.rows <- readline(prompt = "How many rows of data would like displayed? ")
  as.numeric(num.rows)

  columns <- c(gene.n, log2,pval)

  if(U.D.diffexp == "up"){
    up <- subset.data.frame(sub.test, diffexpressed == "up", select = columns)
    up <- up[order(-up$log2.fold_change.), ]
    up <- up[1:num.rows,]
    h.up <- hux(up)
    bold(h.up)[1,]           <- TRUE
    bottom_border(h.up)[1,]  <- 0.4
    align(h.up)[,2]          <- 'right'
    right_padding(h.up)      <- 10
    left_padding(h.up)       <- 10
    width(h.up)              <- 0.35
    return(h.up)
  }
  if(U.D.diffexp == "down"){
    down <- subset.data.frame(sub.test, diffexpressed == "down",select = columns)
    down <- down[order(down$log2.fold_change.), ]
    down <- down[1:num.rows,]
    h.down <- hux(down)
    bold(h.down)[1,]           <- TRUE
    bottom_border(h.down)[1,]  <- 0.4
    align(h.down)[,2]          <- 'right'
    right_padding(h.down)      <- 10
    left_padding(h.down)       <- 10
    width(h.down)              <- 0.35
    return(h.down)
  }
  if (U.D.diffexp == "neither"){
    columns <- c(columns, "diffexpressed")
    sub.test <- subset.data.frame(sub.test, select = columns)
    sub.test <- sub.test[1:num.rows,]
    h <- hux(sub.test)
    bold(h)[1,]           <- TRUE
    bottom_border(h)[1,]  <- 0.4
    align(h)[,2]          <- 'right'
    right_padding(h)      <- 10
    left_padding(h)       <- 10
    width(h)              <- 0.35
    return(h)
  }
}
