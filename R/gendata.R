#' Genomics Lab Data
#'
#' This dataset comes from analysed RNAseq data and has been
#' adjusted and edited to not conflict with copyright of the lab.
#' It contains the following parameter values for 8651 genes.
#'
#' @format A data frame with 8651 rows and 9 variables:
#' \describe{
#'   \item{\code{gene}}{character. gene names}
#'   \item{\code{sample_1}}{character. description of trial in this case this was the control}
#'   \item{\code{sample_2}}{character. description of trial in this case the experiment (change in independent variable)}
#'   \item{\code{value_1}}{double. expression levels of the gene in control trial}
#'   \item{\code{value_2}}{double. expression levels of gene in experimental trial}
#'   \item{\code{log2.fold_change.}}{double. values of the change in expression usually calculated using the log2 of the experiment divided by the control}
#'   \item{\code{p_value}}{double. p-value of whether the change in expression level is significant}
#'   \item{\code{q_value}}{double. adjusted p-value for the false discovery rate (FDR)}
#'   \item{\code{significant}}{character. yes/no indicating significant/non-significant change in expression}
#' }
"gendata"
