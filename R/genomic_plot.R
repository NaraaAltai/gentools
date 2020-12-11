#' Genomic Plot
#'
#' Takes expression levels of genes found in genomic data sets, and plots them
#' as either a scatter plot, a volcano plot, or a violin plot. While both volcano
#' and violin plots require only the gene names, log2 values, and p values, the
#' scatter plot requires the expression values from the controls, the experiments, and
#' the column stating whether they are significant.
#'
#' @param plot.type can be either "scatter", "volcano", or " violin".
#' @param data data set of genomic data
#' @param interact if TRUE, the function returns an interactive plotly graph for either
#' scatter or volcano plot. if FALSE, the function returns a static plot.
#' @param gene.n column name in data set with gene names (character).
#' @param c.val column name in data set with expression levels in control (numeric).
#' @param e.val column name in data set with expression levels in experiment (numeric).
#' @param log2 column name in data set with calculated log2 values (numeric).
#' @param pval column name in data set with calculated p-values (numeric).
#' @param sig column name in data set that states changes were significant (character).
#'
#' @import ggplot2 ggrepel plotly ggpubr
#' @export
#'
#' @return Returns a ggplot defined as a variable named s (scatter plot), vol (volcano plot),
#' or vio (violin plot).
#'
#' @examples
#'  genomic_plot(plot.type = "scatter", interact = FALSE, data = gendata, gene.n = "gene" ,c.val = "value_1",
#'                     e.val = "value_2",sig = "significant")
#'  genomic_plot(plot.type = "volcano", data = gendata, interact = FALSE, gene.n = "gene",
#'                     log2 = "log2.fold_change.", pval = "p_value")
#'  genomic_plot(plot.type = "violin", data = gendata, interact=FALSE, gene.n = "gene",
#'                     log2 = "log2.fold_change.", pval = "p_value")
genomic_plot <- function(plot.type = c("scatter","volcano","violin"), interact = TRUE, data, gene.n, c.val,
                         e.val, log2, pval, sig){

  if(plot.type != "scatter"){
    data$diffexpressed <- "NO"
    data$diffexpressed[data[[log2]] > 0.6 & data[[pval]] < 0.05] <- "UP"
    data$diffexpressed[data[[log2]] < -0.6 & data[[pval]] < 0.05] <- "DOWN"

    data$label <- NA
    data$label[data$diffexpressed != "NO"] <- data[[gene.n]][data$diffexpressed != "NO"]
  }
  if(plot.type == "scatter"){
    x.axis <- readline(prompt = "What is the title of the x-axis? ")
    y.axis <- readline(prompt = "What is the title of the y-axis? ")
    corr <- as.character(round(cor(data[[e.val]], data[[c.val]]), 2))

    s <- ggplot(data = data, aes(x = .data[[c.val]], y = .data[[e.val]], col = .data[[sig]], label = .data[[gene.n]]))+
      geom_point()+
      theme_minimal() +
      scale_color_manual(values=c("black", "red")) +
      geom_vline(xintercept=0) +
      geom_hline(yintercept=0) +
      geom_smooth(method = "lm", se = F, aes(group=1))+
      xlab(x.axis)+
      ylab(y.axis)+
      ggtitle(paste0("r = ", corr))
    if(interact == TRUE){
      return(ggplotly(s))
    }else{
      return(s)
    }
  }
  if(plot.type == "volcano"){
    x.axis <- "-log10(p value)"
    y.axis <- "log2 Fold Change"

    vol <- ggplot(data=data, aes(x=.data[[log2]], y=-log10(.data[[pval]]), col=diffexpressed, label=label)) +
      geom_point() +
      theme_minimal() +
      scale_color_manual(values=c("blue", "black", "red")) +
      geom_vline(xintercept=c(-0.6, 0.6), col="red") +
      geom_hline(yintercept=-log10(0.05), col="red") +
      xlim(-15,15)

    if(interact==TRUE){
      return(ggplotly(vol, tooltip = "label"))
    }else{
      return(vol)
    }

  }
  if(plot.type == "violin"){
    x.axis <- "log2 Fold Change"
    y.axis <- "Differentially Expressed Genes"

    sub.test <- subset.data.frame(data, label != is.na(NA))
    vio <- ggplot(sub.test, aes(x = diffexpressed, y = .data[[log2]], fill = diffexpressed))+
      geom_violin(show.legend = F)+
      scale_fill_manual(values = c("blue","red"))+
      theme_minimal()+
      xlab (x.axis)+
      ylab(y.axis)

    return(vio)
  }
}

