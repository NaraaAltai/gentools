#' Genomics Lab Report
#'
#' genomic_report() take a dataset, report title, author name and date and creates
#' a full laboratory report with Subsection titles and placeholder text. By default, it generates
#' a volcano, violin, and scatterplot and a table from the dataset and adds them to the output document
#' with figure/table caption. The output of this function is the name of the output report which
#' is in a word document format.
#'
#' @param data dataset used to generate the report
#' @param title a string, the title of the report. Defaults to "Genetics Lab Report"
#' @param author a string, the name of the author. Defaults to system username
#' @param date a string. Defaults to the system date
#' @param save_to directory in which the report is going to be saved in, defaults to current dir
#'
#' @return file name of the outputted report, which is a word-document
#' @import rmarkdown
#' @export
#' @examples
#' \dontrun{genomic_report("My Lab Report", "Full Name")}
genomic_report <- function(data,
                            title="Genetics Lab Report",
                            author=Sys.info()[["user"]],
                            date = format(Sys.time(), '%B %d, %Y'),
                            save_to = "./"){

  # read template file and modify
  report <- readLines(system.file("report_template.txt", package = "gentools"))

  report <- gsub(pattern = "xxxdataxxx", replacement = data, report, fixed = TRUE)
  report <- gsub(pattern = "replacetitle", replacement = title, report, fixed = TRUE)
  report <- gsub(pattern = "replaceauthor", replacement = author, report, fixed = TRUE)
  report <- gsub(pattern = "replacedate", replacement = date, report, fixed = TRUE)


  # output template and render
  tf <- tempfile(fileext = ".Rmd")
  file.create("GenomicLabReport.docx", overwrite=FALSE)
  to <- "./GenomicLabReport.docx"
  writeLines(report, tf)

  # generate the report
  render(input=tf,
         output_format="word_document",
         output_dir = "./",
         output_file = "GenomicLabReport.docx")
  my_file_rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    file.rename(from = from,  to = to)
  }

  path_from <- paste('./', "GenomicLabReport.docx", sep='')
  to_dir <- paste(save_to, "GenomicLabReport.docx", sep='')
  my_file_rename(from_path, to_path)
}


