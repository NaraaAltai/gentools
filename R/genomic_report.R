#' Genomics Lab Report
#'
#' genomic_report() creates a a full laboratory report
#'
#' @param title a string, the title of the report. Defaults to "Genetics Lab Report"
#' @param author a string, the name of the author. Defaults to system username
#' @param date a string. Defaults to the system date
#'
#' @return file name of the outputted report, which is a word-document
#' @import rmarkdown
#' @export
#' @examples
#' genomic_report("My Report", "John Doe")
genomic_report <- function(title="Genetics Lab Report",
                       author=Sys.info()[["user"]],
                       date = format(Sys.time(), '%B %d, %Y')){

  # read template file and modify
  report <- readLines("./inst/report_template.txt")

  # report <- gsub(pattern = "replacedata", replacement = data, report, fixed = TRUE)
  report <- gsub(pattern = "replacetitle", replacement = title, report, fixed = TRUE)
  report <- gsub(pattern = "replaceauthor", replacement = author, report, fixed = TRUE)
  report <- gsub(pattern = "replacedate", replacement = date, report, fixed = TRUE)

  # output template and render
  tf <- tempfile(fileext = ".Rmd")
  file.create("LabReport.docx", overwrite=TRUE)
  to <- "./LabReport.docx"
  writeLines(report, tf)

  # generate the report
  render(input=tf,
         output_format="word_document",
         output_dir = "./",
         output_file = "LabReport.docx")
}


