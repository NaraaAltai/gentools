#' Import dataset
#'
#' import_data() lets the user import a dataset from given file path.
#'
#' @details
#' This function lets the user import dataset into R from Stata, SPSS, SAS,
#' excel, and delimited text files and returns a dataframe.
#'
#' @param file Path to data file.
#' @param ... Additional arguments.
#'
#' @export
#' @return A dataframe containing the dataset
#' @examples
#' \dontrun{import("./filename.csv")}
#'
#' @importFrom haven read_sas read_stata read_spss
#' @importFrom readxl read_excel
#' @importFrom vroom vroom
#' @importFrom tools file_ext
import_data <- function(file, ...){

  # if no file specified, prompt user
  if(missing(file))
    file <- file.choose()


  # get file info

  file <- tolower(file)
  basename <- basename(file)
  extension <- tools::file_ext(file)


  # import dataset

  df <- switch(extension,
               "sas7bdat" = haven::read_sas(file, ...),
               "dta" = haven::read_stata(file, ...),
               "sav" = haven::read_spss(file, ...),
               "xlsx" = readxl::read_excel(file, ...),
               "xls" = readxl::read_excel(file, ...),
               "tabular" = read.csv(file, ...),
               vroom::vroom(file, ...)
  )

  # return data frame
  return(df)
}

