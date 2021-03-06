% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genomic_report.R
\name{genomic_report}
\alias{genomic_report}
\title{Genomics Lab Report}
\usage{
genomic_report(
  data,
  title = "Genetics Lab Report",
  author = Sys.info()[["user"]],
  date = format(Sys.time(), "\%B \%d, \%Y"),
  save_to = "./"
)
}
\arguments{
\item{data}{dataset used to generate the report}

\item{title}{a string, the title of the report. Defaults to "Genetics Lab Report"}

\item{author}{a string, the name of the author. Defaults to system username}

\item{date}{a string. Defaults to the system date}

\item{save_to}{directory in which the report is going to be saved in, defaults to current dir}
}
\value{
file name of the outputted report, which is a word-document
}
\description{
genomic_report() take a dataset, report title, author name and date and creates
a full laboratory report with Subsection titles and placeholder text. By default, it generates
a volcano, violin, and scatterplot and a table from the dataset and adds them to the output document
with figure/table caption. The output of this function is the name of the output report which
is in a word document format.
}
\examples{
\dontrun{genomic_report("My Lab Report", "Full Name")}
}
