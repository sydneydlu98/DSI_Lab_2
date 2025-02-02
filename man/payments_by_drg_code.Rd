% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DSI_Lab_2_package_ans.R
\name{payments_by_drg_code}
\alias{payments_by_drg_code}
\title{Boxplot of Payments by DRG code}
\usage{
payments_by_drg_code(data, type)
}
\arguments{
\item{data}{a data frame}

\item{type}{a string name for the type of payments}
}
\value{
A box plot of payments by DRG code
}
\description{
This function produces a boxplot of payments by DRG code.
}
\examples{
library(readr)
drg_data <- read_csv("DRG_data.csv")

payments_by_drg_code(drg_data, "Average Medicare Payments")

}
