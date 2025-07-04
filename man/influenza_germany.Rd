% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{influenza_germany}
\alias{influenza_germany}
\title{German Influenza (FLU) Surveillance data}
\format{
A data frame with 1,037 rows and 4 columns:
\describe{
\item{ReportingWeek}{Reporting Week in "2024-W03" format}
\item{AgeGroup}{Age groups: \verb{00+} for all and \code{00-14}, \code{15-59} and \verb{60+} for age stratified cases.}
\item{Cases}{Weekly case count}
\item{Incidence}{Calculated weekly incidence}
}
}
\source{
License CC-BY 4.0: Robert Koch-Institut (2025): Laborbestätigte Influenzafälle in Deutschland. Dataset. Zenodo.
DOI:10.5281/zenodo.14619502. \url{https://github.com/robert-koch-institut/Influenzafaelle_in_Deutschland}
}
\usage{
influenza_germany
}
\description{
A subset of the weekly German influenza surveillance data from January 2020 to January 2025.
}
\examples{
library(ggplot2)

influenza_germany |>
  align_dates_seasonal(
    dates_from = ReportingWeek, date_resolution = "isoweek", start = 28
  ) -> df_flu_aligned

ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence, color = season)) +
  geom_line() +
  facet_wrap(~AgeGroup) +
  theme_bw() +
  theme_mod_rotate_x_axis_labels_45()

}
\keyword{datasets}
