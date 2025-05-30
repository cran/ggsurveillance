% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_theme_helpers.R
\name{theme_mod_disable_legend, theme_mod_legend_position}
\alias{theme_mod_disable_legend, theme_mod_legend_position}
\alias{theme_mod_disable_legend}
\alias{theme_mod_legend_position}
\alias{theme_mod_legend_top}
\alias{theme_mod_legend_bottom}
\alias{theme_mod_legend_left}
\alias{theme_mod_legend_right}
\alias{theme_mod_remove_legend_title}
\title{Quickly adjust the legend position}
\usage{
theme_mod_disable_legend()

theme_mod_legend_position(
  position = c("top", "bottom", "left", "right", "none", "inside"),
  position.inside = NULL
)

theme_mod_legend_top()

theme_mod_legend_bottom()

theme_mod_legend_left()

theme_mod_legend_right()

theme_mod_remove_legend_title()
}
\arguments{
\item{position}{Position of the ggplot2 legend.
Options are \verb{("top", "bottom", "left", "right", "none", "inside")}}

\item{position.inside}{Coordinates for the legend inside the plot.
If set overwrites \code{position} to \code{inside}.}
}
\value{
Changes the \code{legend.position} of the \code{\link[ggplot2:theme]{ggplot2::theme()}}.
}
\description{
Convenience functions to control the legend position for \code{ggplot2}.
Has to be called after setting the theme.
}
