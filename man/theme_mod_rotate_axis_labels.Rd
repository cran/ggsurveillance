% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_theme_helpers.R
\name{theme_mod_rotate_axis_labels}
\alias{theme_mod_rotate_axis_labels}
\alias{theme_mod_rotate_x_axis_labels}
\alias{theme_mod_rotate_x_axis_labels_90}
\alias{theme_mod_rotate_x_axis_labels_45}
\alias{theme_mod_rotate_x_axis_labels_30}
\alias{theme_mod_rotate_x_axis_labels_60}
\alias{theme_mod_rotate_y_axis_labels}
\title{Rotate axis labels}
\usage{
theme_mod_rotate_x_axis_labels(
  angle = 90,
  margin_top = 2,
  vjust = 0.4,
  hjust = 0,
  ...
)

theme_mod_rotate_x_axis_labels_90(angle = 90, ...)

theme_mod_rotate_x_axis_labels_45(angle = 45, ...)

theme_mod_rotate_x_axis_labels_30(angle = 30, ...)

theme_mod_rotate_x_axis_labels_60(angle = 60, ...)

theme_mod_rotate_y_axis_labels(angle = 90, hjust = 0.5, vjust = 0, ...)
}
\arguments{
\item{angle}{Angle of rotation. Should be between 10 and 90 degrees.}

\item{margin_top}{Used to move the tick labels downwards to prevent text intersecting the x-axis.
Increase for angled multiline text (e.g. 5 for two lines at 45°).}

\item{hjust, vjust}{Text justification within the rotated text element. Just ignore.}

\item{...}{Arguments passed to \code{theme_mod_rotate_x_axis_labels} and \code{\link[ggplot2:element]{ggplot2::element_text()}}.}
}
\value{
Changes the rotation of the axis labels by modifying the \code{axis.text} of the \code{\link[ggplot2:theme]{ggplot2::theme()}}.
}
\description{
Rotate axis labels by 90°, 45° or any angle.
Has to be called after setting the theme.
}
