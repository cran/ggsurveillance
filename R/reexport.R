#' Duplicate rows according to a weighting variable
#'
#' @description
#' `uncount()` is provided by the tidyr package, and re-exported
#' by ggsurveillance. See [tidyr::uncount()] for more details.
#'
#' `uncount()` and its alias `expand_counts()` are complements of [dplyr::count()]: they take
#' a data.frame with a column of frequencies and duplicate each row according to
#' those frequencies.
#'
#' @inheritParams tidyr::uncount
#'
#' @return A `data.frame` with rows duplicated according to weights.
#' @examples
#' df <- data.frame(x = c("a", "b"), n = c(2, 3))
#' df |> uncount(n)
#' # Or equivalently:
#' df |> expand_counts(n)
#'
#' @name uncount, expand_counts
#' @rdname uncount
#' @aliases expand_counts
NULL

#' @importFrom tidyr uncount
#' @name uncount
#' @rdname uncount
#' @export
tidyr::uncount

#' @importFrom tidyr uncount
#' @rdname uncount
#' @export
expand_counts <- tidyr::uncount

#' Date labeller
#'
#' Re-export from the scales package.
#' * Can be used to overwrite the default locale of date labels.
#' * `label_date_short()` only labels part of the dates, when they change,
#' i.e. year is only labelled when the year changes.
#' * See [scales::label_date()] and [scales::label_date_short()] for more details.
#'
#' @inheritParams scales::label_date
#'
#' @return A character vector of formatted dates.
#'
#' @examples
#' library(tidyr)
#' library(outbreaks)
#' library(ggplot2)
#'
#' # Change locale of date labels to Italian
#' sars_canada_2003 |> # SARS dataset from outbreaks
#'   pivot_longer(starts_with("cases"), names_prefix = "cases_", names_to = "origin") |>
#'   ggplot(aes(x = date, weight = value, fill = origin)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_x_date(labels = label_date("%B %Y", locale = "it"), date_breaks = "1 month") +
#'   scale_y_cases_5er() +
#'   theme_classic()
#'
#' # label_date_short()
#' sars_canada_2003 |> # SARS dataset from outbreaks
#'   pivot_longer(starts_with("cases"), names_prefix = "cases_", names_to = "origin") |>
#'   ggplot(aes(x = date, weight = value, fill = origin)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_x_date(labels = label_date_short(), date_breaks = "1 week") +
#'   scale_y_cases_5er() +
#'   theme_classic()
#'
#' @importFrom scales label_date
#' @name label_date
#' @rdname label_date
#' @export
scales::label_date

#' @importFrom scales label_date_short
#' @name label_date
#' @rdname label_date
#' @export
scales::label_date_short
