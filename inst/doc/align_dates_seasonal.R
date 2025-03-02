## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 4
)

## ----setup, echo = FALSE------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsurveillance)

## ----echo=FALSE---------------------------------------------------------------
influenza_germany |>
  filter(AgeGroup == "00+") |>
  align_dates_seasonal(
    dates_from = ReportingWeek,
    date_resolution = "isoweek",
    start = 28
  ) -> df_flu_aligned

ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence)) +
  stat_summary(
    aes(linetype = "Historical Median (Min-Max)"),
    data = . %>% filter(!current_season),
    fun.data = median_hilow, geom = "ribbon", alpha = 0.3
  ) +
  stat_summary(
    aes(linetype = "Historical Median (Min-Max)"),
    data = . %>% filter(!current_season),
    fun = median, geom = "line"
  ) +
  geom_line(
    aes(linetype = "2024/25"),
    data = . %>% filter(current_season), colour = "dodgerblue4", linewidth = 2
  ) +
  labs(linetype = NULL) +
  scale_x_date(date_labels = "%b'%y") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.8))

## -----------------------------------------------------------------------------
library(ggplot2)

influenza_germany |>
  align_dates_seasonal(
    dates_from = ReportingWeek, date_resolution = "epiweek", start = 28
  ) -> df_flu_aligned

ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence, color = season)) +
  geom_line() +
  facet_wrap(~AgeGroup) +
  theme_bw()

## -----------------------------------------------------------------------------
influenza_germany |>
  align_dates_seasonal(dates_from = ReportingWeek) |>
  group_by(AgeGroup, season) |>
  tally(wt = Cases) |>
  pivot_wider(names_from = AgeGroup, values_from = n)

## -----------------------------------------------------------------------------
influenza_germany |>
  filter(AgeGroup == "00+") |>
  align_dates_seasonal(
    dates_from = ReportingWeek,
    date_resolution = "isoweek",
    start = 28
  ) -> df_flu_aligned

ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence)) +
  stat_summary(
    aes(linetype = "Historical Median (Min-Max)"),
    data = . %>% filter(!current_season),
    fun.data = median_hilow, geom = "ribbon", alpha = 0.3
  ) +
  stat_summary(
    aes(linetype = "Historical Median (Min-Max)"),
    data = . %>% filter(!current_season),
    fun = median, geom = "line"
  ) +
  geom_line(
    aes(linetype = "2024/25"),
    data = . %>% filter(current_season), colour = "dodgerblue4", linewidth = 2
  ) +
  labs(linetype = "") +
  scale_x_date(date_labels = "%b'%y") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.8))

## -----------------------------------------------------------------------------
influenza_germany |>
  filter(AgeGroup != "00+") |>
  align_dates_seasonal(dates_from = ReportingWeek) |>
  ggplot(aes(x = ReportingWeek, weight = Cases, fill = season)) +
  geom_vline_year(color = "grey50") +
  # Use stat = count for more efficient plotting
  geom_epicurve(color = NA, stat = "count") +
  scale_y_cases_5er() +
  theme_bw()

