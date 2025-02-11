## ----include = FALSE----------------------------------------------------------
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
library(outbreaks)
library(ggsurveillance)

## -----------------------------------------------------------------------------
ggplot(outbreaks::ebola_kikwit_1995, aes(x = date, weight = onset)) +
  geom_epicurve(date_resolution = "week") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%V'%g", name = "week") +
  scale_y_cases_5er() +
  theme_bw()

## -----------------------------------------------------------------------------
outbreaks::sars_canada_2003 |>
  pivot_longer(starts_with("cases"), names_prefix = "cases_", names_to = "origin") |>
  ggplot(aes(x = date, weight = value, fill = origin)) +
  geom_epicurve(date_resolution = "week") +
  scale_y_cases_5er() +
  theme_bw()

## -----------------------------------------------------------------------------
influenza_germany |>
  # Keep Age Groups 00-14, 15-59, 60+
  filter(AgeGroup != "00+") |>
  # Calc Influenza Seasons
  align_dates_seasonal(dates_from = ReportingWeek) |>
  ggplot(aes(x = ReportingWeek, weight = Cases, fill = season)) + # , weight = Cases
  geom_vline_year(color = "grey50") +
  geom_epicurve(color = NA, stat = "bin_date", date_resolution = "week") +
  scale_y_cases_5er() +
  theme_bw()

## -----------------------------------------------------------------------------
influenza_germany |>
  # Calc Influenza Seasons
  align_dates_seasonal(dates_from = ReportingWeek) |>
  ggplot(aes(x = ReportingWeek, weight = Cases, color = AgeGroup)) + # , weight = Cases
  geom_line(stat = "bin_date", date_resolution = "month") +
  scale_y_cases_5er() +
  facet_wrap(~AgeGroup) +
  theme_bw()

