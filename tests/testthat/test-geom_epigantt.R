test_that("linewidth calculation works correctly", {
  # Create test data
  test_data <- data.frame(
    y = factor(1:20),
    xmin = as.Date("2024-01-01"),
    xmax = as.Date("2024-01-10")
  )

  # Test with default parameters
  result <- .calc_linewidth(test_data, flipped_aes = TRUE, max = 8, min = 1, scaling_factor = 100)
  expect_identical(result, 5) # 100/20 = 5

  # Test flipped aes
  test_data <- data.frame(
    x = factor(1:20),
    ymin = as.Date("2024-01-01"),
    ymax = as.Date("2024-01-10")
  )

  # Test with default parameters
  result <- .calc_linewidth(test_data, flipped_aes = FALSE, max = 8, min = 1, scaling_factor = 100)
  expect_identical(result, 5)

  # Test with single observation (should return max)
  single_data <- data.frame(
    y = factor(1),
    xmin = as.Date("2024-01-01"),
    xmax = as.Date("2024-01-10")
  )
  result <- .calc_linewidth(single_data, flipped_aes = TRUE, max = 8, min = 1, scaling_factor = 100)
  expect_identical(result, 8)

  # Test with many observations (should return min)
  many_data <- data.frame(
    y = factor(1:200),
    xmin = as.Date("2024-01-01"),
    xmax = as.Date("2024-01-10")
  )
  result <- .calc_linewidth(many_data, flipped_aes = TRUE, max = 8, min = 1, scaling_factor = 100)
  expect_identical(result, 1)
})

test_that("geom_epigantt works with example data", {
  # Create minimal example data
  df <- data.frame(
    Patient = c("A", "B"),
    start = as.Date(c("2024-01-01", "2024-01-02")),
    end = as.Date(c("2024-01-10", "2024-01-12")),
    group = c("ward1", "ward2")
  )

  # Test plot creation
  p <- ggplot(df) +
    geom_epigantt(aes(y = Patient, xmin = start, xmax = end, color = group))

  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("1_geom_epigantt_basic", p)

  # Coord Flip
  p1 <- ggplot(df) +
    geom_epigantt(aes(x = Patient, ymin = start, ymax = end, color = group))
  expect_no_error(p1)
  vdiffr::expect_doppelganger("1_geom_epigantt_flip_aes", p1)
})

test_that("geom_epigantt handles NA values correctly", {
  # Create data with NAs
  df_na <- data.frame(
    Patient = c("A", "B", "C"),
    start = as.Date(c("2024-01-01", NA, "2024-01-03")),
    end = as.Date(c("2024-01-10", "2024-01-12", NA))
  )

  # Test with na.rm = TRUE
  expect_no_error(ggplot(df_na) +
    geom_epigantt(
      aes(y = Patient, xmin = start, xmax = end),
      na.rm = TRUE
    ))

  # Test with na.rm = FALSE
  expect_warning(vdiffr::expect_doppelganger(
    "2_geom_epigantt_na",
    ggplot(df_na) +
      geom_epigantt(
        aes(y = Patient, xmin = start, xmax = end),
        na.rm = FALSE
      )
  ), regexp = "Removed")
})

test_that("geom_epigantt full test", {
  linelist_hospital_outbreak |>
    tidyr::pivot_longer(
      cols = starts_with("ward"),
      names_to = c(".value", "num"),
      names_pattern = "ward_(name|start_of_stay|end_of_stay)_([0-9]+)",
      values_drop_na = TRUE
    ) -> df_stays_long

  linelist_hospital_outbreak |>
    tidyr::pivot_longer(cols = starts_with("pathogen"), values_to = "date") -> df_detections_long

  p <- ggplot(df_stays_long) +
    geom_epigantt(aes(y = Patient, xmin = start_of_stay, xmax = end_of_stay, color = name)) +
    geom_point(aes(y = Patient, x = date), data = df_detections_long) +
    scale_y_discrete_reverse() +
    theme_bw() +
    theme(legend.position = "bottom")

  expect_no_error(p)
  expect_warning(vdiffr::expect_doppelganger("3_geom_epigantt_full", p), regexp = "Removed")

  # Test coord_flip()
  p1 <- ggplot(df_stays_long) +
    geom_epigantt(aes(y = Patient, xmin = start_of_stay, xmax = end_of_stay, color = name)) +
    geom_point(aes(y = Patient, x = date), data = df_detections_long) +
    scale_y_discrete_reverse() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
  expect_no_error(p1)
  expect_warning(vdiffr::expect_doppelganger("3_geom_epigantt_full_flip_aes", p1), regexp = "Removed")
})
