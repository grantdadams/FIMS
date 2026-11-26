# Helper data for tests of catch-at-length models ----
# Used by test-catch_at_length.R, test-call_module.R and
# test-integration-retape.R.

# A 3-year, 4-length-bin (25, 30, 35, 40 mm) catch-at-length model with 1
# directed pot fishery (retained + discarded catch and length comps), 1 survey
# (mature index and immature/mature length comps), and 2 growth-increment
# observations.
cal_years <- 2001:2003
cal_lengths <- c(25, 30, 35, 40)
cal_weights <- c(0.0001, 0.0002, 0.0004, 0.0008)
cal_terminal_molt <- c(0.1, 0.3, 0.7, 0.95)

cal_data <- dplyr::bind_rows(
  # Directed fishery retained catch
  data.frame(
    type = "catch_retained",
    fleet = "pot_fleet",
    age = NA_real_,
    length = NA_real_,
    maturity = NA_character_,
    timing = cal_years,
    observed = c(10, 12, 11),
    unit = "mt",
    uncertainty = "~ dlnorm(meanlog = log_catch_retained_expected, sdlog = 0.05)"
  ),
  # Directed fishery discarded catch
  data.frame(
    type = "catch_discarded",
    fleet = "pot_fleet",
    age = NA_real_,
    length = NA_real_,
    maturity = NA_character_,
    timing = cal_years,
    observed = c(3, 4, 3.5),
    unit = "mt",
    uncertainty = "~ dlnorm(meanlog = log_catch_discarded_expected, sdlog = 0.07)"
  ),
  # Directed fishery retained length comps
  data.frame(
    type = "length_comp",
    fleet = "pot_fleet",
    age = NA_real_,
    length = rep(cal_lengths, times = length(cal_years)),
    maturity = NA_character_,
    timing = rep(cal_years, each = length(cal_lengths)),
    observed = rep(c(0.05, 0.15, 0.35, 0.45), times = length(cal_years)),
    unit = "proportion",
    uncertainty = "~ robust_normal_multinomial(size = 100, prob = lengthcomp_retained_proportion)"
  ),
  # Directed fishery discarded length comps
  data.frame(
    type = "length_comp",
    fleet = "pot_fleet",
    age = NA_real_,
    length = rep(cal_lengths, times = length(cal_years)),
    maturity = NA_character_,
    timing = rep(cal_years, each = length(cal_lengths)),
    observed = rep(c(0.40, 0.35, 0.15, 0.10), times = length(cal_years)),
    unit = "proportion",
    uncertainty = "~ robust_normal_multinomial(size = 50, prob = lengthcomp_discarded_proportion)"
  ),
  # Survey mature biomass index
  data.frame(
    type = "index",
    fleet = "survey_fleet",
    age = NA_real_,
    length = NA_real_,
    maturity = "mature",
    timing = cal_years,
    observed = c(50, 55, 52),
    unit = "mt",
    uncertainty = "~ dlnorm(meanlog = log_index_expected, sdlog = 0.1)"
  ),
  # Survey immature length comps
  data.frame(
    type = "length_comp",
    fleet = "survey_fleet",
    age = NA_real_,
    length = rep(cal_lengths, times = length(cal_years)),
    maturity = "immature",
    timing = rep(cal_years, each = length(cal_lengths)),
    observed = rep(c(0.45, 0.30, 0.15, 0.10), times = length(cal_years)),
    unit = "proportion",
    uncertainty = "~ robust_normal_multinomial(size = 100, prob = lengthcomp_immature_proportion)"
  ),
  # Survey mature length comps
  data.frame(
    type = "length_comp",
    fleet = "survey_fleet",
    age = NA_real_,
    length = rep(cal_lengths, times = length(cal_years)),
    maturity = "mature",
    timing = rep(cal_years, each = length(cal_lengths)),
    observed = rep(c(0.05, 0.20, 0.35, 0.40), times = length(cal_years)),
    unit = "proportion",
    uncertainty = "~ robust_normal_multinomial(size = 100, prob = lengthcomp_mature_proportion)"
  ),
  # Empirical weight at length
  data.frame(
    type = "weight_at_length",
    fleet = NA_character_,
    age = NA_real_,
    length = rep(cal_lengths, times = 2),
    maturity = rep(c("immature", "mature"), each = length(cal_lengths)),
    timing = NA_real_,
    observed = rep(cal_weights, times = 2),
    unit = "mt",
    uncertainty = NA_character_
  ),
  # Terminal molt probability at length by year (model years and the year
  # after the last)
  data.frame(
    type = "terminal_molt_probability_at_length",
    fleet = NA_character_,
    age = NA_real_,
    length = rep(cal_lengths, times = length(cal_years) + 1),
    maturity = "immature",
    timing = rep(c(cal_years, max(cal_years) + 1), each = length(cal_lengths)),
    observed = rep(cal_terminal_molt, times = length(cal_years) + 1),
    unit = "proportion",
    uncertainty = NA_character_
  ),
  # Growth increment observations (fleet = NA)
  data.frame(
    type = "growth_increment",
    fleet = NA_character_,
    age = NA_real_,
    length = c(27.5, 32.5),
    maturity = "immature",
    timing = NA_real_,
    observed = c(8.0, 9.0),
    unit = "mm",
    uncertainty = "~ dlnorm(meanlog = log_growth_increment_expected, sdlog = 0.05)"
  )
)
