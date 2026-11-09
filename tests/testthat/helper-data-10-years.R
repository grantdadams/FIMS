# Helper data for tests that fit or run a model ----
# 10 years of data_big keep the models fast. Weight at age has 1 more year,
# for the start of the year after the last model year.
data_10_years <- FIMS::data_big |>
  dplyr::filter(
    is.na(timing) |
      timing <= dplyr::if_else(type == "weight_at_age", 11, 10)
  ) |>
  FIMSFrame()
