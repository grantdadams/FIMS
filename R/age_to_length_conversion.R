# Age-to-length keys
#
# An age-to-length key gives P(length bin | age), with 1 row per age that
# sums to 1. A fleet's key is either data (a fixed key in the FIMSFrame) or is
# derived from growth: the population length distribution at age is
# calculated on a fine population length grid, then mapped onto the fleet's
# length bins.

#' Distribution of length at age over length bins
#'
#' Normal length at age, integrated over each bin. The first bin also holds
#' all shorter lengths and the last bin all longer lengths. 1e-12 is added to
#' every bin before the rows are renormalized, so no bin has zero probability
#' and its log stays finite.
#'
#' @param mean_length,sd_length Mean and SD of length at each age.
#' @param edges Bin edges. There are length(edges) - 1 bins.
#' @return Matrix of probabilities, ages by bins.
#' @noRd
length_distribution <- function(mean_length, sd_length, edges) {
  n_bins <- length(edges) - 1
  n_ages <- length(mean_length)
  sd_length <- ad_max(sd_length, 1e-8)
  # Cumulative probability below each interior edge, ages by edges.
  interior_edges <- matrix(edges[2:n_bins], n_ages, n_bins - 1, byrow = TRUE)
  # RTMB's pnorm(), matrix() and rowSums() work on AD values. The base and
  # stats versions do not.
  below <- RTMB::pnorm(
    interior_edges,
    RTMB::matrix(mean_length, n_ages, n_bins - 1),
    RTMB::matrix(sd_length, n_ages, n_bins - 1)
  )
  below <- RTMB::matrix(below, n_ages, n_bins - 1)
  # The upper tail is written as pnorm(2 * mean - edge), which equals
  # 1 - pnorm(edge) but avoids the loss of precision from subtracting a
  # number close to 1 from 1.
  above_last <- RTMB::pnorm(2 * mean_length - edges[n_bins], mean_length, sd_length)
  probability <- cbind(
    below[, 1],
    below[, -1, drop = FALSE] - below[, -(n_bins - 1), drop = FALSE],
    above_last
  ) + 1e-12
  probability / RTMB::rowSums(probability)
}

#' Weights that move probability from 1 set of length bins to another
#'
#' Each source bin's probability is shared among destination bins in
#' proportion to how much of the source bin they cover. These depend only on
#' the bin edges, so they are data.
#'
#' @return Matrix, source bins by destination bins.
#' @noRd
rebin_weights <- function(source_edges, destination_edges) {
  # Widen the outer destination edges to cover the whole source range, so the
  # fleet's first and last bins act as minus and plus groups.
  destination_edges[1] <- min(destination_edges[1], source_edges[1])
  last <- length(destination_edges)
  destination_edges[last] <- max(
    destination_edges[last], source_edges[length(source_edges)]
  )
  n_source <- length(source_edges) - 1
  n_destination <- length(destination_edges) - 1
  rebin_matrix <- matrix(0, n_source, n_destination)
  for (j in seq_len(n_destination)) {
    overlap <- pmin(source_edges[-1], destination_edges[j + 1]) -
      pmax(source_edges[-(n_source + 1)], destination_edges[j])
    rebin_matrix[, j] <- pmax(overlap, 0) / diff(source_edges)
  }
  rebin_matrix
}

#' A fleet's age-to-length key from the population length distribution
#'
#' @param length_probability Population P(length bin | age), ages by bins.
#' @param rebin_matrix Output of rebin_weights() for this fleet.
#' @noRd
age_to_length_conversion_derived <- function(length_probability, rebin_matrix) {
  key <- length_probability %*% rebin_matrix + 1e-12
  key / RTMB::rowSums(key)
}

#' Fleet bin edges from the lower edges in the data
#'
#' The last bin is given the same width as the one before it. The lower edges
#' come from resolve_fleet_length_bins(), which sorts them and removes
#' duplicates.
#'
#' @param lower_edges The fleet's length bins (lower edges).
#' @param fleet The fleet name, for the error message.
#' @noRd
edges_from_lower_edges <- function(lower_edges, fleet) {
  n_bins <- length(lower_edges)
  if (n_bins < 2) {
    cli::cli_abort(c(
      "x" = "Fleet {.val {fleet}} has {n_bins} length bin{?s}:
      {.val {lower_edges}}.",
      "i" = "A fleet with length data needs at least 2 length bins, because
      the width of the last bin is set from the bin before it.",
      "i" = "Check the fleet's lengths with {.code dplyr::count(get_data(data),
      fleet, type, length)}."
    ))
  }
  c(
    lower_edges,
    lower_edges[n_bins] + (lower_edges[n_bins] - lower_edges[n_bins - 1])
  )
}

#' The default population length grid
#'
#' Bins 1 length unit wide (the units of the length data, mm in data_big)
#' from the smallest fleet lower edge up to the largest fleet upper edge,
#' plus 1 more bin at the top. A relative tolerance of 1e-12 means a fleet
#' upper edge that is a whole number plus rounding error does not add a bin.
#'
#' A population bin wider than a fleet bin would spread its probability
#' evenly over the fleet bins it covers, which flattens the predicted length
#' composition. So every fleet bin must be at least 1 length unit wide.
#'
#' @param fleet_edges A list of bin edges, 1 element per fleet with length
#'   bins, named by fleet.
#' @return The population bin edges.
#' @noRd
default_population_edges <- function(fleet_edges) {
  if (length(fleet_edges) == 0) {
    cli::cli_abort(c(
      "x" = "Growth that models length needs length bins, but no fleet has
      any.",
      "i" = "Add {.val length_comp} or {.val length_bin} rows for at least 1
      fleet.",
      "i" = "Check the data types of each fleet with
      {.code dplyr::count(get_data(data), fleet, type)}."
    ))
  }
  narrowest_bin <- vapply(
    fleet_edges, function(bin_edges) min(diff(bin_edges)), numeric(1)
  )
  is_narrow <- narrowest_bin < 1 - 1e-12
  if (any(is_narrow)) {
    cli::cli_abort(c(
      "x" = "{.val {names(fleet_edges)[is_narrow]}} {?has/have} length bins
      narrower than 1 length unit.",
      "i" = "The narrowest fleet bin is {.val {min(narrowest_bin)}} length
      units wide.",
      "i" = "The population length grid has bins 1 length unit wide. A
      population bin wider than a fleet bin would flatten the predicted
      length composition. Use fleet length bins at least 1 unit wide."
    ))
  }
  lower <- min(vapply(fleet_edges, min, numeric(1)))
  fleet_upper <- max(vapply(fleet_edges, max, numeric(1)))
  tolerance <- 1e-12 * max(fleet_upper - lower, 1)
  # The upper edge is built by adding 1 at a time, so the value compared with
  # the top fleet edge is the value the grid ends on. When the lower edge is
  # not a whole number, lower + n can differ from it in the last digit.
  upper <- lower
  n_regular_bins <- 0
  while (upper < fleet_upper - tolerance) {
    upper <- upper + 1
    n_regular_bins <- n_regular_bins + 1
  }
  c(lower + seq_len(n_regular_bins) - 1, upper, upper + 1)
}
