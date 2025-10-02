#' Find variable labels that match a given string
find_label <- function(data, pattern) {
  # Extract all variable labels
  all_labels <- map(data, ~attr(.x, "label"))

  # Find matches (case-insensitive search)
  keep(all_labels, function(x) {
    if (is.null(x)) {
      FALSE
    } else {
      str_detect(x, pattern = pattern)
    }
  })
}


#' Find variables with any level that match a pattern
find_level <- function(data, pattern) {
  # Extract all variable labels
  all_levels <- map(data, levels)

  # Find matches (case-insensitive search)
  keep(all_levels, function(x) {
    if (is.null(x)) {
      FALSE
    } else {
      str_detect(x, pattern = pattern) %>% any()
    }
  })
}
