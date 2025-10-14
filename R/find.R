#' Find variable labels that match a given string
#'
#' @export
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
#'
#' @export
#'
#' @importFrom purrr keep
#' @importFrom stringr str_detect
find_level <- function(data, pattern) {
  # Extract all variable labels
  all_levels <- map(data, levels)

  # Find matches (case-sensitive search)
  keep(all_levels, function(x) {
    if (is.null(x)) {
      FALSE
    } else {
      str_detect(x, pattern = pattern) %>% any()
    }
  })
}


#' Make educated guesses for which variables pertain to the same battery
#'
#' @export
find_batts <- function(data) {

  meta(data)$variable_label %>%
    map_chr(~str_extract(.x, "^Q[[:digit:]]+")) %>%
    {tibble(var = names(.),
            cat = .)} %>%
    split(~cat) %>%
    map(~pull(.x, var)) %>%
    # break apart the groups whose members dont all have the same levels
    imap(function(vec, name) {

      levels_out <- vec %>%
        set_names() %>%
        map(~data[[.x]] %>% levels())

      if (length(unique(levels_out)) > 1) {

        levels_out_sub <- unique(levels_out)

        map(levels_out_sub, function(sub) {
          keep(levels_out, ~identical(.x, sub)) %>%
            names()
        }) %>%
          set_names(1:length(levels_out_sub))

      } else {
        names(levels_out)
      }
    }) %>%
    list_flatten() %>%
    keep(~length(.x) > 1)


}

