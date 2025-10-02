#' Create a metadata file for a dataset
#'
#'
#'
#'
#'
#' @export
#'
#' @importFrom purrr map_chr map
meta <- function(data) {
  # Extract variable information
  tibble(
    var = names(data),
    class = map_chr(data, ~paste(class(.x), collapse = ", ")),

    var_levels = map(data, ~{
      if (is.factor(.x)) {
        levels(.x)
      } else if (inherits(.x, "haven_labelled")) {
        attr(.x, "labels")
      } else {
        NA_integer_
      }

    }),

  n_levels = ifelse(is.na(var_levels),
                    NA_real_,
                    length(var_levels)),

  variable_label = map_chr(data, ~{
      label <- attr(.x, "label")
      if (is.null(label)) NA_character_ else as.character(label)
    })
  )
}
