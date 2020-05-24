#' Test if an input is a list of something
#'
#' Tests if all elements of a list are of a specific type.
#' - `is_list_of` accepts any test function.
#' - `is_list_of_lists()` tests if all elements are lists.
#' - `is_list_of_characters()` tests if all elements are character vectors.
#'
#' @param x List to test
#' @param test Test to apply to each element of `x`.
#'     If a character vector, tests if the (class)[class()] or (type)[typeof()]
#'     of `x` matches `test`.
#'     If an unquoted function name, an anonymous function, or a purrr-style
#'     lambda function, applies the function to each element of `x`.
#'
#' @return A logical value. If `input` is not a list, `FALSE` and a warning.
#'
#' @seealso [is.list()] to test if an input is a list
#'
#'   [purrr::flatten()] or [rlang::flatten()] to remove a level of hierarchy
#'   from a list
#'
#'   [rlang::squash()] to remove all levels of hierarchy from a list
#'
#' @export
#' @example examples/is_list_of.R

is_list_of <- function(x, test) {
  if (!is.list(x)) {
    rlang::warn(
      c("`x` is not a list.", paste0("`x` is of class ", class(x), "."))
    )
    return(FALSE)
  }

  if (length(test) != 1) stop("`test` must be length one.")

  if (is.character(test)) {
    all(vapply(x, class, character(1)) == test) |
    all(vapply(x, typeof, character(1)) == test)
  } else {
    all(vapply(x, rlang::as_function(test), logical(1)))
  }
}

#' @rdname is_list_of
#' @export

is_atomic_list <- function(x) is_list_of(x, rlang::is_atomic)

#' @rdname is_list_of
#' @export

is_list_of_lists <- function(x) is_list_of(x, is.list)

#' @rdname is_list_of
#' @export

is_character_list <- function(x) is_list_of(x, is.character)
