h2h_long <- function(cr_data, ..., fill = list()) {
  cr_data %>%
    as_longcr(repair = TRUE) %>%
    get_cr_matchups() %>%
    summarise_item(c("player1", "player2"), ...) %>%
    # This seems more consistent than `player1 = .data$player1` etc.
    tidyr::complete(
      !!! syms(c("player1", "player2")),
      fill = fill
    ) %>%
    add_class("h2h_long")
}

long_to_mat <- function(tbl, row_key, col_key, value = NULL,
                        silent = TRUE) {
  row <- pull(tbl, !! enquo(row_key))
  col <- pull(tbl, !! enquo(col_key))
  if (identical(enquo(value), quo(NULL))) {
    val <- tbl %>%
      select(- !! enquo(row_key), - !! enquo(col_key)) %>%
      first_col(silent = silent, target_name = "value")
  } else {
    val <- pull(tbl, !! enquo(value))
  }

  if (is.list(val)) {
    fill <- list(NULL)
  } else {
    fill <- NA
  }

  row_names <- levels2(row)
  col_names <- levels2(col)
  res <- matrix(
    fill, nrow = length(row_names), ncol = length(col_names),
    dimnames = list(row_names, col_names)
  )
  # For repairing in case `val` is list
  res_attrs <- attributes(res)

  res[cbind(as.character(row), as.character(col))] <- val
  attributes(res) <- res_attrs

  res
}

as_h2h_mat <- function(tbl) {
  if (!inherits(tbl, "h2h_long")) {
    stop("`as_h2h_mat` must be applied only to `h2h_long` object.")
  }

  tbl %>%
    long_to_mat(.data$player1, .data$player2,
                silent = ncol(.) == 3) %>%
    add_class("h2h_mat")
}

h2h_mat <- function(cr_data, ...) {
  cr_data %>%
    h2h_long(...) %>%
    as_h2h_mat()
}

fill_h2h_new <- function(x, fill) {
  UseMethod("fill_h2h_new")
}

fill_h2h_new.default <- function(x, fill) {
  stop("`fill_h2h` should be applied to either `h2h_long` or `h2h_mat` object.")
}

fill_h2h_new.h2h_long <- function(x, fill) {

}
