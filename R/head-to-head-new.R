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

first_col_name <- function(tbl, silent = TRUE, target_name = "value") {
  if (ncol(tbl) == 0) {
    if (!silent) {
      message("No ", target_name, " found. Using dummy vector.")
    }

    NULL
  } else {
    res <- colnames(tbl)[1]
    if (!silent) {
      message("Using ", res, " as ", target_name, ".")
    }

    res
  }
}

miss_value <- function(type = NULL, fill = NULL) {
  if (!identical(fill, NULL)) {
    fill
  } else {
    if (identical(type, "list")) {
      list(NULL)
    } else {
      NA
    }
  }
}

long_to_mat <- function(tbl, row_key, col_key, value = NULL,
                        fill = NULL, silent = TRUE) {
  row <- pull(tbl, !! enquo(row_key))
  col <- pull(tbl, !! enquo(col_key))

  if (identical(enquo(value), quo(NULL))) {
    val_name <- tbl %>%
      select(- !! enquo(row_key), - !! enquo(col_key)) %>%
      first_col_name(silent = silent)

    if (identical(val_name, NULL)) {
      mat_elem <- miss_value(NULL, fill)
      val <- rep(mat_elem, nrow(tbl))
    } else {
      val <- tbl[[val_name]]
      mat_elem <- miss_value(class(val), fill)
    }
  } else {
    val <- pull(tbl, !! enquo(value))
    mat_elem <- miss_value(class(val), fill)
  }

  row_names <- levels2(row)
  col_names <- levels2(col)
  res <- matrix(
    mat_elem, nrow = length(row_names), ncol = length(col_names),
    dimnames = list(row_names, col_names)
  )
  # For repairing in case `val` is list
  res_attrs <- attributes(res)

  res[cbind(as.character(row), as.character(col))] <- val
  attributes(res) <- res_attrs

  res
}

as_h2h_mat <- function(tbl, fill = NULL) {
  if (!inherits(tbl, "h2h_long")) {
    stop("`as_h2h_mat` must be applied only to `h2h_long` object.")
  }

  tbl %>%
    long_to_mat(.data$player1, .data$player2, fill = fill,
                silent = ncol(.) == 3) %>%
    add_class("h2h_mat")
}

h2h_mat <- function(cr_data, ..., fill = NULL) {
  cr_data %>%
    h2h_long(...) %>%
    # This might be even faster then `fill` as list-argument to `h2h_long`
    # in case of many players
    tidyr::drop_na() %>%
    add_class_cond("h2h_long") %>%
    as_h2h_mat(fill = fill)
}
