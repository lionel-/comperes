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

long_to_mat <- function(tbl, row_key, col_key, value) {
  data <- tibble(
    row = tbl[[row_key]],
    col = tbl[[col_key]],
    val = tbl[[value]]
  ) %>%
    # To account for factors
    tidyr::complete(row, col) %>%
    mutate(
      row = as.character(row),
      col = as.character(col)
    ) %>%
    arrange(row, col)

  row_names <- sort(unique(data$row))
  col_names <- sort(unique(data$col))

  matrix(
    data$val,
    nrow = length(row_names), ncol = length(col_names),
    dimnames = list(row_names, col_names),
    byrow = TRUE
  )
}

h2h_mat <- function(cr_data, ..., fill = list()) {
  dots <- rlang::enquos(...)

  if (length(dots) == 0) {
    h2h_fun <- quos(value = NA_real_)
  } else {
    h2h_fun <- dots[1]
  }

  h2h_long_res <- cr_data %>%
    h2h_long(!!! h2h_fun)

  value_col <- setdiff(colnames(h2h_long_res), c("player1", "player2"))

  h2h_long_res %>%
    long_to_mat("player1", "player2", value_col) %>%
    add_class("h2h_mat")
}
