#' Compute long format of Head-to-Head values
#'
#' Functions to compute Head-to-Head values in long pair-value format.
#'
#' @param cr_data Competition results ready for [as_longcr()].
#' @param ... Name-value pairs of Head-to-Head functions (see Details).
#' @param fill A named list that for each variable supplies a single value to
#'   use instead of NA for missing pairs (see tidyr's
#'   [complete()][tidyr::complete()]).
#' @param mat Matrix of Head-to-Head values.
#' @param value String name to be used for column with Head-to-Head value.
#' @param drop Use `TRUE` to drop rows with missing Head-to-Head values (see
#'   Details).
#'
#' @details `h2h_long()` computes Head-to-Head values in
#'   [long][convert-pair-value] format. It allows computation of multiple
#'   Head-to-Head values at the same time by supplying multiple summary
#'   functions in `...`. If no function is supplied in `...`
#'
#' After computing Head-to-Head values of present matchups, implicit missing
#' matchups are turned into explicit (by adding corresponding rows with filling
#' values in Head-to-Head columns) by using tidyr's
#' [complete()][tidyr::complete()]. Use `fill` as in `complete()` to control
#' filling values. To drop those rows use tidyr's [drop_na()][tidyr::drop_na()].
#'
#' `to_h2h_long()` takes __object of [h2h_mat][h2h-mat] structure__ and converts
#' it into `h2h_long` object with value column named as stored in `value`. Use
#' `drop = TRUE` to remove rows with missing values in value column (but not in
#' players').
#'
#' @section Head-to-Head value:
#' Head-to-Head value is a summary statistic of players'
#' [matchups][get_cr_matchups()]. In other words, every game is converted in
#' series of "subgames" between ordered pairs of players (including selfplay)
#' which is stored as [widecr][results-widecr] object. After that summary of
#' item, defined by columns `player1` and `player2` is computed using
#' [summarise_item()].
#'
#' That said, name-value pairs of Head-to-Head functions should be defined as
#' for `summarise_item()` applied to data with columns `game`, `player1`,
#' `score1`, `player2`, `score2`.
#'
#' @return An object of class `h2h_long` which is a
#'   [tibble][tibble::tibble] with columns `player1`, `player2` and those,
#'   produced by Head-to-Head functions (for `h2h_long()` maybe none).
#'
#' @examples
#' ncaa2005 %>%
#'   h2h_long(
#'     mean_score = mean(score1),
#'     mean_abs_score = mean(abs(score1 - score2))
#'   )
#'
#' ncaa2005[-(1:2), ] %>%
#'   h2h_long(
#'     mean_score = mean(score1),
#'     fill = list(mean_score = 0)
#'   )
#'
#' @name h2h-long
NULL

#' @rdname h2h-long
#' @export
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

#' @rdname h2h-long
#' @export
to_h2h_long <- function(mat, value = "h2h_value", drop = FALSE) {
  mat %>%
    mat_to_long("player1", "player2", value, drop = drop) %>%
    add_class_cond("h2h_long")
}

#' Compute matrix format of Head-to-Head values
#'
#' Functions to compute Head-to-Head values in matrix pair-value format.
#'
#' @param cr_data Competition results ready for [as_longcr()].
#' @param ... Name-value pairs of Head-to-Head functions (see Details).
#' @param fill A single value to use instead of `NA` for missing pairs.
#' @param tbl Data frame with long format of Head-to-Head values.
#' @param value String name for column with Head-to-Head value.
#'
#' @details `h2h_mat()` computes Head-to-Head values in
#'   [matrix][convert-pair-value] format. It allows multiple
#'   Head-to-Head functions in `...` but only first (if present) will be used.
#'   Basically, it uses supplied function to compute [long format][h2h-long] of
#'   Head-to-Head values and then transforms it naturally to matrix, filling
#'   missing values with `fill`.
#'
#' `to_h2h_mat()` takes __object of `h2h_long` structure__ and converts it into
#' `h2h_mat` using column with name `value` for values and filling missing
#' values with `fill`. If `value` is `NULL` it takes first non-player column. If
#' there is no such column, it will use vector of dummy values (`NA`s or
#' `fill`s).
#'
#' @inheritSection h2h-long Head-to-Head value
#'
#' @return An object of class `h2h_mat` which is a [matrix] with row names
#'   indicating first player in matchup, col names - second and values -
#'   Head-to-Head values.
#'
#' @examples
#' # Only first function is used
#' ncaa2005 %>%
#'   h2h_mat(
#'     mean_score = mean(score1),
#'     mean_abs_score = mean(abs(score1 - score2))
#'   )
#'
#' ncaa2005[-(1:2), ] %>%
#'   h2h_mat(mean_score = mean(score1), fill = 0)
#'
#' @name h2h-mat
NULL

#' @rdname h2h-mat
#' @export
h2h_mat <- function(cr_data, ..., fill = NULL) {
  dots <- rlang::quos(...)
  if (length(dots) > 1) {
    dots <- dots[1]
    was_trunc <- TRUE
  } else {
    was_trunc <- FALSE
  }

  res_long <- cr_data %>% h2h_long(!!! dots)

  value_col <- setdiff(colnames(res_long), c("player1", "player2"))
  if (was_trunc || (length(value_col) == 0)) {
    assert_used_value_col(value_col)
  }

  res_long %>%
    # This might be even faster then `fill` as list-argument to `h2h_long`
    # in case of many players
    tidyr::drop_na(one_of(value_col)) %>%
    to_h2h_mat(value = value_col, fill = fill)
}

#' @rdname h2h-mat
#' @export
to_h2h_mat <- function(tbl, value = NULL, fill = NULL) {
  tbl %>%
    long_to_mat("player1", "player2", value, fill = fill,
                silent = !identical(value, NULL)) %>%
    add_class_cond("h2h_mat")
}


# Head-to-Head functions --------------------------------------------------



# Head-to-Head helpers ----------------------------------------------------

