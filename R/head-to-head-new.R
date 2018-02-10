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
#' [matchups][get_matchups()]. In other words, every game is converted in
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
    get_matchups() %>%
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
#' Common Head-to-Head functions
#'
#' List of commonly used functions for computing Head-to-Head values.
#'
#' @details `h2h_funs` is a named list of [quosures][rlang::quo()]
#'   representing commonly used expressions of Head-to-Head functions for
#'   computing Head-to-Head values with [h2h_long()] or [h2h_mat()]. Names of
#'   the elements will be used as Head-to-Head value names. To use them inside
#'   `h2h_long()` or `h2h_mat()` use [unquoting][rlang::quasiquotation]
#'   mechanism from rlang package.
#'
#' Currently present functions:
#' - `mean_score_diff` - computes mean score difference of `player1`
#' compared to `player2`.
#' - `mean_score_diff_pos` - equivalent to `mean_score_diff` but
#' returns 0 if result is negative.
#' - `mean_score` - computes mean score of `player1`.
#' - `sum_score_diff` - computes sum of score differences of
#'   `player1` compared to `player2`.
#' - `sum_score_diff_pos` - equivalent to `sum_score_diff` but
#' returns 0 if result is negative.
#' - `sum_score` - computes sum of scores of `player1`.
#' - `num_wins` - computes number of matchups `player1` scored __more__
#' than `player2`. Draws (determined by [dplyr::near()]) and matchups between
#' same player are omitted.
#' - `num_wins2` - computes number of matchups `player1` scored __more__ than
#' `player2` __plus__ half the number of matchups where they had draw. Matchups
#' between same player are omitted.
#' - `num` - computes number of matchups.
#'
#' __Note__ that it is generally better to subset `h2h_funs` using names
#' rather than indices because the order of elements might change in future
#' versions.
#'
#' @examples
#' ncaa2005 %>% h2h_long(!!! h2h_funs)
#'
#' ncaa2005 %>% h2h_mat(!!! h2h_funs["num_wins2"])
#'
#' @seealso [Long format][h2h-long] of Head-to-Head values.
#'
#' [Matrix format][h2h-mat] of Head-to-Head values.
#'
#' @export
h2h_funs <- list(
  mean_score_diff = quo(mean(score1 - score2)),
  mean_score_diff_pos = quo(max(mean(score1 - score2), 0)),
  mean_score = quo(mean(score1)),
  sum_score_diff = quo(sum(score1 - score2)),
  sum_score_diff_pos = quo(max(sum(score1 - score2), 0)),
  sum_score = quo(sum(score1)),
  num_wins = quo(num_wins(player1, score1, player2, score2,
                          half_for_draw = FALSE)),
  num_wins2 = quo(num_wins(player1, score1, player2, score2,
                           half_for_draw = TRUE)),
  num = quo(n())
)

#' Compute number of wins
#'
#' Function to accompany `num_wins` and `num_wins2` in [h2h_funs]. May be useful
#' for outer usage.
#'
#' @param player1 Vector of `player1` identifiers.
#' @param score1 Vector of scores for `player1`.
#' @param player2 Vector of `player2` identifiers.
#' @param score2 Vector of scores for `player2`.
#' @param half_for_draw Use `TRUE` to add half the mathups with draws.
#'
#' @keywords internal
#'
#' @export
num_wins <- function(player1, score1, player2, score2,
                     half_for_draw) {
  not_identity <- player1 != player2
  score1 <- score1[not_identity]
  score2 <- score2[not_identity]

  near_score <- near(score1, score2)

  sum(score1[!near_score] > score2[!near_score]) +
    half_for_draw * 0.5 * sum(near_score)
}
