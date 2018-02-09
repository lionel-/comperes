# General -----------------------------------------------------------------
#' Skip some action
#'
#' Function for skipping action.
#'
#' @param x Input object
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return An exact copy of `x`.
#'
#' @examples identical(skip_action(1), 1)
#'
#' @export
skip_action <- function(x, ...) {
  x
}

add_name_prefix <- function(tbl, prefix = "", except = NULL) {
  if (identical(prefix, "") || (length(prefix) == 0)) {
    return(tbl)
  }

  rename_cols <- setdiff(colnames(tbl), except)
  if (length(rename_cols) > 0) {
    rename_list <- as.list(rename_cols)
    names(rename_list) <- paste0(prefix, rename_cols)

    tbl <- tbl %>%
      rename(!!! rename_list)
  }

  tbl
}

first_col <- function(tbl, except = NULL, silent = TRUE,
                      target_name = "first column") {
  names_left <- setdiff(colnames(tbl), except)

  if (length(names_left) == 0) {
    if (!isTRUE(silent)) {
      message("No ", target_name, " found. Using vector of NAs.")
    }

    rep(NA, nrow(tbl))
  } else {
    if (!isTRUE(silent)) {
      message("Using ", names_left[[1]], " as ", target_name, ".")
    }

    tbl[[names_left[1]]]
  }
}

#' Levels of vector
#'
#' Extension of [levels()] function. If `levels(x)` is not `NULL`, it is
#' returned. Otherwise, character representation of unique sorted values is
#' returned (with `NA` treated based on `na.last` as in [sort()]).
#'
#' @param x An object of interest.
#' @param na.last Argument for controlling the treatment of `NA`s. See [sort()].
#'
#' @examples
#' fac_vec <- factor(c("a", "b"), levels = c("a", "b", "c"))
#' levels2(fac_vec)
#'
#' levels2(c(10, 1, 2, NA, 11))
#'
#' @export
levels2 <- function(x, na.last = TRUE) {
  if (identical(levels(x), NULL)) {
    as.character(unique(sort(x, na.last = na.last)))
  } else {
    as.character(levels(x))
  }
}

# Operations with class ---------------------------------------------------
add_class <- function(obj, class_char) {
  class(obj) <- c(class_char, class(obj))

  obj
}

add_class_cond <- function(x, class) {
  if (class(x)[1] != class) {
    class(x) <- c(class, class(x))
  }

  x
}

reconstruct <- function(new, old) {
  class(new) <- class(old)

  new
}


# Competition results -----------------------------------------------------
get_formatC_width <- function(vec) {
  floor(log10(length(unique(vec)))) + 1
}

renamecreate_columns <- function(df, info, fill = NA_integer_) {
# info is a data.frame that should consist from two columns:
  # target - names of target columns (which will be repaired into);
  # original - names of original columns (which will be repaired from).
    # If original is NA then new column with corresponded target name is
    #created with values from 'fill'.
  res <- df
  absent_original <- is.na(info$original)
  if (any(absent_original)) {
    res[, info$target[absent_original]] <- rep(list(rep(fill, nrow(df))))
  }
  if (any(!absent_original)) {
    colnames(res)[match(info$original[!absent_original], colnames(res))] <-
      info$target[!absent_original]
  }

  res
}

reduce_full_join <- function(x, by) {
  reduce_f <- function(x, y) {
    full_join(x = x, y = y, by = by)
  }

  Reduce(f = reduce_f, x = x)
}


# Convertion between long data and matrix ---------------------------------
#' Convert between long pair-value data and matrix
#'
#' Functions for convertion between long data (data frame with columns for pair
#' identifiers and value column) and matrix.
#'
#' @param tbl Data frame with pair-value data.
#' @param row_key String name of column for first key in pair.
#' @param col_key String name of column for second key in pair.
#' @param value String name of column for value (or `NULL` for `long_to_mat()`).
#' @param fill Value to fill for missing pairs.
#' @param silent Use `TRUE` to omit message about guessed value column (see
#'   Details).
#' @param mat Matrix with pair-value data.
#' @param drop Use `TRUE` to drop rows with "missing" values (see Details).
#'
#' @details Pair-value data is commonly used in description of pairs of objects.
#' Pair is described by two keys (usually integer or character) and value is an
#' object of arbitrary nature.
#'
#' In __long format__ there are at least three columns: for first key in pair,
#' for second and for value (might be more). In __matrix format__ pair-value
#' data is represented as matrix of values with row names as character
#' representation of first key, column names - second key.
#'
#' `long_to_mat()` works as follows:
#' - Pair identifiers are taken from columns with names `row_key` (to be used as
#' row names) and `col_key` (to be used as column names). Unique identifiers
#' (and future dimension names) are determined with [levels2()]. __Note__ that
#' `NA`s are treated as single unknown key and put on last place.
#' - Values are taken from column with name `value`. __Note__ that if `value` is
#' `NULL` then `long_to_mat()` will take first non-key column. If there is no
#' such column, it will use vector of dummy values (`NA`s or `fill`s). In both
#' cases a message is given if `silent = FALSE`.
#' - Output is a matrix with described row and column names. Value of pair
#' "key_1" and "key_2" is stored at intersection of row "key_1" and "key_2".
#' __Note__ that in case of duplicated pairs the value from first occurance is
#' taken.
#'
#' `mat_to_long()` basically performs inverse operation to `long_to_mat()`. If
#' `drop = TRUE` it drops rows with values (but not keys) being missing.
#'
#' @return `long_to_mat()` returns a matrix with selected values where row names
#'   indicate first key in pair, col names - second.
#'
#' `mat_to_long()` returns a `tibble` with three columns: the
#' one for first key in pair, the one for second, and the one for value.
#'
#' @examples
#' long_data <- data.frame(
#'   key_1 = c("a", "a", "b"),
#'   key_2 = c("c", "d", "c"),
#'   val = 1:3,
#'   stringsAsFactors = FALSE
#' )
#'
#' mat_data <- long_data %>% long_to_mat("key_1", "key_2", "val")
#' print(mat_data)
#'
#' # Converts to tibble
#' mat_data %>% mat_to_long("new_key_1", "new_key_2", "new_val")
#'
#' # Not drops rows with missing values
#' mat_data %>% mat_to_long("new_key_1", "new_key_2", "new_val")
#'
#' @name convert-pair-value
NULL

#' @rdname convert-pair-value
#' @export
long_to_mat <- function(tbl, row_key, col_key, value = NULL,
                        fill = NULL, silent = FALSE) {
  assert_single_string(row_key, col_key)
  row <- tbl[[row_key]]
  col <- tbl[[col_key]]

  if (identical(value, NULL)) {
    val_name <- tbl %>%
      select(-one_of(row_key, col_key)) %>%
      first_col_name(silent = silent)

    if (identical(val_name, NULL)) {
      mat_elem <- miss_value(NULL, fill)
      val <- rep(mat_elem, nrow(tbl))
    } else {
      val <- tbl[[val_name]]
      mat_elem <- miss_value(class(val), fill)
    }
  } else {
    assert_single_string(value)
    val <- tbl[[value]]
    mat_elem <- miss_value(class(val), fill)
  }

  row_names <- levels2(row, na.last = TRUE)
  col_names <- levels2(col, na.last = TRUE)
  res <- matrix(
    mat_elem, nrow = length(row_names), ncol = length(col_names),
    dimnames = list(row_names, col_names)
  )
  # For repairing in case `val` is list
  res_attrs <- attributes(res)

  # Used to handle NAs
  row_inds <- match(as.character(row), row_names)
  col_inds <- match(as.character(col), col_names)
  # rev() is used to pick first value in case of duplicated pair
  res[cbind(rev(row_inds), rev(col_inds))] <- rev(val)
  attributes(res) <- res_attrs

  res
}

#' @rdname convert-pair-value
#' @export
mat_to_long <- function(mat, row_key, col_key, value, drop = FALSE) {
  assert_single_string(row_key, col_key, value)

  rows <- rep(rownames(mat), each = ncol(mat))
  cols <- rep(colnames(mat), times = nrow(mat))
  vals <- c(t(mat))

  res <- tibble(
    !! enexpr(row_key) := rows,
    !! enexpr(col_key) := cols,
    !! enexpr(value) := vals
  )

  if (isTRUE(drop)) {
    res <- res %>% tidyr::drop_na(one_of(value))
  }

  res
}

first_col_name <- function(tbl, silent = FALSE, target_name = "value") {
  if (ncol(tbl) == 0) {
    res <- NULL
  } else {
    res <- colnames(tbl)[1]
  }

  if (!silent) {
    assert_used_value_col(res, target_name)
  }

  res
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


# Assertions --------------------------------------------------------------
assert_single_string <- function(...) {
  dots <- quos(..., .named = TRUE)

  for (nm in names(dots)) {
    value <- rlang::eval_tidy(dots[[nm]])
    if (!(is.character(value) && length(value) == 1)) {
      stop("`", nm, "` should be a single string.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

assert_used_value_col <- function(x, target_name = "value") {
  if (length(x) == 0) {
    message("No ", target_name, " found. Using dummy vector.")
  } else {
    message("Using ", x, " as ", target_name, ".")
  }

  invisible(TRUE)
}

assert_used_names <- function(info, prefix = "") {
  # info is a data.frame that should consist from two columns:
  # target - names of used columns;
  # original - names of original columns.
  absent_original <- is.na(info$original)

  target <- info$target[!absent_original]
  original <- info$original[!absent_original]
  if (any(!absent_original) && any(target != original)) {
    unmatched <- target != original
    used_names_message <-
      paste0(original[unmatched], " -> ", target[unmatched], collapse = "\n  ")
    message(
      prefix, "Some matched names are not perfectly matched:\n  ",
      used_names_message, "\n"
    )
  }

  if (any(absent_original)) {
    message(
      prefix,
      sprintf(
        "Next columns are not found. Creating with NAs.\n  %s",
        paste0(info$target[absent_original], collapse = ", ")
      ),
      "\n"
    )
  }

  invisible(TRUE)
}
