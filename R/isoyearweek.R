new_isoyearweek <- function(x = integer()) {
  vctrs::vec_assert(x, integer())
  vctrs::new_vctr(x, class = "fhi_isoyearweek")
}

#' `isoyearweek` vector
#'
#' This creates a double vector that represents percentages so when it is
#' printed, it is multiplied by 100 and suffixed with `%`.
#'
#' @param x A numeric vector
#' @return An S3 vector of class `fhi_isoyearweek`.
#' @export
#' @examples
#' isoyearweek("2020-01")
isoyearweek <- function(x = integer()) {
  if (vctrs::vec_is(x, vctrs::new_date())) {
    x <- as_isoyearweek.Date(x)
  } else if (vctrs::vec_is(x, character())) {
    x <- as_isoyearweek.character(x)
  }
  x <- vctrs::vec_cast(x, integer())
  new_isoyearweek(x)
}

#' is_isoyearweek
#' @param x a
#' @export
is_isoyearweek <- function(x) {
  inherits(x, "fhi_isoyearweek")
}

#' @export
vec_ptype_abbr.fhi_isoyearweek <- function(x, ...) {
  "yrwk"
}

#' @export
format.fhi_isoyearweek <- function(x, ...) {
  yrwks[vctrs::vec_data(x)]
}

#' as_isoyearweek
#' @param x Variable
#' @export
as_isoyearweek <- function(x) {
  UseMethod("as_isoyearweek")
}

#' @export
as_isoyearweek.default <- function(x) {
  vctrs::vec_cast(x, new_isoyearweek())
}

#' @export
as_isoyearweek.Date <- function(x) {
  x <- format(x, "%G-%V")
  x <- unlist(lapply(x, function(x) which(yrwks %in% x)))
  isoyearweek(x)
}

#' @export
as_isoyearweek.character <- function(x) {
  if (sum(stringr::str_detect(x, "^[0-9][0-9][0-9][0-9]-[0-9][0-9]$"), na.rm = T) > 0) {
    # already in correct format
    x <- unlist(lapply(x, function(x) which(yrwks %in% x)))
    return(isoyearweek(x))
  } else if (sum(stringr::str_detect(x, "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$"), na.rm = T) > 0) {
    # in date format
    x <- as.Date(x)
    return(as_isoyearweek.Date(x))
  } else {
    return(rep(NA, length = length(x)))
  }
}

# types ----
#' @export
vec_ptype2.fhi_isoyearweek.fhi_isoyearweek <- function(x, y, ...) new_isoyearweek()

#' @export
vec_ptype2.fhi_isoyearweek.integer <- function(x, y, ...) integer()

#' @export
vec_ptype2.integer.fhi_isoyearweek <- function(x, y, ...) integer()

# casting ----
#' @export
vec_cast.fhi_isoyearweek.fhi_isoyearweek <- function(x, to, ...) x

#' @export
vec_cast.fhi_isoyearweek.integer <- function(x, to, ...) isoyearweek(x)

#' @export
vec_cast.integer.fhi_isoyearweek <- function(x, to, ...) vctrs::vec_data(x)

#' @export
vec_cast.fhi_isoyearweek.double <- function(x, to, ...) isoyearweek(as.integer(x))

#' @export
vec_cast.double.fhi_isoyearweek <- function(x, to, ...) vctrs::vec_data(x)

#' @export
vec_cast.fhi_isoyearweek.character <- function(x, to, ...) as_isoyearweek.character(x)

#' @export
vec_cast.character.fhi_isoyearweek <- function(x, to, ...) format.fhi_isoyearweek(x)

# arithmetic ----
#' artihmetic
#' @param op x
#' @param x x
#' @param y x
#' @param ... x
#' @export
vec_arith.fhi_isoyearweek <- function(op, x, y, ...) {
  UseMethod("vec_arith.fhi_isoyearweek", y)
}

#' @export
vec_arith.fhi_isoyearweek.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.fhi_isoyearweek.fhi_isoyearweek <- function(op, x, y, ...) {
  switch(
    op,
    "-" = as.integer(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.numeric.fhi_isoyearweek <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = isoyearweek(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.fhi_isoyearweek.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = isoyearweek(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.character.fhi_isoyearweek <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = isoyearweek(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.fhi_isoyearweek.character <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = isoyearweek(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
seq.fhi_isoyearweek <- function(
                                from,
                                to,
                                by,
                                length.out = NULL,
                                along.with = NULL,
                                ...) {
  retval <- seq(
    as.numeric(isoyearweek(from)),
    as.numeric(isoyearweek(to)),
    1
  )
  isoyearweek(retval)
}


methods::setOldClass(c("fhi_isoyearweek"))
