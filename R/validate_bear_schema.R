# Validate the stable schema and categories of the combined BEAR dataset.

bear_schema_columns <- c(
  "dataset", "metaid", "studyid", "method", "measure", "subset", "z",
  "b", "se", "year", "topic", "ss", "z_operator", "effect_scale", "p",
  "outcome_group", "orig.z", "orig.z_operator", "orig.p",
  "orig.b", "orig.se", "orig.ss", "source"
)

check_bear_schema <- function(bear) {
  if (!is.data.frame(bear))
    stop("BEAR must be a data frame.", call. = FALSE)

  if (!identical(names(bear), bear_schema_columns))
    stop("BEAR columns do not match the expected schema.", call. = FALSE)

  character_columns <- c(
    "dataset", "metaid", "studyid", "method", "measure", "subset", "topic",
    "z_operator", "effect_scale", "outcome_group", "source"
  )
  numeric_columns <- c(
    "z", "b", "se", "year", "ss", "p", "orig.z", "orig.p", "orig.b",
    "orig.se", "orig.ss"
  )

  if (!all(vapply(bear[character_columns], is.character, logical(1))))
    stop("BEAR character columns have unexpected types.", call. = FALSE)
  if (!all(vapply(bear[numeric_columns], is.numeric, logical(1))))
    stop("BEAR numeric columns have unexpected types.", call. = FALSE)
  if (any(vapply(bear, is.list, logical(1))))
    stop("BEAR must not contain list columns.", call. = FALSE)

  for (column in c("measure", "method", "effect_scale")) {
    allowed <- get(paste0("bear_", column, "_levels"))
    if (any(!is.na(bear[[column]]) & !bear[[column]] %in% allowed))
      stop("BEAR contains an unrecognised ", column, ".", call. = FALSE)
  }

  for (column in c("z_operator", "orig.z_operator")) {
    if (any(!is.na(bear[[column]]) &
           !bear[[column]] %in% c("<", "=", ">")))
      stop("BEAR contains an unrecognised ", column, ".", call. = FALSE)
  }
  if ("field" %in% names(bear))
    stop("BEAR must not contain the obsolete field column.", call. = FALSE)
  if (!"topic" %in% names(bear))
    stop("BEAR must contain topic.", call. = FALSE)
  if (!all(bear$dataset %in% names(bear_names)))
    stop("BEAR contains an unrecognised dataset.", call. = FALSE)

  invisible(TRUE)
}
