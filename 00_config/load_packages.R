packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "readr",
  "data.table",
  "lubridate",
  "fixest"
)

invisible(
  lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package not installed: ", pkg)
    }
    library(pkg, character.only = TRUE)
  })
  # add comment
)
