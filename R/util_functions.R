#' @title Get R package information
#' @description This function returns a tibble with information about the R packages that are currently attached to the R session.
#' @return A tibble with the following columns: package, version, and date.
#' @examples
#' get_r_package_info()
#'

get_r_package_info <- function() {

  r_package_table <- sessioninfo::package_info()
  rownames(r_package_table) <- NULL

  r_package_table <- r_package_table |>
    tibble::as_tibble() |>
    dplyr::mutate(
      version = ifelse(is.na(r_package_table$loadedversion),
                       r_package_table$ondiskversion,
                       r_package_table$loadedversion)) |>
    dplyr::filter(.data[["attached"]] == TRUE) |>
    dplyr::select(
      dplyr::any_of(c("package", "version",
                      "date"))
    )

  return(r_package_table)
}

