#' Create List with Unique Entries
#'
#' @param refs_df data frame as created with create_references_df()
#' @return list with unique values for selected columns
#' @export
#' @importFrom dplyr count arrange
#' @importFrom rlang quo_name :=
#' @importFrom stats setNames
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' unique_entries_list <- create_list_with_unique_entries(refs_df)
#' str(unique_entries_list, 1)
#' }
create_list_with_unique_entries <- function(refs_df) {

  select_columns <- function(pattern) {
    columns <- unique(stringr::str_extract(names(refs_df), pattern))
    columns[! is.na(columns)]
  }

  tidy_df <- function(columns) {
    refs_df[, columns] %>%
      tidyr::gather(key = "key", value = "value") %>%
      dplyr::filter(! is.na(.data$key)) %>%
      dplyr::count(.data$value)
  }

  get_unique_entries <- function(column) {
    refs_df %>%
      dplyr::count(.data[[column]]) %>%
      dplyr::arrange(.data[[column]])
  }

  get_unique_multi_entries <- function(column) {
    select_columns(sprintf("^%s[0-9][0-9]", column)) %>%
      tidy_df() %>%
      dplyr::rename(!!rlang::quo_name(column) := .data$value)
  }

  ignore_cols <-  c(
    select_columns(".*[0-9][0-9].*"),
    "rec_number", "ref_type", "database_path", "database_name"
  )

  columns_single <- setdiff(names(refs_df), ignore_cols)
  columns_multi <- c("author", "author_secondary", "author_tertiary", "keyword")

  c(
    lapply(stats::setNames(nm = columns_single), get_unique_entries),
    lapply(stats::setNames(nm = columns_multi), get_unique_multi_entries)
  )
}
