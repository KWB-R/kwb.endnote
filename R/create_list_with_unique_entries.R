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

  get_unique_entries <- function(column) {
    dplyr::count(refs_df, .data[[column]]) %>%
    dplyr::arrange(.data[[column]])
   }

  select_columns <- function(pattern) {
    extract_cols <- unique(stringr::str_extract(names(refs_df), pattern))
    extract_cols <- extract_cols[!is.na(extract_cols)]
    extract_cols
  }

  tidy_df <- function(columns) {
    refs_df[,columns] %>%
    tidyr::gather(key = "key", value = "value") %>%
    dplyr::filter(!is.na(.data$key)) %>%
    dplyr::count(.data$value)

  }

  get_unique_multi_entries <- function(column = "author") {

    selected_cols <- select_columns(sprintf("^%s[0-9][0-9]", column))

    tidy_df(selected_cols) %>%
    dplyr::rename(!!rlang::quo_name(column) := .data$value)

  }

  multi_entry_cols <- c("author",
                        "author_secondary",
                        "author_tertiary",
                        "keyword")

  unique_entries_multi <- lapply(multi_entry_cols, get_unique_multi_entries)

  unique_entries_multi <- stats::setNames(unique_entries_multi, nm =multi_entry_cols)

  ignore_cols <- select_columns(".*[0-9][0-9].*")


  ignore_cols <-  c(ignore_cols, "rec_number", "ref_type", "database_path",
                    "database_name")

  columns <- setdiff(names(refs_df), ignore_cols)

  unique_entries <- lapply(columns, get_unique_entries)

  unique_entries <- stats::setNames(unique_entries, nm = columns)





  ignore_cols <- ignore_cols[!is.na(ignore_cols)]

  c(unique_entries, unique_entries_multi)

}
