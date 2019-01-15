#' Check Endnote for Problematic References
#'
#' @param endnote_list list created with create_endnote_list()
#' @param replace_na if TRUE NAs are replaced with 'replace_na_text' (
#' default: TRUE) defined in relevant helper function
#' @param dbg show debug messages (default: TRUE)
#' @return a data frame with problematic entries
#' @export
#' @importFrom kwb.utils catAndRun
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' problematic_entries <- check_problematic_entries(endnote_list)
#' head(problematic_entries)
#' }
check_problematic_entries <- function(
  endnote_list, replace_na = TRUE, dbg = TRUE) {

  entries_org <- kwb.utils::catAndRun(
    sprintf("Creating data frame from '%s'",
            deparse(substitute(endnote_list))),
    dbg = dbg,
    expr = create_references_df(endnote_list)
  )

  entries_cleaned <- kwb.utils::catAndRun(
    sprintf("Creating 'cleaned' data frame from '%s' for comparison",
            deparse(substitute(endnote_list))),
    dbg = dbg,
    expr = clean_references_df(endnote_list, replace_na, dbg)
  )

  has_problems <- ! sapply(names(entries_org), function(col_name) {
    identical(entries_org[[col_name]], entries_cleaned[[col_name]])
  })

  cols_with_problems <- names(which(has_problems))

  check_list <- lapply(cols_with_problems, function(column) {

    indices <- which(! sapply(seq_len(nrow(entries_org)), function (i) {
      identical(entries_org[[column]][i],entries_cleaned[[column]][i])}))

    tibble::tibble(
      rec_number = entries_org[["rec_number"]][indices],
      key = column,
      value_org = entries_org[[column]][indices],
      value_clean = entries_cleaned[[column]][indices]
    )
  })

  dplyr::bind_rows(check_list) %>%
    dplyr::mutate(rec_number = as.numeric(.data$rec_number)) %>%
    dplyr::arrange(.data$rec_number, .data$key)
}
