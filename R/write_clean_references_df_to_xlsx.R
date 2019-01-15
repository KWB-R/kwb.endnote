#' Write Clean References Dataframe to XLSX
#'
#' @param endnote_list list created with create_endnote_list()
#' @param file file where to save (default: "references_clean.xlsx")
#' @param replace_na if TRUE NAs are replaced with 'replace_na_text' (
#' default: FALSE)
#' @param dbg show debug messages (default: TRUE)
#' @param ... additional arguments passed to openxlsx::write.xlsx()
#' publication type
#' @export
#' @importFrom dplyr select_if arrange desc
#' @importFrom openxlsx write.xlsx
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' write_clean_references_df_to_xlsx(endnote_list)
#' }
write_clean_references_df_to_xlsx <- function(endnote_list,
   file = default_clean_xlsx(endnote_list),
                                              replace_na = FALSE,
                                              dbg = TRUE,
                                              ...) {
  refs_clean_df <- clean_references_df(endnote_list,
                                       replace_na = replace_na,
                                       dbg = dbg)

  refs_list_by_pubtype <- create_list_by_pubtype_from_df(refs_clean_df)

  unique_entries_list <- create_list_with_unique_entries(refs_clean_df)


  refs_list <- c(refs_list_by_pubtype, unique_entries_list)


  openxlsx::write.xlsx(refs_list, file, ...)
}
