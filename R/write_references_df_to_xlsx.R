#' Write References Dataframe to XLSX
#'
#' @param endnote_list list created with create_endnote_list()
#' @param file file where to save (default: "references.xlsx")
#' @param ... additional arguments passed to openxlsx::write.xlsx()
#' @return write references dataframe to xlsx with one sheet for each
#' publication type
#' @export
#' @importFrom dplyr select_if arrange desc
#' @importFrom openxlsx write.xlsx
#' @importFrom stats setNames
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' write_references_df_to_xlsx(endnote_list)
#' }
write_references_df_to_xlsx <- function(endnote_list,
                                        file = "references.xlsx",
                                        ...) {
  refs_df <- create_references_df(endnote_list) %>%
    dplyr::arrange(dplyr::desc(.data$rec_number))



  refs_list_by_pubtype <- create_list_by_pubtype_from_df(refs_df)

  unique_entries_list <- create_list_with_unique_entries(refs_df)


  refs_list <- c(refs_list_by_pubtype, unique_entries_list)

  openxlsx::write.xlsx(refs_list, file, ...)
}

