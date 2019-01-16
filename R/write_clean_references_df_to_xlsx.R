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
   file = default_clean_xlsx(endnote_list), replace_na = FALSE, dbg = TRUE, ...
) {

  write_references(endnote_list, file, clean = TRUE, replace_na, dbg, ...)
}
