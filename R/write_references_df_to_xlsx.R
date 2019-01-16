#' Write References Dataframe to XLSX
#'
#' @param endnote_list list created with create_endnote_list()
#' @param file name of file to save (default: default_xlsx(endnote_list))
#' @param export_dir directory where to save 'file' (default: ".")
#' @param dbg show debug messages (default: TRUE)
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
write_references_df_to_xlsx <- function(
  endnote_list, file = default_xlsx(endnote_list), export_dir = ".",
  dbg = TRUE, ...
) {

  write_references(endnote_list, file, export_dir, clean = FALSE, dbg = dbg, ...)
}
