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
write_references_df_to_xlsx <- function(
  endnote_list, file = "references.xlsx", ...
) {

  write_references(endnote_list, file, clean = FALSE, replace_na, dbg, ...)
}
