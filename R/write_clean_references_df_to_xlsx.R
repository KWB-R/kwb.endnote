#' Write Clean References Dataframe to XLSX
#'
#' @param refs_cleaned_df references dataframe as retrieved by clean_references_df()
#' @param file file where to save (default: "references_clean.xlsx")
#' @param ... additional arguments passed to openxlsx::write.xlsx()
#' publication type
#' @export
#' @importFrom dplyr select_if arrange desc
#' @importFrom openxlsx write.xlsx
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' refs_clean_df <- clean_references_df(refs_df)
#' write_clean_references_df_to_xlsx(refs_clean_df)
#' }
write_clean_references_df_to_xlsx <- function(refs_cleaned_df,
                                              file = "references_clean.xlsx",
                                              ...) {

  write_references_df_to_xlsx(refs_cleaned_df, file, ...)
}
