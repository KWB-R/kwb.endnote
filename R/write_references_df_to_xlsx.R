#' Write References Dataframe to XLSX
#'
#' @param refs_df references dataframe as retrieved by create_references_df()
#' @param file file where to save (default: "references.xlsx")
#' @param ... additional arguments passed to openxlsx::write.xlsx()
#' @return write references dataframe to xlsx with one sheet for each
#' publication type
#' @export
#' @importFrom dplyr select_if arrange desc
#' @importFrom openxlsx write.xlsx
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' write_references_df_to_xlsx(refs_df)
#' }
write_references_df_to_xlsx <- function(refs_df,
                                        file = "references.xlsx",
                                        ...) {
  pub_types <- unique(refs_df$ref_type_name)
  refs_df_list <- lapply(pub_types, function(x) {
    refs_df[refs_df$ref_type_name == x, ] %>%
      dplyr::select_if(~sum(!is.na(.)) > 0) %>%
      dplyr::mutate(rec_number = as.numeric(rec_number)) %>%
      dplyr::arrange(dplyr::desc(.data$rec_number))
  })

  refs_df_list <- c(list(refs_df),refs_df_list)

  refs_df_list <- stats::setNames(refs_df_list, c("ALL", pub_types))

  openxlsx::write.xlsx(refs_df_list, file, ...)
}

