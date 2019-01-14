#' Create List By Pub Type From Datafram
#'
#' @param refs_df data frame as created with create_references_df()
#' @return list with references with one sublist for each publication type
#' @export
#' @importFrom dplyr select_if mutate arrange desc
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' refs_list_by_pubtype <- create_list_by_pubtype_from_df(refs_df)
#' str(refs_list_by_pubtype, 1)
#' }
create_list_by_pubtype_from_df <- function(refs_df) {
  pub_types <- unique(refs_df$ref_type_name)
  refs_df_list <- lapply(pub_types, function(x) {
    refs_df[refs_df$ref_type_name == x, ] %>%
      dplyr::select_if(~sum(!is.na(.)) > 0) %>%
      dplyr::mutate(rec_number = as.numeric(.data$rec_number)) %>%
      dplyr::arrange(dplyr::desc(.data$rec_number))
  })

  refs_df_list <- c(list(refs_df),refs_df_list)

  refs_df_list <- stats::setNames(refs_df_list, c("ALL", pub_types))

  refs_df_list
}
