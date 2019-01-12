#' Helper function: clean DOIs
#'
#' @param dois vectors with DOIs to clean
#' @return cleaned DOIs
#' @export
#' @importFrom stringr str_remove_all regex str_trim
#' @importFrom dplyr bind_cols
clean_dois <- function(dois) {

  dois %>%
    stringr::str_remove_all("^http(s)?://") %>%
    stringr::str_remove_all("^(dx\\.)?doi\\.org/") %>%
    stringr::str_remove_all(stringr::regex("^doi:?\\s?",
                                           ignore_case = TRUE)) %>%
    stringr::str_trim()

}


#' Clean References Dataframe
#'
#' @param references_df as retrieved by create_references_df()
#' @return cleaned references_df
#' @export
#' @importFrom dplyr mutate
clean_references_df <- function(references_df) {

  references_df %>%
    dplyr::mutate(electronic_resource_num = clean_dois(.data$electronic_resource_num))


}
