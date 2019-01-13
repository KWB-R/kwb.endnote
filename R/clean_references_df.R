#' Helper function: clean DOIs
#'
#' @param dois vectors with DOIs to clean
#' @return cleaned DOIs
#' @export
#' @importFrom stringr str_remove_all regex str_trim
clean_dois <- function(dois) {

  dois %>%
    stringr::str_remove_all("^http(s)?://") %>%
    stringr::str_remove_all("^(dx\\.)?doi\\.org/") %>%
    stringr::str_remove_all(stringr::regex("^doi:?\\s?",
                                           ignore_case = TRUE)) %>%
    stringr::str_trim()

}


#' Helper function: clean project names
#'
#' @param  vector with project names to clean
#' @return vector with cleaned project names
#' @export
#' @importFrom stringr str_remove_all str_replace_all regex str_trim
clean_project_names <- function(project_names) {

  project_names %>%
    stringr::str_replace_all("\\s+?/", ",") %>%
    stringr::str_remove_all("-") %>%
    stringr::str_remove_all("_.*") %>%
    stringr::str_replace_all(stringr::regex("^techneau.*",
                                            ignore_case = TRUE),
                             "TECHNEAU") %>%
    stringr::str_remove_all("\\s+?") %>%
    stringr::str_trim() %>%
    tolower()

}

if(FALSE) {
  stringr::str_split(clean_project_names(project_names),
                     ",",
                     simplify = TRUE) %>%
    tibble::as.tibble() %>%
    dplyr::mutate_if(is.character, list(~dplyr::na_if(., NA_character_)))
}

#' Clean References Dataframe
#'
#' @param references_df as retrieved by create_references_df()
#' @return cleaned references_df
#' @export
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' refs_clean_df <- clean_references_df(refs_df)
#' head(refs_clean_df)
#' }
clean_references_df <- function(references_df) {

  references_df %>%
    dplyr::mutate(electronic_resource_num = clean_dois(.data$electronic_resource_num),
                  custom2 = clean_project_names(.data$custom2))

}

