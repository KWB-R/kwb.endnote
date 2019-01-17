#' Helper function: clean DOIs
#'
#' @param dois vectors with DOIs to clean
#' @param dbg show debug messages (default: TRUE)
#' @return cleaned DOIs
#' @export
#' @importFrom stringr str_remove_all regex str_trim
#' @importFrom kwb.utils catAndRun
clean_dois <- function(dois, dbg = TRUE) {
  remove <- stringr::str_remove_all

  kwb.utils::catAndRun("Clean 'DOI'", dbg = dbg, expr = {
    dois %>%
      remove("^http(s)?://") %>%
      remove("^(dx\\.)?doi\\.org/") %>%
      remove(stringr::regex("^doi:?\\s?", ignore_case = TRUE)) %>%
      stringr::str_trim()
  })
}

#' Helper function: clean project names
#'
#' @param project_names with project names to clean
#' @param give_hints if TRUE hints will be generated, e.g. "add_project_name"
#' in case of missing entries (default: FALSE)
#' @param dbg show debug messages (default: TRUE)
#' @return vector with cleaned project names
#' @export
#' @importFrom stringr str_remove_all str_replace_all regex str_trim
#' @importFrom kwb.utils catAndRun
clean_project_names <- function(
                                project_names, give_hints = FALSE, dbg = TRUE) {
  kwb.utils::catAndRun("Clean 'Project Names'", dbg = dbg, expr = {
    project_names <- project_names %>%
      stringr::str_replace_all("\\s+?/", ",") %>%
      stringr::str_remove_all("-") %>%
      stringr::str_remove_all("_.*") %>%
      stringr::str_replace_all(
        stringr::regex("^techneau.*", ignore_case = TRUE), "TECHNEAU"
      ) %>%
      stringr::str_remove_all("\\s+?") %>%
      stringr::str_trim() %>%
      tolower()

    if (give_hints) {
      project_names <- give_hints_project_names(project_names, dbg)
    }

    project_names
  })
}


if (FALSE) {
  clean_project_names(project_names) %>%
    stringr::str_split(",", simplify = TRUE) %>%
    tibble::as.tibble() %>%
    dplyr::mutate_if(is.character, list(~dplyr::na_if(., NA_character_)))
}


#' Helper function: clean author names
#'
#' @param author_names with author names to clean
#' @param give_hints if TRUE hints will be generated, e.g.
#' "fix_multiple_authors_per_line" in case of missing entries (default: FALSE)
#' @param dbg show debug messages (default: TRUE)
#' @return vector with cleaned author names
#' @export
#' @importFrom kwb.utils catAndRun
clean_author_names <- function(
                               author_names, give_hints = FALSE, dbg = TRUE) {
  kwb.utils::catAndRun(
    "No cleaning of author_names implemented yet. Only hints are generated in
    case that user defines 'give_hints = TRUE' (default: FALSE)",
    expr = {},
    dbg = dbg
  )

  if (give_hints) {
    author_names <- give_hints_author_names(author_names, dbg = dbg)
  }

  author_names
}


#' Helper function: clean access information
#'
#' @param access vector with accessibility information
#' @param give_hints if TRUE hints will be generated, e.g.
#'  "add_public_or_confidential" in case of missing entries (default: FALSE)
#' @param dbg show debug messages (default: TRUE)
#' @return vector with cleaned accessibility information
#' @export
#' @importFrom stringr str_remove_all str_replace_all regex str_trim
clean_accessibility <- function(
                                access, give_hints = FALSE, dbg = TRUE) {
  replace_all <- function(string, pattern, replacement) {
    stringr::str_replace_all(
      string = string,
      pattern = stringr::regex(pattern, ignore_case = TRUE),
      replacement = replacement
    )
  }

  kwb.utils::catAndRun("Clean 'Accessibility'", dbg = dbg, expr = {
    access <- access %>%
      replace_all("PU.*", "public") %>%
      replace_all("public.*", "public") %>%
      replace_all("privat.*", "confidential") %>%
      replace_all("intern.*", "confidential") %>%
      replace_all("confident.*", "confidential") %>%
      stringr::str_trim()

    if (give_hints) access <- give_hints_accessiblity(access, dbg)

    access
  })
}

#' Clean References Dataframe
#'
#' @param endnote_list list created with create_endnote_list()
#' @param give_hints if TRUE hints will be generated, e.g.
#'  "add_public_or_confidential" for accessiblity data
#' @param dbg show debug messages (default: TRUE)
#' @return cleaned references_df
#' @export
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_clean_df <- clean_references_df(endnote_list)
#' head(refs_clean_df)
#' }
clean_references_df <- function(endnote_list, give_hints = FALSE, dbg = TRUE) {
  refs_df <- create_references_df(endnote_list, collapse = TRUE)

  refs_df <- kwb.utils::catAndRun(
    messageText = "Check 'Author Names'", dbg = dbg, expr = {
      is_author <- stringr::str_detect(names(refs_df), "author")

      col_authors <- names(refs_df)[is_author]

      for (col_author in col_authors) {
        if (give_hints && col_author == "author01") {
          replace_na_with_value(
            refs_df[[col_author]],
            "add_at_least_one_author_with_lastname_semicolon_firstname"
          )
        }
        refs_df[[col_author]] <- clean_author_names(
          refs_df[[col_author]],
          give_hints = give_hints, dbg = dbg
        )
      }

      refs_df
    }
  )

  refs_df %>% dplyr::mutate(
    electronic_resource_num = clean_dois(.data$electronic_resource_num, dbg = dbg),
    custom2 = clean_project_names(.data$custom2,
      give_hints = TRUE,
      dbg = dbg
    ),
    custom3 = clean_accessibility(.data$custom3,
      give_hints = TRUE,
      dbg = dbg
    )
  )
}
