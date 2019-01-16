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
#' @param replace_na if TRUE NAs are replaced with 'replace_na_text' (
#' default: FALSE)
#' @param replace_na_text text for replacing NAs (default: "add_project_name")
#' @param dbg show debug messages (default: TRUE)
#' @return vector with cleaned project names
#' @export
#' @importFrom stringr str_remove_all str_replace_all regex str_trim
#' @importFrom kwb.utils catAndRun
clean_project_names <- function(
  project_names, replace_na = FALSE, replace_na_text = "add_project_name",
  dbg = TRUE
) {

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

    if (replace_na) {
      project_names[is.na(project_names)] <- replace_na_text
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
#' @param replace_na if TRUE NAs are replaced with 'replace_na_text' (
#' default: FALSE)
#' @param replace_na_text text for replacing NAs (default:
#' "add_author_lastname_semicolon_firstname"")
#' @param dbg show debug messages (default: TRUE)
#' @return vector with cleaned author names
#' @export
#' @importFrom stringr str_detect
#' @importFrom kwb.utils catAndRun
clean_author_names <- function(
  author_names, replace_na = FALSE,
  replace_na_text = "add_author_lastname_semicolon_firstname", dbg = TRUE
) {

  kwb.utils::catAndRun("Clean and Check 'Author Names'", dbg = dbg, expr = {

    multiple_authors_idx <- which(
      unlist(lapply(author_names, function(x)
        length(stringr::str_split(x, pattern = ",")[[1]])
      )) > 2
    )

    author_names[multiple_authors_idx] <- "fix_multiple_authors_per_line"

    firstname_lastname_idx <- which(
      stringr::str_detect(author_names, "^\\s?\\w+\\.?\\s+\\w+") &
        ! stringr::str_detect(author_names, ",")
    )

    author_names[firstname_lastname_idx] <- c(
      "fix_firstname_lastname_with_lastname_semicolon_firstname"
    )

    # author_names <- author_names %>%
    #   stringr::str_trim()

    if (replace_na) {

      author_names[is.na(author_names)] <- replace_na_text
    }

    author_names
  })
}

#' Helper function: clean access information
#'
#' @param access vector with accessibility information
#' @param replace_na if TRUE NAs are replaced with 'replace_na_text' (
#' default: FALSE)
#' @param replace_na_text text for replacing NAs (default: "add_public_or_confidential")
#' @param dbg show debug messages (default: TRUE)
#' @return vector with cleaned accessibility information
#' @export
#' @importFrom stringr str_remove_all str_replace_all regex str_trim
clean_accessibility <- function(
  access, replace_na = FALSE, replace_na_text = "add_public_or_confidential",
  dbg = TRUE
) {

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

    if (replace_na) {
      access[is.na(access)] <- replace_na_text
    }

    access
  })
}

#' Clean References Dataframe
#'
#' @param endnote_list list created with create_endnote_list()
#' @param replace_na if TRUE NAs are replaced with 'replace_na_text' (
#' default: FALSE) defined in relevant helper function
#' @param dbg show debug messages (default: TRUE)
#' @return cleaned references_df
#' @export
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_clean_df <- clean_references_df(endnote_list)
#' head(refs_clean_df)
#' }
clean_references_df <- function(endnote_list, replace_na = FALSE, dbg = TRUE) {

  refs_df <- create_references_df(endnote_list,collapse = TRUE)

  refs_df <- kwb.utils::catAndRun(

    messageText = "\nClean and Check 'Author Names'", dbg = dbg, expr = {

      is_author <- stringr::str_detect(names(refs_df), "author")

      col_authors <- names(refs_df)[is_author]

      for (col_author in col_authors) {
        replace_author_na <- (col_author == "author01")
        refs_df[[col_author]]  <- clean_author_names(
          refs_df[[col_author]], replace_na = replace_author_na, dbg = FALSE
        )
      }

      refs_df
    }
  )

  refs_df %>% dplyr::mutate(
    electronic_resource_num = clean_dois(.data$electronic_resource_num, dbg = dbg),
    custom2 = clean_project_names(.data$custom2, replace_na, dbg = dbg),
    custom3 = clean_accessibility(.data$custom3, replace_na, dbg = dbg))
}
