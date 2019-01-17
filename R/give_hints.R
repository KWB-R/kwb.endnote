#' Helper Function: Give Hints For Project Names
#'
#' @param project_names vector with project names to check
#' @param dbg should debug messages be printed (default: TRUE)
#' @importFrom stringr str_detect
#' @return vector with project_names with hints how to improve data quality (in
#' case give_hints = TRUE)
#' @export
give_hints_project_names <- function(project_names, dbg = TRUE) {
  kwb.utils::catAndRun(
    "Generate hints for 'Project Names'",
    expr = {
      replace_na_with_value(project_names, "add_project_name")
    }
  )
}


get_indices_multiple_authors <- function(author_names) {
  which(unlist(lapply(author_names, function(x)
    length(stringr::str_split(x, pattern = ",")[[1]]))) > 2)
}

get_indices_firstname_lastname <-  function(author_names) {
  which(
  stringr::str_detect(author_names, "^\\s?\\w+\\.?\\s+\\w+") &
    !stringr::str_detect(author_names, ",")
)}


get_indices_ends_with_semicolon <- function(author_names) {
  which(author_names %>%
  stringr::str_trim() %>%
  stringr::str_detect(".*,$"))
}

#' Helper Function: Give Hints For Author Names
#'
#' @param author_names vector with author names to check
#' @param dbg should debug messages be printed (default: TRUE)
#' @importFrom kwb.utils catAndRun
#' @importFrom stringr str_detect
#' @return vector with author_names with hints how to improve data quality (in
#' case give_hints = TRUE)
#' @export
give_hints_author_names <- function(author_names, dbg = TRUE) {
  kwb.utils::catAndRun(
    "Generate hints for 'Author Names'",
    dbg = dbg,
    expr = {
      multiple_authors_idx <- get_indices_multiple_authors(author_names)
      firstname_lastname_idx <- get_indices_firstname_lastname(author_names)
      ends_with_semicolon_idx <- get_indices_ends_with_semicolon(author_names)


      author_fullname$value %>%
      replace_indices_with_value(multiple_authors_idx,
                                 "fix_multiple_authors_per_line",
                                 dbg) %>%
      replace_indices_with_value(firstname_lastname_idx,
                                 paste0("fix_firstname_lastname_with_",
                                 "lastname_semicolon_firstname"),
                                 dbg) %>%
      replace_indices_with_value(ends_with_semicolon_idx,
                                 "fix_author_name_by_deleting_last_semicolon",
                                 dbg) %>%
      replace_na_with_value("add_author_lastname_semicolon_firstname",
                            dbg)
    }
  )
}


#' Helper Function: Give Hints For Accessibility
#'
#' @param access vector with accessibility metadata to check
#' @param dbg should debug messages be printed (default: TRUE)
#' @importFrom kwb.utils catAndRun
#' @return vector with access info with hints how to improve data quality (in
#' case give_hints = TRUE)
#' @export
give_hints_accessiblity <- function(access, dbg = TRUE) {
  kwb.utils::catAndRun(
    "Generate hints for 'Accessiblity'",
    expr = {
      replace_na_with_value(access, "add_public_or_confidential")
    }
  )
}
