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
      multiple_authors_idx <- which(unlist(lapply(author_names, function(x)
        length(stringr::str_split(x, pattern = ",")[[1]]))) > 2)

      firstname_lastname_idx <- which(
        stringr::str_detect(author_names, "^\\s?\\w+\\.?\\s+\\w+") &
          !stringr::str_detect(author_names, ",")
      )

      if (length(multiple_authors_idx) > 0) {
        author_names[multiple_authors_idx] <- "fix_multiple_authors_per_line"
      }

      if (length(firstname_lastname_idx) > 0) {
        author_names[firstname_lastname_idx] <- paste0(
          "fix_firstname_lastname_with_",
          "lastname_semicolon_firstname"
        )
      }

      replace_na_with_value(author_names, "add_author_lastname_semicolon_firstname")
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
