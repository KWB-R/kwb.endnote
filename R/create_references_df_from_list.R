#' @noRd
#' @keywords internal
null_to_na <- function(x, na_fill = NA_character_) {

  if (is.null(x[[1]])) na_fill else x[[1]]
}

#' Helper function: get abstract from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param collapse should separate fields in "style" be collapsed to one field?
#' (default: FALSE)
#' @return one row abstract data frame
#' @export
#' @importFrom dplyr bind_cols
get_abstract <- function(record_list, collapse = FALSE) {

  abstract <- record_list$record$abstract

  if (is.null(abstract)) {

    return(NA_character_)
  }

  collapse_fields(abstract, collapse, element = 1)
}

#' Helper function: get keywords from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "keyword"
#' @param extract_value extract_value = "keyword"
#' @param collapse should separate fields in "style" be collapsed to one field?
#' (default: FALSE)
#' @return one row keywords data frame
#' @export
#' @importFrom dplyr bind_cols
get_keywords <- function(
  record_list, col_name = "keyword", extract_value = "keywords",
  collapse = FALSE
) {

  get_multi_entry(
    entries = record_list$record[[extract_value]],
    col_name = col_name,
    element = "style",
    collapse = FALSE
  )
}


#' Helper function: get authors from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "author"
#' @param extract_value extract_value = "authors"
#' @param collapse should separate fields in "style" be collapsed to one field?
#' (default: FALSE)
#' @return one row authors data frame
#' @export
#' @importFrom dplyr bind_cols
get_authors <- function(
  record_list, col_name = "author", extract_value = "authors", collapse = FALSE
) {

  get_multi_entry(
    entries = record_list$record$contributors[[extract_value]],
    col_name = col_name,
    element = 1,
    collapse = collapse
  )
}

# get_multi_entry --------------------------------------------------------------
get_multi_entry <- function(entries, col_name, element = NULL, collapse = FALSE) {

  if (is.null(entries)) {

    return(stats::setNames(nm = colname_i(col_name, 1), tibble::tibble(
      value = NA_character_
    )))
  }

  dplyr::bind_cols(lapply(seq_along(entries), function(i) {

    entry <- if (is.null(element)) {

      entries[[i]]

    } else {

      collapse_fields(entries[[i]], collapse, element)

    }

    stats::setNames(nm = colname_i(col_name, i), tibble::tibble(
      value = null_to_na(entry)
    ))
  }))
}

collapse_fields <- function(entries, collapse = TRUE, element) {

  if (is.list(entries) && collapse) {
    paste(collapse = "", lapply(seq_along(entries), function(i) {

      null_to_na(entries[[i]][[element]])
    }))
} else {
  null_to_na(entries[[element]])
}
}

#' Helper function: get secondary authors from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @return one row authors data frame
#' @export
#' @inheritParams get_authors
get_secondary_authors <- function(record_list, collapse = FALSE) {

  get_authors(record_list, "author_secondary", "secondary-authors", collapse)
}

#' Helper function: get tertiary authors from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @return one row authors data frame
#' @export
#' @inheritParams get_authors
get_tertiary_authors <- function(record_list, collapse = FALSE) {

  get_authors(record_list, "author_tertiary", "tertiary-authors", collapse)
}

#' Helper function: get pdfurls from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "url_pdfurls"
#' @param collapse should separate fields in "style" be collapsed to one field?
#' (default: FALSE)
#' @return one row pdfurls data frame
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
get_pdfurls <- function(record_list, col_name = "urls_pdf", collapse = FALSE) {

  get_multi_entry(
    entries = record_list$record$urls$`pdf-urls`,
    col_name = col_name,
    collapse = collapse
  )
}


#' Reference List to Data Frame
#'
#' @param record_list list with one record of create_endnote_list()
#' @param collapse should separate fields in "style" be collapsed to one field?
#' (default: FALSE)
#' @return data frame for record
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
record_list_to_df <- function(record_list,
                              collapse = FALSE) {

  get_record_entry <- function(path) get_list_entry(record_list$record, path)
  get_style <- function(path) {
    collapse_fields(get_record_entry(path), collapse = collapse, element = 1)
  }
  get_titles_style <- function(path) get_style(c("titles", path))
  get_period_style <- function(path) get_style(c("periodical", path))
  get_dates_style <- function(path) get_style(c("dates", path))


  tibble::tibble(
    rec_number = null_to_na(get_record_entry("rec-number")),
    ref_type = as.numeric(null_to_na(get_record_entry("ref-type"))),
    ref_type_name = attr(get_record_entry("ref-type"), which = "name"),
    abstract = get_abstract(record_list, collapse)
  ) %>%
    dplyr::bind_cols(get_keywords(record_list, collapse = collapse)) %>%
    dplyr::bind_cols(get_authors(record_list,collapse = collapse)) %>%
    dplyr::bind_cols(get_secondary_authors(record_list, collapse = collapse)) %>%
    dplyr::bind_cols(get_tertiary_authors(record_list, collapse = collapse)) %>%
    dplyr::bind_cols(
      tibble::tibble(
        database_name = get_record_entry("database")[[1]],
        database_path = attr(get_record_entry("database"), which = "path"),
        title = get_titles_style("title"),
        title_secondary = get_titles_style("secondary-title"),
        title_tertiary = get_titles_style("tertiary-title"),
        periodical_title_full = get_period_style("full-title"),
        periodical_title_abbr = get_period_style("abbr-1"),
        year = get_dates_style("year"),
        pubdates = get_dates_style(c("pub-dates", "date")),
        pages = get_style("pages"),
        volume = get_style("volume"),
        numvols  = get_style("num-vols"),
        notes = get_style("notes"),
        section = get_style("section"),
        publisher = get_style("publisher"),
        publocation = get_style("pub-location"),
        puborig = get_style("orig-pub"),
        caption = get_style("caption"),
        authaddress = get_style("auth-address"),
        edition = get_style("edition"),
        worktype = get_style("work-type"),
        label = get_style("label"),
        isbn = get_style("isbn"),
        language = get_style("language")
      )
    ) %>%
    dplyr::bind_cols(get_pdfurls(record_list, collapse = collapse)) %>%
    dplyr::bind_cols(tibble::tibble(
      urls_related = get_style(c("urls", "related-urls", "url")),
      electronic_resource_num = get_style("electronic-resource-num"),
      custom1 = get_style("custom1"),
      custom2 = get_style("custom2"),
      custom3 = get_style("custom3"),
      custom7 = get_style("custom7")
    ))
}

#' Create References Dataframe
#'
#' @param endnote_list list created with create_endnote_list()
#' @param collapse should separate fields in "style" be collapsed to one field?
#' (default: FALSE)
#' @return data.frame with columns record_id, rec_number, ref_type_id, ref_type_name
#' @export
#' @importFrom dplyr everything starts_with
#' @examples
#' endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' head(refs_df)
#'
create_references_df <- function(endnote_list, collapse = FALSE) {
  extract_values_from_list <- lapply(seq_along(endnote_list), function(rec_id) {
    record_list_to_df(record_list = endnote_list[rec_id], collapse)
  })

  dplyr::bind_rows(extract_values_from_list) %>%
    dplyr::select(
      dplyr::starts_with("rec_"),
      dplyr::starts_with("ref_"),
      dplyr::starts_with("year"),
      dplyr::starts_with("title"),
      dplyr::starts_with("abstract"),
      dplyr::starts_with("author_ter"),
      dplyr::starts_with("author_sec"),
      dplyr::starts_with("author"),
      dplyr::starts_with("authaddress"),
      dplyr::starts_with("periodical"),
      dplyr::starts_with("pages"),
      dplyr::starts_with("volume"),
      dplyr::starts_with("num"),
      dplyr::starts_with("section"),
      dplyr::starts_with("edition"),
      dplyr::starts_with("pub"),
      dplyr::starts_with("urls_"),
      dplyr::starts_with("electronic"),
      dplyr::starts_with("work"),
      dplyr::starts_with("label"),
      dplyr::starts_with("keyword"),
      dplyr::starts_with("notes"),
      dplyr::starts_with("caption"),
      dplyr::starts_with("custom"),
      dplyr::starts_with("isbn"),
      dplyr::starts_with("language"),
      dplyr::starts_with("database"),
      dplyr::everything()
    )
}
