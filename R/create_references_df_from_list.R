#' @noRd
#' @keywords internal
null_to_na <- function(x, na_fill = NA_character_) {
  if (is.null(x[[1]])) na_fill else x[[1]]
}

#' Helper function: get abstract from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @return one row abstract data frame
#' @export
#' @importFrom dplyr bind_cols
get_abstract <- function(record_list) {
  abstract <- record_list$record$abstract

  if (is.null(abstract)) {
    abstract_vector <- NA_character_
  } else {
    abstract_vector <- lapply(seq_along(abstract), function(i) {
      null_to_na(abstract[[i]][[1]])
    })
  }

  paste(abstract_vector, collapse = "")
}

#' Helper function: get authors from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "author"
#' @param extract_value extract_value = "authors"
#' @return one row authors data frame
#' @export
#' @importFrom dplyr bind_cols
get_authors <- function(record_list, col_name = "author",
                        extract_value = "authors") {
  authors <- record_list$record$contributors[[extract_value]]

  if (is.null(authors)) {
    authors_df <- tibble::tibble(author = NA_character_)
    names(authors_df) <- sprintf("%s1", col_name)
  } else {
    authors_list <- lapply(seq_along(authors), function(i) {
      tmp <- tibble(author = null_to_na(authors[[i]]$style))
      names(tmp) <- sprintf("%s%02d", col_name, i)
      tmp
    })

    authors_df <- dplyr::bind_cols(authors_list)
  }

  return(authors_df)
}


#' Helper function: get secondary authors from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "author_secondary"
#' @return one row authors data frame
#' @export
#' @inheritParams get_authors
get_secondary_authors <- function(record_list,
                                  col_name = "author_secondary",
                                  extract_value = "secondary-authors") {
  get_authors(record_list, col_name, extract_value)
}

#' Helper function: get tertiary authors from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "author_tertiary"
#' @return one row authors data frame
#' @export
#' @inheritParams get_authors
get_tertiary_authors <- function(record_list,
                                 col_name = "author_tertiary",
                                 extract_value = "tertiary-authors") {
  get_authors(record_list, col_name, extract_value)
}


#' Helper function: get pdfurls from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "url_pdfurls"
#' @return one row pdfurls data frame
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
get_pdfurls <- function(record_list, col_name = "urls_pdf") {
  pdfurls <- record_list$record$urls$`pdf-urls`

  if (is.null(pdfurls)) {
    pdfurls_df <- tibble::tibble(pdfurl = NA_character_)
    names(pdfurls_df) <- sprintf("%s1", col_name)
  } else {
    pdfurls_list <- lapply(seq_along(pdfurls), function(i) {
      tmp <- tibble::tibble(pdfurl = null_to_na(pdfurls[[i]]))
      names(tmp) <- sprintf("%s%d", col_name, i)
      tmp
    })

    pdfurls_df <- dplyr::bind_cols(pdfurls_list)
  }

  return(pdfurls_df)
}


#' Reference List to Data Frame
#'
#' @param record_list list with one record of create_endnote_list()
#' @return data frame for record
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
record_list_to_df <- function(record_list) {
  abstract_vector <- get_abstract(record_list)
  authors_df <- get_authors(record_list)
  authors_secondary_df <- get_secondary_authors(record_list)
  authors_tertiary_df <- get_tertiary_authors(record_list)
  urls_pdf_df <- get_pdfurls(record_list)

  record <- record_list$record

  tibble::tibble(
    rec_number = null_to_na(record$`rec-number`),
    ref_type = as.numeric(null_to_na(record$`ref-type`)),
    ref_type_name = attr(record$`ref-type`, which = "name"),
    abstract = abstract_vector
  ) %>%
    dplyr::bind_cols(authors_df) %>%
    dplyr::bind_cols(authors_secondary_df) %>%
    dplyr::bind_cols(authors_tertiary_df) %>%
    dplyr::bind_cols(
      tibble::tibble(
        database_name = record$database[[1]],
        database_path = attr(record$database, which = "path"),
        title = null_to_na(record$titles$title$style),
        title_secondary = null_to_na(record$titles$`secondary-title`$style),
        title_tertiary = null_to_na(record$titles$`tertiary-title`$style),
        periodical_title_full = null_to_na(record$periodical$`full-title`$style),
        periodical_title_secondary = null_to_na(record$periodical$`secondary-title`$style),
        year = null_to_na(record$dates$year$style),
        pubdates = null_to_na(record$dates$`pub_dates`$date$style),
        pages = null_to_na(record$pages$style),
        volume = null_to_na(record$volume$style),
        section = null_to_na(record$section$style),
        publisher = null_to_na(record$publisher$style),
        isbn = null_to_na(record$isbn$style),
        language = null_to_na(record$language$style)
      )
    ) %>%
    dplyr::bind_cols(urls_pdf_df) %>%
    dplyr::bind_cols(tibble::tibble(
      urls_related = null_to_na(record$urls$`related-urls`$style),
      electronic_resource_num = null_to_na(record$`electronic-resource-num`$style),
      custom1 = null_to_na(record$custom1$style),
      custom2 = null_to_na(record$custom2$style),
      custom3 = null_to_na(record$custom3$style),
      custom7 = null_to_na(record$custom7$style)
    ))
}


#' Create References Dataframe
#'
#' @param endnote_list list created with create_endnote_list()
#' @return data.frame with columns record_id, rec_number, ref_type_id, ref_type_name
#' @export
#' @importFrom dplyr everything starts_with
#' @examples
#' endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' head(refs_df)
#'
create_references_df <- function(endnote_list) {
  extract_values_from_list <- lapply(seq_along(endnote_list), function(rec_id) {
    record_list_to_df(record_list = endnote_list[rec_id])
  })

  dplyr::bind_rows(extract_values_from_list) %>%
    dplyr::select(
      dplyr::starts_with("rec_"),
      dplyr::starts_with("ref_"),
      dplyr::starts_with("year"),
      dplyr::starts_with("title"),
      dplyr::starts_with("abstract"),
      dplyr::starts_with("author"),
      dplyr::starts_with("author_sec"),
      dplyr::starts_with("author_ter"),
      dplyr::starts_with("periodical"),
      dplyr::starts_with("pages"),
      dplyr::starts_with("volume"),
      dplyr::starts_with("section"),
      dplyr::starts_with("publisher"),
      dplyr::starts_with("urls_"),
      dplyr::starts_with("electronic"),
      dplyr::starts_with("custom"),
      dplyr::starts_with("isbn"),
      dplyr::starts_with("language"),
      dplyr::starts_with("database"),
      dplyr::everything()
    )
}
