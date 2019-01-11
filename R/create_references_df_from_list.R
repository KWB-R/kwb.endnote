#' @noRd
#' @keywords internal
null_to_na <- function(x, na_fill = NA_character_) {
  if (is.null(x[[1]])) na_fill else x[[1]]
}

#' Helper function: get authors from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "contributors_author"
#' @return one row authors data frame
#' @export
#' @importFrom dplyr bind_cols
get_authors <- function(record_list, col_name = "contributors_author") {

  authors <- record_list$record$contributors$authors

  authors_list <- lapply(seq_along(authors), function(i) {
    tmp <- data.frame(author = null_to_na(authors[[i]]$style),
                      stringsAsFactors = FALSE)
    names(tmp) <- sprintf("%s%02d", col_name, i)
    tmp
  })

dplyr::bind_cols(authors_list)


}

#' Helper function: get pdfurls from list for a reference
#'
#' @param record_list list with one record of create_endnote_list()
#' @param col_name default: "url_pdfurls"
#' @return one row pdfurls data frame
#' @export
#' @importFrom dplyr bind_cols
get_pdfurls <- function(record_list, col_name = "url_pdfurls") {

  pdfurls <- record_list$record$urls$`pdf-urls`

  pdfurls_list <- lapply(seq_along(pdfurls), function(i) {
    tmp <- data.frame(pdfurl = null_to_na(pdfurls[[i]]),
                      stringsAsFactors = FALSE)
    names(tmp) <- sprintf("%s%d", col_name, i)
    tmp
  })

  dplyr::bind_cols(pdfurls_list)
}


#' Reference List to Data Frame
#'
#' @param record_list list with one record of create_endnote_list()
#' @return data frame for record
#' @export
#' @importFrom dplyr bind_cols
record_list_to_df <- function(record_list) {

  authors_df <- get_authors(record_list)
  #pdfurls_df <- get_pdfurls(record_list)

  record <- record_list$record

    tibble::tibble(
      rec_number = null_to_na(record$`rec-number`),
      ref_type = as.numeric(null_to_na(record$`ref-type`)),
      ref_type_name = attr(record$`ref-type`, which = "name")) %>%
    dplyr::bind_cols(authors_df) %>%
    dplyr::bind_cols(
      tibble::tibble(database_name =  record$database[[1]],
      database_path = attr(record$database, which = "path"),
      titles_title =  null_to_na(record$titles$title$style),
      titles_secondary_title =  null_to_na(record$titles$`secondary-title`$style),
      periodical_full_title =  null_to_na(record$periodical$`full-title`$style),
      dates_year =  null_to_na(record$dates$year$style),
      urls_pdf_urls =  null_to_na(record$urls$`pdf-urls`),
      urls_related_urls =  null_to_na(record$urls$`related-urls`$style),
      electronic_resource_num =  null_to_na(record$`electronic-resource-num`$style),
      custom2 = null_to_na(record$custom2$style),
      custom3 = null_to_na(record$custom3$style))
    )

}


#' Create References Dataframe
#'
#' @param endnote_list list created with create_endnote_list()
#' @return data.frame with columns record_id, rec_number, ref_type_id, ref_type_name
#' @export
#' @examples
#' endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' head(refs_df)
#'
create_references_df <- function(endnote_list)  {

  extract_values_from_list <- lapply(seq_along(endnote_list), function(rec_id) {
    record_list_to_df(record_list = endnote_list[rec_id])
  })

 dplyr::bind_rows(extract_values_from_list)

}

