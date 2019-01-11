#' Create Keywords Dataframe
#'
#' @param references_df references_df as recrieved from
#' kwb.endnote::create_df_from_endnote_xml()
#' @return keywords dataframe
#' @export
#' @importFrom tidyr separate_rows
#' @importFrom dplyr filter select mutate count arrange
#' @examples
#' references_df <- create_df_from_endnote_xml()
#' keywords_df <- create_keywords_df(references_df)
#' head(keywords_df)
#'
create_keywords_df <- function(references_df) {
  keywords <- references_df %>%
    dplyr::filter(.data$key1 == "keywords") %>%
    dplyr::select(
      .data$record_id,
      .data$rec_number,
      .data$ref_type_id,
      .data$ref_type_name,
      .data$key1,
      .data$value
    ) %>%
    tidyr::separate_rows(.data$value, sep = ",\\s++")


  keywords_df <- keywords %>%
    dplyr::mutate(value = tolower(.data$value)) %>%
    dplyr::count(.data$value) %>%
    dplyr::arrange(dplyr::desc(.data$n))

  return(keywords_df)
}
