#' Create dataframe from Endnote XML file
#'
#' @param endnote_xml path to Endnote library exported as .xml (default:
#'   \code{default_xml()})
#' @return data.frame with all information from Endnote XML joined with data
#'   from get_reference_type_names()
#' @export
#' @importFrom kwb.read read_xml_as_path_value
#' @importFrom kwb.fakin toSubdirMatrix
#' @importFrom stringr str_remove_all
#' @importFrom dplyr left_join
#' @importFrom fs file_info
#' @examples
#' references_df <- create_df_from_endnote_xml()
#' head(references_df)
#'
create_df_from_endnote_xml <- function(endnote_xml = default_xml()) {
  references <- kwb.read::read_xml_as_path_value(endnote_xml)

  xml_paths <- references$path %>%
    stringr::str_remove_all(pattern = "^/xml/records/record")

  references_df <- kwb.utils::asNoFactorDataFrame(cbind(
    kwb.fakin::toSubdirMatrix(xml_paths),
    references[, -1]
  ))

  references_df[, 1] <- as.numeric(stringr::str_remove_all(
    references_df[, 1], "\\[|\\]"
  ))

  n_col <- ncol(references_df)

  colnames(references_df) <- c("record_id", paste0("key", 1:(n_col - 2)), "value")

  endnote_df <- dplyr::left_join(
    references_df,
    get_reference_type_names(endnote_xml)
  )

  add_file_info_attributes(endnote_df, endnote_xml)
}

if (FALSE) {
  abstracts <- references_df %>%
    dplyr::filter(.data$key1 == "abstract") %>%
    dplyr::group_by(
      .data$record_id,
      .data$rec_number,
      .data$ref_type_id,
      .data$ref_type_name,
      .data$key1
    ) %>%
    dplyr::summarise(value = paste(value, collapse = ""))

  authors <- references_df %>%
    dplyr::filter(.data$key1 == "contributors") %>%
    dplyr::group_by(
      .data$record_id,
      .data$rec_number,
      .data$ref_type_id,
      .data$ref_type_name,
      .data$key1,
      .data$key2,
      .data$key3
    ) %>%
    dplyr::summarise(value = paste(value, collapse = ""))
}
