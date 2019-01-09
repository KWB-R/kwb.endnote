#' Create dataframe from Endnote XML file
#'
#' @param endnote_xml path to Endnote library exported as .xml (default:
#' system.file("extdata/KWB_documents.xml", package = "kwb.endnote")
#' @return data.frame with all information from Endnote XML joined with data from
#' get_reference_type_names()
#' @export
#' @importFrom kwb.read read_xml_as_path_value
#' @importFrom kwb.fakin toSubdirMatrix
#' @importFrom stringr str_remove_all
#' @importFrom dplyr left_join
#' @examples
#' references_df <- create_df_from_endnote_xml()
#' head(references_df)
create_df_from_endnote_xml <- function(
  endnote_xml = system.file("extdata/KWB_documents.xml",
                            package = "kwb.endnote")) {
references <- kwb.read::read_xml_as_path_value(endnote_xml)

references_df <- as.data.frame(cbind(kwb.fakin::toSubdirMatrix(
  stringr::str_remove_all(references$path,
                      pattern = "^/xml/records/record")),
                           references[, -1]),
  stringsAsFactors = FALSE)
references_df[,1] <- as.numeric(stringr::str_remove_all(references_df[,1],
                                             "\\[|\\]"))

n_col <- ncol(references_df)
colnames(references_df) <- c("record_id", paste0("key", 1:(n_col-2)), "value")

references_df <- dplyr::left_join(references_df,
                 get_reference_type_names(endnote_xml))

return(references_df)
}

