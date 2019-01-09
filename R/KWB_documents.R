#' Helper function: get reference type names
#'
#' @param endnote_xml path to Endnote library exported as .xml (default:
#' system.file("extdata/KWB_documents.xml", package = "kwb.endnote")
#' @return data.frame with columns record_id, rec_number, ref_type_id, ref_type_name
#' @export
#' @importFrom xml2 as_list
#' @examples
#' endnote_xml <- system.file("extdata/KWB_documents.xml",
#' package = "kwb.endnote")
#' ref_type_names <- get_reference_type_names(endnote_xml)
#' head(ref_type_names)
get_reference_type_names <- function(
  endnote_xml = system.file("extdata/KWB_documents.xml",
                            package = "kwb.endnote"))  {

references_list <- xml2::as_list(xml2::read_xml(endnote_xml))


n_records <- length(references_list$xml$records)
ref_type_names <- sapply(1:n_records, function(i) {
attr(references_list$xml$records[i]$record$`ref-type`, which = "name")
})

ref_type_ids <- as.numeric(sapply(1:n_records, function(i) {
  references_list$xml$records[i]$record$`ref-type`[[1]]
  }
))

rec_number <- as.numeric(sapply(1:n_records, function(i) {
  references_list$xml$records[i]$record$`rec-number`[[1]]
}
))

data.frame(record_id = 1:n_records,
           rec_number = rec_number,
           ref_type_id = ref_type_ids,
           ref_type_name = ref_type_names,
           stringsAsFactors = FALSE)
}



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



if(FALSE) {
library(dplyr)


  create_df_from_endnote_xml() %>%
  dplyr::filter(.data$ref_type_name == "Journal Article")
}
