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