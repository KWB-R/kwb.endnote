#' Helper function: get reference type names
#'
#' @param endnote_xml path to Endnote library exported as .xml (default:
#'   \code{\link{default_xml()}})
#' @return data.frame with columns record_id, rec_number, ref_type_id, ref_type_name
#' @export
#' @examples
#' ref_type_names <- get_reference_type_names()
#' head(ref_type_names)
get_reference_type_names <- function(endnote_xml = default_xml()) {

  references_list <- create_endnote_list(endnote_xml)


  n_records <- length(references_list)
  ref_type_names <- sapply(1:n_records, function(i) {
    attr(references_list[i]$record$`ref-type`, which = "name")
  })

  ref_type_ids <- as.numeric(sapply(1:n_records, function(i) {
    references_list[i]$record$`ref-type`[[1]]
  }
  ))

  rec_number <- as.numeric(sapply(1:n_records, function(i) {
    references_list[i]$record$`rec-number`[[1]]
  }
  ))

  data.frame(record_id = 1:n_records,
             rec_number = rec_number,
             ref_type_id = ref_type_ids,
             ref_type_name = ref_type_names,
             stringsAsFactors = FALSE)
}

# default_xml ------------------------------------------------------------------

#' Path to Default XML File
#'
#' @return path to xml file stored in this package, containing references from
#'   KWB Endnote database
#'
#' @export
#'
default_xml <- function() {

  system.file("extdata/KWB_documents.xml", package = "kwb.endnote")
}
