#' Create List From Endnote XML
#'
#' @param endnote_xml path to Endnote library exported as .xml (default:
#' system.file("extdata/KWB_documents.xml", package = "kwb.endnote")
#' @return list of imported endnote_xml
#' @export
#' @importFrom xml2 as_list
#' @examples
#' endnote_list <- create_endnote_list()
#' str(endnote_list[1]$record)
create_endnote_list <- function(
                                endnote_xml = system.file("extdata/KWB_documents.xml",
                                  package = "kwb.endnote"
                                )) {
  xml2::as_list(xml2::read_xml(endnote_xml))$xml$records
}
