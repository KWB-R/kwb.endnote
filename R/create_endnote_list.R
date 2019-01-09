#' Create List From Endnote XML
#'
#' @param endnote_xml
#'
#' @return list of imported endnote_xml
#' @export
#' @importFrom xml2 as_list
#' @examples
#' references_list <- create_endnote_list()
#' head(references_list)
create_endnote_list <- function(
  endnote_xml = system.file("extdata/KWB_documents.xml",
                            package = "kwb.endnote")) {

  xml2::as_list(xml2::read_xml(endnote_xml))

}
