#' Create List From Endnote XML
#'
#' @param endnote_xml path to Endnote library exported as .xml (default:
#'   \code{default_xml()})
#' @return list of imported endnote_xml
#' @export
#' @importFrom xml2 as_list
#' @examples
#' endnote_list <- create_endnote_list()
#' str(endnote_list[1]$record)
#' attr(endnote_list, "xml_file_info")
#' attr(endnote_list, "xml_filename_without_extension")
create_endnote_list <- function(endnote_xml = default_xml()) {
  endnote_list <- xml2::as_list(xml2::read_xml(endnote_xml))$xml$records


  add_file_info_attributes(endnote_list, endnote_xml)
}
