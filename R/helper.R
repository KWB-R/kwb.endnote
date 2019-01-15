# attr_of_element --------------------------------------------------------------
attr_of_element <- function(x, element, which) {

  attr(x[[element]], which)
}

# colname_i --------------------------------------------------------------------

# Helper function to generate column name with index as suffix
colname_i <- function(name, i) {

  sprintf("%s%02d", name, i)
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

  system.file("extdata/2019-01-07_KWB_documents.xml", package = "kwb.endnote")
}

# first_of_element -------------------------------------------------------------
first_of_element <- function(x, element) {

  x[[element]][[1]]
}

# get_list_entry ---------------------------------------------------------------
get_list_entry <- function(x, path) {

  while (! is.null(x) && length(path)) {

    p <- path[1]

    path <- path[-1]

    x <- x[[p]]
  }

  x
}

#' Helper function: get reference type names
#'
#' @param endnote_xml path to Endnote library exported as .xml (default:
#'   \code{default_xml()})
#' @return data.frame with columns record_id, rec_number, ref_type_id, ref_type_name
#' @export
#' @examples
#' ref_type_names <- get_reference_type_names()
#' head(ref_type_names)
get_reference_type_names <- function(endnote_xml = default_xml()) {

  records <- unname(create_endnote_list(endnote_xml))

  data.frame(
    record_id = seq_along(records),
    rec_number = numeric_first_elements(records, "rec-number"),
    ref_type_id = numeric_first_elements(records, "ref-type"),
    ref_type_name = sapply(records, attr_of_element, "ref-type", "name"),
    stringsAsFactors = FALSE
  )
}

# numeric_first_elements -------------------------------------------------------
numeric_first_elements <- function(x, element) {

  as.numeric(sapply(x, first_of_element, element))
}
