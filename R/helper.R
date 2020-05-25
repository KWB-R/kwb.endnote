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
  extdata_file("2020-05-21_KWB-documents.xml")
}

# first_of_element -------------------------------------------------------------
first_of_element <- function(x, element) {
  x[[element]][[1]]
}

# get_list_entry ---------------------------------------------------------------
get_list_entry <- function(x, path) {
  while (!is.null(x) && length(path)) {
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

# extdata_file -----------------------------------------------------------------

#' Get Path to File in This Package
#'
#' @param \dots parts of path passed to \code{\link{system.file}}
#' @export
extdata_file <- function(...) {
  system.file("extdata", ..., package = "kwb.endnote")
}

#' Helper function: add fileinfo attributes
#'
#' @param obj object to write attributes to
#' @param path path to file
#' @importFrom fs file_info
#' @importFrom tools file_path_sans_ext
#' @return object with file info attributes
#' @export
add_file_info_attributes <- function(obj, path) {
  structure(
    obj,
    "xml_filename_without_extension" = tools::file_path_sans_ext(basename(path)),
    "xml_file_info" = fs::file_info(path)
  )
}

#' Helper function: get xml filename without extension
#' @param obj list or datafram as retrieved by create_endnote_list() or
#' create_df_from_endnote_xml()
#' @return xml filename without file extension
#' @export
get_xml_filename_without_extension <- function(obj) {
  attr(obj, which = "xml_filename_without_extension")
}

#' Helper function: default filename for cleaned XLSX
#' @param endnote_list list as retrieved by create_endnote_list()
#' @return default clean xlsx filename
#' @export
default_clean_xlsx <- function(endnote_list) {
  sprintf("%s_clean.xlsx", get_xml_filename_without_extension(endnote_list))
}

#' Helper function: default filename for XLSX
#' @param endnote_list list as retrieved by create_endnote_list()
#' @return default xlsx filename
#' @export
default_xlsx <- function(endnote_list) {
  sprintf("%s.xlsx", get_xml_filename_without_extension(endnote_list))
}

#' Helper function: tidy dataframe
#'
#' @param df data frame as retrieved by create_references_df() or
#' clean_references_df()
#' @param exclude_cols vector of column names to exclude for gathering
#' (default: "rec_number")
#' @return a tidy dataframe with columns rec_number, key and value
#' @export
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange
#' @examples
#' \dontrun{
#' endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' refs_df_tidy <- tidy_df(refs_df)
#' }

tidy_df <- function(df, exclude_cols = "rec_number") {
  df %>%
    tidyr::gather("key", "value", setdiff(names(df), exclude_cols)) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::arrange(.data$key)
}


replace_na_with_value <- function(vector, value, dbg = TRUE) {

  na_indices <- which(is.na(vector))

  if (any(na_indices)) {
    msg <- sprintf("Replacing %d NA values with %s",
                   length(na_indices),
                   value)
    kwb.utils::catAndRun(msg, expr = {vector[na_indices] <- value}, dbg = dbg)
  }

  vector
}


replace_indices_with_value <- function(vector, indices, value, dbg = TRUE) {

  n_indices <- length(indices)

  if (n_indices > 0) {
    msg <- sprintf("Replacing %d values with %s",
                   n_indices,
                   value)
    kwb.utils::catAndRun(msg, expr = {vector[indices] <- value}, dbg = dbg)
  }

  vector
}


select_columns <- function(df, pattern) {
  columns <- unique(stringr::str_extract(names(df), pattern))
  columns[!is.na(columns)]
}



#' Helper Function: Get Available Multi Col Names
#'
#' @description all names which are valid inputs for tidy_selected_cols()
#' @param df as retrieved by create_references_df() or clean_references_df()
#' @return all names which are valid inputs for tidy_selected_cols()
#' @export
#' @importFrom stringr str_remove

get_available_multi_cols <- function(df) {

available_multi_cols <- stringr::str_extract(names(df), ".*[0-9][0-9]$") %>%
  stringr::str_remove("[0-9][0-9]$") %>%
  unique()

available_multi_cols <- available_multi_cols[!is.na(available_multi_cols)]

available_multi_cols
}


#' Helper Function: Tidy Selected Cols
#'
#' @param df as retrieved by create_references_df() or clean_references_df()
#' @param column a multi col column e.g. "author"
#' @return tidy dataframe for selected multi col in df
#' @export
#' @importFrom dplyr mutate arrange
#' @importFrom tidyr separate

tidy_selected_cols <- function(df,
                               column = "author") {



  cols_selected <- c("rec_number", select_columns(df,
                                                  pattern = sprintf("^%s[0-9][0-9]",
                                                                    column)))

  if(length(cols_selected) > 1) {
    tidy_df(df[,cols_selected],
            exclude_cols = "rec_number") %>%
    tidyr::separate(.data$key,
                    into = c("key_name", "key_num"),
                    sep = -2,
                    remove = FALSE) %>%
    dplyr::mutate(key_num = as.numeric(.data$key_num)) %>%
    dplyr::arrange(dplyr::desc(.data$rec_number), .data$key)

  } else {
    stop(sprintf("Provided multi column '%s' not 'df'\nPlease choose one of: %s",
                 column,
                 paste(get_available_multi_cols(df), collapse = ", ")
                 ))
  }
}



#' Helper Function: Tidy Multi Cols Dataframe
#'
#' @param df as retrieved by create_references_df() or clean_references_df()
#' @return tidy dataframe for all multi cols in df
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_clean_df <- clean_references_df(endnote_list)
#' multi_cols_df <- tidy_multi_cols_df(refs_clean_df)
#' }

tidy_multi_cols_df <- function(df) {

  tidy_multi_cols_list <- lapply(get_available_multi_cols(df), function(col) {
    tidy_selected_cols(df, column = col)})

  dplyr::bind_rows(tidy_multi_cols_list)

}


#' Helper Function: Tidy Multi Cols List
#'
#' @param df as retrieved by create_references_df() or clean_references_df()
#' @return tidy list with a sublist for echa multi col in df (see:
#' get_available_multi_cols())
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{endnote_list <- create_endnote_list()
#' refs_clean_df <- clean_references_df(endnote_list)
#' multi_cols_list <- tidy_multi_cols_list(refs_clean_df)
#' }

tidy_multi_cols_list <- function(df) {

  valid_multi_cols <- get_available_multi_cols(df)

  list(author = tidy_selected_cols(df, valid_multi_cols[3]),
       author_secondary = tidy_selected_cols(df, valid_multi_cols[2]),
       author_tecondary = tidy_selected_cols(df, valid_multi_cols[1]),
       urls_pdf = tidy_selected_cols(df, valid_multi_cols[4]),
       keywords = tidy_selected_cols(df, valid_multi_cols[5]))

}




