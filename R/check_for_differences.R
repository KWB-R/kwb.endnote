#' Helper function: tidy dataframe
#'
#' @param df data frame as retrieved by create_references_df() or
#' clean_references_df()
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

tidy_df <- function(df) {
  df %>%
  tidyr::gather("key", "value", -.data$rec_number) %>%
  dplyr::filter(!is.na(.data$value)) %>%
  dplyr::arrange(.data$rec_number, .data$key)
}

#' Check two Dataframes for Differences
#'
#' @param df_x data frame as retrieved by create_references_df() or
#' clean_references_df()
#' @param df_y data frame as retrieved by create_references_df() or
#' clean_references_df()
#' @param name_value_x column name for values of 'df_x' dataframe (default:
#' "value_x")
#' @param name_value_y column name for values of 'df_y' dataframe (default:
#' "value_y")
#' @param dbg should dbg messages be printed (default: TRUE)
#' @return a dataframe containing only the differences between df_x and df_y
#' @export
#' @importFrom dplyr rename full_join
#' @importFrom rlang quo :=
#' @examples
#' \dontrun{
#' ############################################################################
#' ### Option 1
#' ### Check differences between two different versions of "KWB_documents.xml"
#' ############################################################################
#'
#' old_xml <- system.file("extdata/2019-01-07_KWB_documents.xml",
#' package = "kwb.endnote")
#' new_xml <- kwb.endnote::default_xml()
#' old_list <- kwb.endnote::create_endnote_list(old_xml)
#' new_list <- kwb.endnote::create_endnote_list(new_xml)
#' old_df <- kwb.endnote::create_references_df(old_list)
#' new_df <- kwb.endnote::create_references_df(new_list)
#' diffs_df_oldnew <- compare_df(old_df, new_df)
#' head(diffs_df_oldnew)
#'
#' ############################################################################
#' ### Option 2:
#' ### Check differences between "as-is" import and "collapsing" fields
#' ############################################################################
#'
#' endnote_list <- create_endnote_list()
#' refs_df <- create_references_df(endnote_list)
#' refs_df_collapse <- create_references_df(endnote_list, collapse = TRUE)
#' diffs_df <- compare_df(refs_df, refs_df_collapse)
#' head(diffs_df)
#'
#' }
check_for_differences <- function(df_x,
                       df_y,
                       name_value_x = "value_x",
                       name_value_y = "value_y",
                       dbg = TRUE) {

df_x_tidy <- tidy_df(df_x) %>%
              dplyr::rename(!!quo(name_value_x) := .data$value)
df_y_tidy <- tidy_df(df_y) %>%
               dplyr::rename(!!quo(name_value_y) := .data$value)


df_xy_tidy <- dplyr::full_join(df_x_tidy,
                                 df_y_tidy,
                                 by = c("rec_number", "key"))


diffs_idx <- which(!sapply(seq_len(nrow(df_xy_tidy)), function(row) {
  identical(df_xy_tidy[[name_value_x]][row], df_xy_tidy[[name_value_y]][row])
} ))


df_xy_tidy[diffs_idx,]
}
