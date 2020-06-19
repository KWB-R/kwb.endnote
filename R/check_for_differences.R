#' Check two Dataframes for Differences
#'
#' @param df_x data frame as retrieved by create_references_df() or
#' clean_references_df()
#' @param df_y data frame as retrieved by create_references_df() or
#' clean_references_df()
#' @param dbg should dbg messages be printed (default: TRUE)
#' @return a dataframe containing only the differences between df_x and df_y
#' @export
#' @importFrom dplyr rename full_join
#' @importFrom rlang quo_name :=
#' @importFrom kwb.utils catIf catAndRun
#' @examples
#' \dontrun{
#' ############################################################################
#' ### Option 1
#' ### Check differences between two different versions of "KWB_documents.xml"
#' ############################################################################
#'
#' old_xml <- extdata_file("2020-05-25_KWB-documents.xml")
#' new_xml <- extdata_file("2020-06-17_KWB-documents.xml")
#' old_list <- kwb.endnote::create_endnote_list(old_xml)
#' new_list <- kwb.endnote::create_endnote_list(new_xml)
#' old_df <- kwb.endnote::create_references_df(old_list)
#' new_df <- kwb.endnote::create_references_df(new_list)
#' diffs_df_oldnew <- check_for_differences(old_df, new_df)
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
#' diffs_df <- check_for_differences(refs_df, refs_df_collapse)
#' head(diffs_df)
#'
#' }
check_for_differences <- function(
                                  df_x, df_y, dbg = TRUE) {
  name_df_x <- deparse(substitute(df_x))
  name_df_y <- deparse(substitute(df_y))

  name_value_x <- sprintf("value_%s", name_df_x)
  name_value_y <- sprintf("value_%s", name_df_y)

  tidy_name <- function(name) paste0(name, "_tidy")

  get_text <- function(name_df, name_value) sprintf(
      "Tidying data.frame '%s' and rename 'value' to '%s'. Saving to %s",
      name_df, name_value, tidy_name(name_df)
    )

  df_x_tidy <- kwb.utils::catAndRun(
    messageText = get_text(name_df_x, name_value_x), dbg = dbg,
    expr = tidy_df(df_x) %>%
      dplyr::rename(!!rlang::quo_name(name_value_x) := .data$value)
  )

  df_y_tidy <- kwb.utils::catAndRun(
    messageText = get_text(name_df_y, name_value_y), dbg = dbg,
    expr = tidy_df(df_y) %>%
      dplyr::rename(!!rlang::quo_name(name_value_y) := .data$value)
  )

  join_cols <- c("rec_number", "key")

  messageText <- sprintf(
    "Joining data.frame '%s' and '%s' with cols '%s'",
    tidy_name(name_df_x), tidy_name(name_df_y), kwb.utils::stringList(join_cols)
  )

  df_xy_tidy <- kwb.utils::catAndRun(
    messageText = messageText, dbg = dbg,
    expr = dplyr::full_join(df_x_tidy, df_y_tidy, by = join_cols)
  )

  messageText <- sprintf(
    "Identifying indices with different content for '%s' and '%s'",
    name_value_x, name_value_y
  )

  diffs_idx <- kwb.utils::catAndRun(
    messageText = messageText, dbg = dbg,
    expr = which(!sapply(seq_len(nrow(df_xy_tidy)), function(row) identical(
        df_xy_tidy[[name_value_x]][row],
        df_xy_tidy[[name_value_y]][row]
      )))
  )

  kwb.utils::catIf(dbg, sprintf(
    "Content has changed for %d indices", length(diffs_idx)
  ))

  df_xy_tidy[diffs_idx, ]
}
