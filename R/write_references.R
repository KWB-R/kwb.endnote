# write_references -------------------------------------------------------------
write_references <- function(
  endnote_list, file, export_dir = ".", clean, replace_na = FALSE, dbg = TRUE, ...
)
{
  refs_df <- if (clean) {

    endnote_list %>%
      clean_references_df(replace_na = replace_na, dbg = dbg)

  } else {

    endnote_list %>%
      create_references_df() %>%
      dplyr::arrange(dplyr::desc(.data$rec_number))
  }

  path <- file.path(export_dir, file)

  openxlsx::write.xlsx(file = path, ..., x = c(
    create_list_by_pubtype_from_df(refs_df),
    create_list_with_unique_entries(refs_df)
  ))
}
