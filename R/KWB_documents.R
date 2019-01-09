#' Helper function: get reference type names
#'
#' @param endnote_xml path to Endnote library exported as .xml
#' @return data.frame with columns record_id, ref_type_id, ref_type_name
#' @export
#' @importFrom xml2 as_list
#' @examples
#' endnote_xml <- system.file("extdata/KWB_documents.xml",
#' package = "kwb.endnote")
#' ref_type_names <- get_reference_type_names(endnote_xml)
#' head(ref_type_names)
get_reference_type_names <- function(endnote_xml) {

references_list <- xml2::as_list(xml2::read_xml(endnote_xml))


n_records <- length(references_list$xml$records)
ref_type_names <- sapply(1:n_records, function(i) {
attr(references_list$xml$records[i]$record$`ref-type`, which = "name")
})

ref_type_ids <- as.numeric(sapply(1:n_records, function(i) {
  references_list$xml$records[i]$record$`ref-type`[[1]]
  }
))

data.frame(record_id = 1:n_records,
           ref_type_id = ref_type_ids,
           ref_type_name = ref_type_names,
           stringsAsFactors = FALSE)
}



#' Create dataframe from Endnote XML file
#'
#' @param endnote_xml path to Endnote library exported as .xml
#'
#' @return data.frame with all information from Endnote XML joined with data from
#' get_reference_type_names()
#' @export
#' @importFrom kwb.read read_xml_as_path_value
#' @importFrom kwb.fakin toSubdirMatrix
#' @importFrom stringr str_remove_all
#' @importFrom dplyr left_join
#' @examples
#' endnote_xml <- system.file("extdata/KWB_documents.xml",
#' package = "kwb.endnote")
#' references_df <- create_df_from_endnote_xml(endnote_xml)
#' head(references_df)
create_df_from_endnote_xml <- function(endnote_xml) {

references <- kwb.read::read_xml_as_path_value(endnote_xml)

references_df <- as.data.frame(cbind(kwb.fakin::toSubdirMatrix(
  stringr::str_remove_all(references$path,
                      pattern = "^/xml/records/record")),
                           references[, -1]),
  stringsAsFactors = FALSE)
references_df[,1] <- as.numeric(stringr::str_remove_all(references_df[,1],
                                             "\\[|\\]"))

n_col <- ncol(references_df)
colnames(references_df) <- c("record_id", paste0("key", 1:(n_col-2)), "value")

references_df <- dplyr::left_join(references_df,
                 get_reference_type_names(endnote_xml))

return(references_df)
}



if(FALSE) {
library(dplyr)

## Bibs with abstracts
refs_with_abstract <- references_df %>%
  dplyr::filter(.data$key1 == "abstract") %>%
  dplyr::count(.data$record_id, .data$ref_type_name)


### Percent of Publications with Abstracts
100*nrow(refs_with_abstract)/length(unique(references_df$record_id))


## Group references by "custom3" (confidential or not?)

refs_with_accessability_level <- references_df %>%
  dplyr::filter(.data$key1 == "custom3") %>%
  dplyr::count(.data$value)


is_confidential <- stringr::str_detect(refs_with_accessability_level$value,
                         stringr::regex("conf|PU|intern|private",
                                        ignore_case = TRUE))

sum(refs_with_accessability_level$n[is_confidential])


## Group references by "custom2" (i.e. "project names")

refs_with_project <- references_df %>%
  dplyr::filter(.data$key1 == "custom2")

nrow(refs_with_project)

### in percent
100*nrow(refs_with_project)/nrow(references_df)


unique_project_names <- refs_with_project  %>%
  dplyr::count(.data$value)




## KWB pubs by type

refs_by_type <- get_reference_type_names(references_list) %>%
  dplyr::count(.data$ref_type_name) %>%
  dplyr::arrange(dplyr::desc(.data$n))

## KWB pubs by author
refs_by_author_lastfirst <- references_df %>%
  dplyr::filter(.data$key2 == "authors") %>%
  dplyr::count(.data$value)  %>%
  dplyr::arrange(dplyr::desc(.data$n))

refs_by_author_last <- references_df %>%
  dplyr::filter(.data$key2 == "authors") %>%
  dplyr::mutate(value = stringr::str_remove_all(.data$value, ",.*")) %>%
  dplyr::mutate(value = stringr::str_remove_all(.data$value, "^\\w+\\.?\\s+")) %>%
  dplyr::count(.data$value)  %>%
  dplyr::arrange(dplyr::desc(.data$n))

g <- ggplot2::ggplot(refs_by_author_last[1:30, ],
                ggplot2::aes(x = forcats::fct_reorder(value, n), y = n)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

#plotly::ggplotly(g)
print(g)

references_df %>%
  dplyr::filter(.data$ref_type_name == "Journal Article")
}
