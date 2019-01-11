#' Plot Publications By Year
#'
#' @param refs_df reference dataframe as retrieved by
#' kwb.endnote::create_references_df()
#' @return plot with publications by year
#' @export
#' @import ggplot2
#' @importFrom stringr str_trim
#' @examples
#' endnote_list <- kwb.endnote::create_endnote_list()
#' refs_df <- kwb.endnote::create_references_df(endnote_list)
#' plot_pubs_by_year(refs_df)
#'
plot_pubs_by_year <- function(refs_df) {
  refs_df %>%
  dplyr::mutate(dates_year = stringr::str_trim(.data$dates_year)) %>%
  dplyr::count(.data$ref_type_name, .data$dates_year) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$dates_year,
                               y = .data$n,
                               fill = .data$ref_type_name)) +
  ggplot2::geom_col() +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Year",
                y = "Number Of Publications",
                fill = "Publication Type",
                title = "Publications By Year") +
  ggplot2::coord_flip() +
  ggplot2::theme(legend.position = "right")

}
