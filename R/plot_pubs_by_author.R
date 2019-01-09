#' Plot Number of Publications by Author
#'
#' @param pubs_by_author_df a data frame with author names in column "value" and
#' number of publications (in column "n")
#' @return plot of number of publications per author
#' @export
#' @import ggplot2
#' @importFrom forcats fct_reorder
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' refs_by_author_lastfirst <- references_df %>%
#' dplyr::filter(.data$key2 == "authors") %>%
#' dplyr::count(.data$value)  %>%
#' dplyr::arrange(dplyr::desc(.data$n))
#' plot_pubs_by_author(refs_by_author_lastfirst[1:30, ])

plot_pubs_by_author <- function(pubs_by_author_df) {
  g <- ggplot2::ggplot(pubs_by_author_df,
                       ggplot2::aes(x = forcats::fct_reorder(.data$value,
                                                             .data$n),
                                    y = .data$n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Number of Publications",
                  x = "Author")
  g
}
