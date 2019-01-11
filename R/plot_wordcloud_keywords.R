#' Plot Wordcloud Keywords
#'
#' @param keywords_df keywords dataframe as retrieved by
#' create_keywords_df()
#' @param ... additional arguments passed to wordcloud2::wordcloud2()
#' @return wordcloud keywords plot
#' @export
#' @importFrom dplyr rename
#' @importFrom wordcloud2 wordcloud2
plot_wordcloud_keywords <- function(keywords_df, ...) {
  keywords_df  %>%
  dplyr::rename(word = value,
                freq = n) %>%
  wordcloud2::wordcloud2(...)
}
