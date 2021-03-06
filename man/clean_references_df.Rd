% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_references_df.R
\name{clean_references_df}
\alias{clean_references_df}
\title{Clean References Dataframe}
\usage{
clean_references_df(endnote_list, give_hints = FALSE, dbg = TRUE)
}
\arguments{
\item{endnote_list}{list created with create_endnote_list()}

\item{give_hints}{if TRUE hints will be generated, e.g.
"add_public_or_confidential" for accessiblity data}

\item{dbg}{show debug messages (default: TRUE)}
}
\value{
cleaned references_df
}
\description{
Clean References Dataframe
}
\examples{
\dontrun{endnote_list <- create_endnote_list()
refs_clean_df <- clean_references_df(endnote_list)
head(refs_clean_df)
}
}
