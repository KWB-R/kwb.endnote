#
# description: This script opens a database connection to a file in SQLite
#   format, lists the tables and reads the table contents into a list.
#
# author: Hauke Sonnenberg
# created: 2020-02-18
#

#install.packages("RSQLite")
#library(DBI)

grammar <- list(
  dms = "~/Projekte2/dms",
  dms_hs = "~/../Downloads/endnote-db/KWB-documents_20191205.Data/sdb",
  sdb = "<root>/sdb.eni",
  pdb = "<root>/pdb.eni"
)

#paths <- kwb.utils::resolve(grammar, root = "dms")
paths <- kwb.utils::resolve(grammar, root = "dms_hs")

# Create an ephemeral in-memory RSQLite database
con <- DBI::dbConnect(RSQLite::SQLite(), paths$pdb)

table_names <- DBI::dbListTables(con)

contents <- lapply(setNames(nm = table_names), DBI::dbReadTable, con = con)

write.csv2(contents$pdf_index[, c(1, 3, 5)], "~/tmp/pdf_index.csv")

x <- contents$refs[, c("id", "keywords")]

#write.csv(x, "keywords.csv", row.names = FALSE)

DBI::dbDisconnect(con)
