#
# description: This script opens a database connection to a file in SQLite
#   format, lists the tables and reads the table contents into a list.
#
# author: Hauke Sonnenberg
# created: 2020-02-18
#

#install.packages("RSQLite")
library(DBI)

# Create an ephemeral in-memory RSQLite database
#con <- dbConnect(RSQLite::SQLite(), "~/Projekte2/dms/pdb.eni")
con <- dbConnect(RSQLite::SQLite(), "~/Projekte2/dms/sdb.eni")

table_names <- dbListTables(con)

contents <- lapply(setNames(nm = table_names), dbReadTable, con = con)

str(contents, 2)

x <- contents$refs[, c("id", "keywords")]

View(x)

#write.csv(x, "keywords.csv", row.names = FALSE)

dbDisconnect(con)
