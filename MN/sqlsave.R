

#input the table name here
table_name <- "DS_20141028_RST_0ZV157LCHOGDG"
#input db or data link info here(either olive or analystdb)
database_name <- "analystdb"

#build strings
tbl <- paste(table_name, database_name , sep ="@")
sqlc <- paste("select * from ", tbl) #sep = "")

con <-odbcConnect("Olive11", uid="olive", pwd="olive_tr33s")
#sqlSave(con, test_table, "TEST_TABLE")
g <- sqlQuery(con,  sqlc)
#d <- sqlQuery(con, "select * from TEST_TABLE")
close(con)

#rename table to it fits the schema
ntable <- paste(table_name, "X", sep = "")# sep = "_") 


try(sqlDrop(con, ntable, errors = FALSE), silent = TRUE)
sqlSave(con, g, table = ntable)


sqlSave(con, g, rownames = TRUE , addPK=TRUE)


sqlFetch(con, "USArrests", rownames = "state") # get the lot
foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 222
sqlUpdate(con, foo, "USArrests")
sqlFetch(con, "USArrests", rownames = "state", max = 5)
sqlDrop(con, "USArrests") 
close(con)