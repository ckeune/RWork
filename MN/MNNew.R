

required_libraries <- c('reshape2',
                        'stringr',
                        'dplyr',
                        'scales',
                        'RODBC',
                        'PF')
setwd("C:/R/MN")
source("RRscDF.R")

for (lib_name in required_libraries){
  if (!require(lib_name, character.only = T)) 
    install.packages(lib_name)
  library(lib_name, character.only = T)
} 

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



#put in the rows # for the data
#gtest <- subset((g[ , 9:12]), TOTAL_CONTROL == 0)
#NAME YOUR COLUMNS FOR YOUR DATA
gtest <- g[, c("CONTROL"     ,   "TOTAL_CONTROL" , "EXPOSED"   ,     "TOTAL_EXPOSED" )]

#insert confidence intervals
conf <- c(.95,.9,.8)


## START MATH

g$c_acct_rate <- Map("/", g$CONTROL, g$TOTAL_CONTROL)
g$e_acct_rate <- Map("/", g$EXPOSED, g$TOTAL_EXPOSED)
g$observed_uplift <- Map("-",g$e_acct_rate, g$c_acct_rate )
#mtmeans[] <- Map(`/`, mtcars, mtmeans)
#gt <- g
#g <- gt
#set limit of dataset length
l <- nrow(gtest)

rMN <- NULL
#set limit of dataset length
for (i in conf)
{
  for (n in 1:l)
  {
   xl2 <- c(gtest[n,1],gtest[n,2],gtest[n,3],gtest[n,4])
   rMN  <- rbind(rMN, subset(RRscDF(xl2, pf=F , alpha = 1-i, rnd = 2), select = c("LL", "UL")))
  
  } 
#rMN <- NULL
names(rMN) <- paste(names(rMN), i)
g <- cbind(g,rMN)
rMN <- NULL
}


ntable <- paste(table_name, "X", sep = "")# sep = "_") 


con <-odbcConnect("Olive11", uid="olive", pwd="olive_tr33s")
try(sqlDrop(con, ntable, errors = FALSE), silent = TRUE)
sqlSave(con, g, table = ntable)
close(con)


 
# 
# conf <- c(.95,.9,.8)
# for (n in conf)
# {
#      print(n)
# }
# 


