

required_libraries <- c('ggplot2',
                        'reshape2',
                        'stringr',
                        'lubridate',
                        'dplyr',
                        'scales',
                        'RODBC',
                        'PropCIs'
                        'PF')


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


#diffscorecitest(41,257, 64, 244, .90)





x <- data.frame(1)
x <- cbind(x,diffscorecitest(41,257, 64, 244, .90))

x <- data.frame(200:220, 40:60, 200:220, 20:40)
names(x)<-c("set1","set2","set3","set4")


#x <- mapply(g,diffscorecitest )
gmini <- g[1:10,]
gtest <- g[1:50, 9:12]
mfun <- diffscorecitest
#mfun <- dt
mp <- t(mapply ("RRscDF",  x1=gmini$CONTROL, n1=gmini$TOTAL_CONTROL, x2=gmini$EXPOSED, n2=gmini$TOTAL_EXPOSED,  conf.level = .90) )

gminiwl <- cbind(gmini, mp)
 

0.914  0.989

diffscoreci(822,1093,  1016, 1421, .9)

RRsc(822,1093,  1016, 1421, .9)

x1=822;n1=1093;x2=1016;n2=1421
da<-c(x2,n2,x1,n1)
rrs<-RRsc(da, pf=F , alpha = .1)



gtest <- g[1:50, 9:12]
l <- nrow(gtest)

k <- 1
for (n in 1:l)
{
  xl2 <- c(gtest[n,1],gtest[n,2],gtest[n,3],gtest[n,4])
  rMN <- rbind(rMN, RRscDF(xl2, pf=F , alpha = .1, rnd = 2))
  #print(rrst)

}

rMN
gtest2 <- cbind(gtest, rMN)

gtest2
rMN <- NULL


names(gtest) <- c("x1","n1","x2","n2")

RRscDF(xl2 , pf=F , alpha = .1, rnd = 2)
 
rrst<- rbind(RRscDF(xl, pf=F , alpha = .1, rnd = 2))


xl2 <- c(gtest[1,1],gtest[1,2],gtest[1,3],gtest[1,4])

mfun <- RRscDF
omg <- mapply(mfun, y = gtest, pf=F , alpha = .1, rnd = 2)

sapply(gtest, RRscDF)


RRsc(y)
y <- c(1,0,1,2)
xxxx <- RRscDF(y)

if(is.matrix(y))
  y <- c(t(cbind(y[,1],apply(y,1,sum))))
x1 <- y[1]
n1 <- y[2]
x2 <- y[3]
n2 <- y[4]
if  (n1 == 0 | n2 ==0 )   out2 <- data.frame(t(c(0,0,0))) else  out2 <- data.frame(t(c(x1,n1,x2,n2)))


out2


ner <- c("PF", "LL", "UL")
out <- data.frame(t(c(PF  = 0,LL = 0,UL = 0)))
out

y <- c(1,0,1,2)
z <- c(19,932,19,293)
names(RRscDF(y)) == names(RRscDF(z))
subset(RRscDF(z), select = c("LL", "UL"))
