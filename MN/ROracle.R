install.packages("ROracle")
library(ROracle)
drv <- dbDriver("Oracle")
con <- dbConnect(drv, "analyst", "2m442D", localhost:1521/ffmis02.internal.essence.co.uk")