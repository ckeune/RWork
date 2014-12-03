gamma <- 1:3 
beta <- 1:4
gb.df <-expand.grid(gamma=gamma, beta=beta)
mfux <- dt
mp <- mapply ("mfun", x=gb.df$gamma, df=gb.df$beta, MoreArgs=list(ncp=1, log=FALSE) ) 

gb.df <-  cbind(gb.df ,  mapply ("mfun", x=gb.df$gamma, df=gb.df$beta, MoreArgs=list(ncp=1, log=FALSE) ) )

gb.df <- cbind(gb.df,Map(mfux, gamma, beta))