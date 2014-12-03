

#diffscorecitest(41,257, 64, 244, .90)
x1 <- 41
n1 <- 257
x2 <- 64
n2 <- 244
conf.level <- .9
 

    px = x1/n1
    py = x2/n2
    z = qchisq(conf.level,1)
    proot = px-py
    dp = 1-proot
    niter = 1
    while(niter <= 50){
      dp = 0.5*dp
      up2 = proot+dp
      score = z2stat(px,n1,py,n2,up2)
      if(score<z){ proot = up2 }
      niter = niter+1
      if((dp<0.0000001) || (abs(z-score)<.000001)){
        niter = 51
        ul = up2}
    }
    
    proot = px-py
    dp = 1+proot
    niter = 1
    while(niter <= 50){
      dp = 0.5*dp
      low2 = proot-dp
      score = z2stat(px,n1,py,n2,low2)
      if(score<z){ proot = low2 }
      niter = niter+1
      if((dp<0.0000001) || (abs(z-score)<.000001)){
        ll = low2
        niter = 51}
    }
    cint <- c(ll,ul)
    attr(cint, "conf.level") <- conf.level
    rval <- list(conf.int = cint)
    class(rval) <- "htest"
    conf <- c(ll, ul)
    return(ll,ul)
}


x <- data.frame(1)
x$ll  <-  ll
x$ul  <-  ul
  

