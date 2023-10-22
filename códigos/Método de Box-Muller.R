
normal.bm <- function(x){
  n <- x/2
  
  u1 <- runif(n,min=0,max=1)
  u2 <- runif(n,min=0,max=1)
  
  x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
  x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
    
  z <- c(x1,x2)
  
  return(z)
}

z <- normal.bm(1000)
boxplot(z)



