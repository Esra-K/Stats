rgenerator<- function(n){
rng <- vector(length = n)
m <- 2 ** 32
a <- 1103515245
c <- 12345
d <- as.numeric(Sys.time()) * 1000
  
  for (i in 1:n) {
  d <- (a * d + c) %% m
  rng[i] <- d 
  }
  return(rng)
}

dugen <- function(n, lowerbound, upperbound){
  rng2<- rgenerator(n)
  for (i in 1:n) {
    rng2[i] <- rng2[i] * (upperbound - lowerbound) / (2 ** 32)+ lowerbound
  }
  return(rng2)
}

visualdugen2<- function(n, lowerbound, upperbound){
  t<- dugen(n, lowerbound, upperbound)
  plot(t)
}

visualdugen<- function(n, lowerbound, upperbound){
  a<- dugen(n, lowerbound, upperbound)
  v<- data.frame("rvdugen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvdugen)) +
    geom_histogram()
}

cugen <- function(){
  a <- dugen(1, 0, 1)
  return(a)
}

brgen <- function(p){
  a<-dugen(1,0,1)
  #print(a)
  if(a<p){
    return(0)
  }
  else{
    return(1)
  }
}

visualbrgen2<- function(p){
  a<- c()
  for(i in 1:1000){
    a[i] <- brgen(p)
  }
  plot(a)
}

visualbrgen<- function(p){
  a<- c()
  for(i in 1:1000){
    a[i] <- brgen(p)
  }
  v<- data.frame("rvbrgen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvbrgen)) +
    geom_histogram()
}


bigen <- function(n,p){
  sum<-0
 for(i in 1:n){
 a<-brgen(p)
 sum<- sum+a
 }
  return(sum)
}

visualbigen2 <- function(n,p){
  a<- c()
  for(i in 1:1000){
    a[i] <- bigen(n,p)
  }
  plot(a)
}

visualbigen<- function(n,p){
  a<- c()
  for(i in 1:1000){
    a[i] <- bigen(n,p)
  }
  v<- data.frame("rvbigen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvbigen)) +
    geom_histogram()
}

gegen <-function(p){
  a <- 0
  while(a == 0){
    a <- brgen(p)
  }
  counter <- 0
  a <- brgen(p)
  while(a == 0){
    counter <- counter + 1
    a <- brgen(p)
  }
  return(counter)
}

visualgegen2 <-function(p){
  a<- c()
  for(i in 1:1000){
    a[i] <- gegen(p)
  }
  plot(a)
}


visualgegen<- function(p){
  a<- c()
  for(i in 1:1000){
    a[i] <- gegen(p)
  }
  v<- data.frame("rvgegen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvgegen)) +
    geom_histogram()
}


expgen <- function(landa){
  x<-dugen(1,0,1)
  return(((-1)/landa)*log(x))
}

visualexpgen2 <- function(landa){
  a<- c()
  for(i in 1:1000){
    a[i] <- expgen(landa)
  }
  plot(a)
}

visualexpgen<- function(landa){
  a<- c()
  for(i in 1:1000){
    a[i] <- expgen(landa)
  }
  v<- data.frame("rvexpgen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvexpgen)) +
    geom_histogram()
}


gagen<- function(k,landa){
  sum<- 0
  for(i in 1:k){
  sum <-sum + expgen(landa)
  }
  return(sum)
}

visualgagen2 <- function(k,landa){
  a<- c()
  for(i in 1:1000){
    a[i] <- gagen(k,landa)
  }
  plot(a)
}

visualgagen<- function(k,landa){
  a<- c()
  for(i in 1:1000){
    a[i] <- gagen(k,landa)
  }
  v<- data.frame("rvgagen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvgagen)) +
    geom_histogram()
}


pogen <- function(landa,t){
  sum <- 0
  counter <- -1
  while (sum <= t){
    sum <- sum + expgen(landa)
    counter <- counter + 1
  }
  return(counter)
}

visualpogen2 <- function(landa,t){
  a<- c()
  for(i in 1:1000){
    a[i] <- pogen(landa,t)
  }
  plot(a)
}

visualpogen<- function(landa,t){
  a<- c()
  for(i in 1:1000){
    a[i] <- pogen(landa,t)
  }
  v<- data.frame("rvpogen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvpogen)) +
    geom_histogram()
}

nogen <- function(s,u){
  a <- pogen(s,1)
  a <- a - s + u
  return(a)
}

visualnogen<- function(s,u){
  a<- rnorm(1000 , u , s)
#  for(i in 1:1000){
 #   a[i] <- nogen(s,u)
  #}
  
  v<- data.frame("rvnogen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvnogen)) +
    geom_histogram()
}


