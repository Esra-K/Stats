library(shiny)
library(ggplot2)

library(shinythemes)


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

cugen <- function(n){
  rng2 <- runif(n)
  for (i in 1:n) {
    rng2[i] <- rng2[i]  / (2 ** 32)
  }
  return(rng2)
}

dugen <- function(n, lowerbound, upperbound){
  rng2<- cugen(n)
  for (i in 1:n) {
    rng2[i] <- rng2[i] * (upperbound - lowerbound) + lowerbound
  }
  return(rng2)
}


mledugen <- function(v)
{ 
  a<- min(v)
  b<- max(v)
  parameter <- c(a,b)
  return(parameter)
}

visualdugen<- function(n, lowerbound, upperbound){
  a<- dugen(n, lowerbound, upperbound)
  v<- data.frame("rvdugen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvdugen),c) +
    geom_histogram(color = "black",fill = "blue")
  
  
}

brgen <- function(p){
  a<-rnorm(1,0,1)
  if(a<p){
    return(1)
  }
  else{
    return(0)
  }
}

mlebrgen <- function(v)
{ 
  sum <- 0 #v[1] + v[2] + .... + v[n]
  for(j in 1:length(v)){
    sum <- sum + v[j]
  }
  
  p <- (sum)/length(v)
  parameter <- c(p)
  return(parameter)
}


visualbrgen<- function(n,p){
  a<- c()
  for(i in 1:n){
    a[i] <- brgen(p)
  }
  v<- data.frame("rvbrgen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvbrgen)) +
    geom_histogram(color = "black",fill = "green")
}


bigen <- function(tedad,p){
  sum<-0
  for(i in 1:tedad){
    a<-brgen(p)
    sum<- sum+a
  }
  return(sum)
}

mlebigen <- function(v)
{ 
  sum <- 0
  for(j in 1:length(v)){
    sum <- sum + v[j]
  }
  
  p <- (sum)/((length(v))^(2))
  parameter <- c(p)
  return(parameter)
}


visualbigen<- function(n,tedad,p){
  a<- c()
  for(i in 1:n){
    a[i] <- bigen(tedad,p)
  }
  v<- data.frame("rvbigen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvbigen)) +
    geom_histogram(color = "black",fill = "yellow")
}

gegen <-function(p){
  counter <- 1
  a <- brgen(p)
  while(a == 0){
    counter <- counter + 1
    a <- brgen(p)
  }
  return(counter)
}

mlegegen <- function(v)
{ 
  sum <- 0
  for(j in 1:length(v)){
    sum <- sum + v[j]
  }
  p <- length(v)/(sum)
  parameter <- c(p)
  return(parameter)
}




visualgegen<- function(n,p){
  a<- c()
  for(i in 1:n){
    a[i] <- gegen(p)
  }
  v<- data.frame("rvgegen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvgegen)) +
    geom_histogram(color = "black",fill = "magenta")
}


expgen <- function(landa){
  x<-runif(1,0,1)
  #print(x)
  return(((-1)/landa)*(log(x)))
}


mleexpgen <- function(v)
{ 
  sum <- 0
  for(j in 1:length(v)){
    sum <- sum + v[j]
  }
  p <- length(v)/(sum)
  parameter <- c(p)
  return(parameter)
}

visualexpgen<- function(n,landa){
  a<- c()
  for(i in 1:n){
    a[i] <- expgen(landa)
  }
  v<- data.frame("rvexpgen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvexpgen)) +
    geom_histogram(color = "black",fill = "cyan")
}


gagen<- function(k,landa){
  sum<- 0
  for(i in 1:k){
    sum <-sum + expgen(landa)
  }
  return(sum)
}


mlegagen <- function(v)
{ 
  sum <- 0
  for(j in 1:length(v)){
    sum <- sum + v[j]
  }
  sum1 <- 0
  
  for(k in 1:length(v)){
    sum1 <- sum1 + log(v[k])
  }
  
  alpa <- (0.5/(log(sum/length(v)) - (sum1/length(v))))
  beta <- sum/(length(v)*alpa) 
  parameter <- c(alpa,1/beta)
  return(parameter)
}


visualgagen<- function(n,k,landa){
  a<- c()
  for(i in 1:n){
    a[i] <- gagen(k,landa)
  }
  v<- data.frame("rvgagen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvgagen)) +
    geom_histogram(color = "black",fill = "red")
}


pogen <- function(landa,t){
  sum <- 0
  counter <- (-1)
  while (sum <= t){
    sum <- sum + expgen(landa)
    counter <- counter + 1
  }
  return(counter)
}

mlepogen <- function(v)
{ 
  sum <- 0
  for(j in 1:length(v)){
    sum <- sum + v[j]
  }
  landa <- (sum)/length(v)
  parameter <- c(landa)
  return(parameter)
}


visualpogen<- function(n,landa,t){
  a<- c()
  for(i in 1:n){
    a[i] <- pogen(landa,t)
  }
  v<- data.frame("rvpogen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvpogen)) +
    geom_histogram(color = "black",fill = "orange")
}

nogen <- function(s,u){
  # a <- pogen(s,1)
  #  a <- (a - u)/sqrt(s)
  return(rnorm(1,u,sqrt(s)))
}


mlenogen <- function(v)
{ 
  sum <- 0
  for(j in 1:length(v)){
    sum <- sum + v[j]
  }
  u <- (sum)/length(v)
  sum1 <- 0
  for(k in 1:length(v)){
    sum1 <- sum1 + ((v[k]-u)^(2))
  }
  s <- (sum1)/length(v)
  parameter <- c(s,u)
  return(parameter)
}


visualnogen<- function(n,s,u){
  a <- c()
  
  for(i in 1:n){
    a[i] <- nogen(s,u)
  }
  
  v<- data.frame("rvnogen"=a)
  library(ggplot2)
  ggplot(v, aes(x = rvnogen)) +
    geom_histogram(color = "black",fill = "darkred")
}


ui <-
    navbarPage(
      
      theme = shinytheme("simplex"),
  
      title = "Random Variable",
      
      navbarMenu(title = "Uniform",
      tabPanel(title = "Uniform Distribution",
          
                 actionButton("unig","generate RV"),
                 actionButton("unih","plot distribution"),
                 sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                 numericInput("low","Lower bound:",0),
                 numericInput("up","Upper bound:",100),
                 
                 mainPanel(
                 plotOutput("uniP"),
                 verbatimTextOutput("uniT",placeholder = TRUE)
                 )
              
      ),
      tabPanel(title = "Uniform ML",
               actionButton("uniE","Estimator"),
               actionButton("unig2","generate RV"),
               actionButton("unih2","plot distribution"),
              
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               textAreaInput('uniV','input numbers:',"0,1",placeholder = "enter a vector!"),
               hr(),
               
               mainPanel(
                 verbatimTextOutput("uni1",TRUE),
                 verbatimTextOutput("uni2",TRUE),
                 plotOutput("uniP2"),
                 verbatimTextOutput("uniT2",FALSE)
               )
               )
      ), 
      navbarMenu(title = "Bernoli",
              tabPanel(title = "Bernoli Distribution",
               actionButton("brg","generate RV"),
               actionButton("brh","plot distribution"),
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               numericInput("p","posibility:",0.5),
               
               
               mainPanel(
                 plotOutput("brP"),
                 verbatimTextOutput("brT",placeholder = TRUE)
               )
               ),
              tabPanel(title = "Bernoli ML",
                       actionButton("brE","Estimator"),
                       actionButton("brg2","generate RV"),
                       actionButton("brh2","plot distribution"),
                       
                       sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                       textAreaInput("brV","input numbers:","0,1",placeholder = "enter a vector!"),
                       hr(),
                       mainPanel(
                         verbatimTextOutput("br1",TRUE),
                         plotOutput("brP2"),
                         verbatimTextOutput("brT2",FALSE)
                       )
              )),
      navbarMenu("Binomial",
              tabPanel(title = "Binomial Distribution",
               actionButton("big","generate RV"),
               actionButton("bih","plot distribution"),
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               numericInput("p","posibility:",0.5),
               numericInput("tedad","number:",100),
               
               
               mainPanel(
                 plotOutput("biP"),
                 verbatimTextOutput("biT",placeholder = TRUE)
               )
               )
              ,
              tabPanel(title = "Binomial ML",
                       actionButton("biE","Estimator"),
                       actionButton("big2","generate RV"),
                       actionButton("bih2","plot distribution"),
                       
                       sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                       textAreaInput("biV","input numbers:","0,1",placeholder = "enter a vector!"),
                       hr(),
                       mainPanel(
                         verbatimTextOutput("bi1",TRUE),
                         plotOutput("biP2"),
                         verbatimTextOutput("biT2",FALSE)
                       )
              )),
      navbarMenu(title = "Geometric",
                tabPanel(title = "Geometric Distribution",
               actionButton("geog","generate RV"),
               actionButton("geoh","plot distribution"),
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               numericInput("p","posibility:",0.5),
               
               
               mainPanel(
                 plotOutput("geoP"),
                 verbatimTextOutput("geoT",placeholder = TRUE)
               )
               )
               ,
               tabPanel(title = "Geometric ML",
                        actionButton("geoE","Estimator"),
                        actionButton("geog2","generate RV"),
                        actionButton("geoh2","plot distribution"),
                        
                        sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                        textAreaInput("geoV","input numbers:","0,1",placeholder = "enter a vector!"),
                        hr(),
                        mainPanel(
                          verbatimTextOutput("geo1",TRUE),
                          plotOutput("geoP2"),
                          verbatimTextOutput("geoT2",FALSE)
                        )
               )),
      navbarMenu("Exponential",
              tabPanel(title = "Exponential Distribution",
               actionButton("expg","generate RV"),
               actionButton("exph","plot distribution"),
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               numericInput("landa","Landa:",1),
               
               
               
               mainPanel(
                 plotOutput("expP"),
                 verbatimTextOutput("expT",placeholder = TRUE)
                 
               )),
              tabPanel(title = "Exponential ML",
                       actionButton("expE","Estimator"),
                       actionButton("expg2","generate RV"),
                       actionButton("exph2","plot distribution"),
                       
                       sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                       textAreaInput("expV","input numbers:","0,1",placeholder = "enter a vector!"),
                       hr(),
                       mainPanel(
                         verbatimTextOutput("exp1",TRUE),
                         plotOutput("expP2"),
                         verbatimTextOutput("expT2",FALSE)
                       )
              )
              ),
      navbarMenu(title = "Gamma",
              tabPanel( title = "Gamma Distribution",
               actionButton("gag","generate RV"),
               actionButton("gah","plot distribution"),
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               numericInput("k","k:",1),
               numericInput("landa","Landa:",1),
               
               
               mainPanel(
                 plotOutput("gaP"),
                 verbatimTextOutput("gaT",placeholder = TRUE )
               )
               ),
              tabPanel(title = "Gamma ML",
                       actionButton("gaE","Estimator"),
                       actionButton("gag2","generate RV"),
                       actionButton("gah2","plot distribution"),
                       
                       sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                       
                       textAreaInput("gaV","input numbers:","0,1",placeholder = "enter a vector!"),
                       hr(),
                       mainPanel(
                         verbatimTextOutput("ga1",TRUE),
                         verbatimTextOutput("ga2",TRUE),
                         plotOutput("gaP2"),
                         verbatimTextOutput("gaT2",FALSE)
                       )
              )
              ),
      navbarMenu("Poassion",
              tabPanel( title = "Poassion Distribution",
               actionButton("pog","generate RV"),
               actionButton("poh","plot distribution"),
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               numericInput("landa","Landa:",1),
               numericInput("t","time:",10),
               
               
               mainPanel(
                 plotOutput("poP"),
                 verbatimTextOutput("poT",placeholder = TRUE)
               )
               ),
              tabPanel(title = "Poassion ML",
                       actionButton("poE","Estimator"),
                       actionButton("pog2","generate RV"),
                       actionButton("poh2","plot distribution"),
                       
                       sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                       textAreaInput("poV","input numbers:","0,1",placeholder = "enter a vector!"),
                       hr(),
                       mainPanel(
                         verbatimTextOutput("po1",TRUE),
                         plotOutput("poP2"),
                         verbatimTextOutput("poT2",FALSE)
                       )
              )),
      navbarMenu(title = "Normal",
              tabPanel( title = "Normal Distribution",
               actionButton("nog","generate RV"),
               actionButton("noh","plot distribution"),
               sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
               numericInput("s","s:",1),
               numericInput("u","u:",1),
               
               
               mainPanel(
                 plotOutput("noP"),
                 verbatimTextOutput("noT",placeholder = TRUE)
               )
               ),
              tabPanel(title = "Normal ML",
                       actionButton("noE","Estimator"),
                       actionButton("nog2","generate RV"),
                       actionButton("noh2","plot distribution"),
                       
                       sliderInput(inputId="n","Sample Size n",value=100,min=10,max=100000,step=5),
                       textAreaInput("noV","input numbers:","0,1",placeholder = "enter a vector!"),
                       hr(),
                       mainPanel(
                         verbatimTextOutput("no1",TRUE),
                         verbatimTextOutput("no2",TRUE),
                         plotOutput("noP2"),
                         verbatimTextOutput("noT2",FALSE)
                       )
              )
      )
  )



server <- function(input, output, session) {
  

   uniFP <- eventReactive(input$unih,{ visualdugen(input$n,input$low,input$up) })
   brFP  <- eventReactive(input$brh, { visualbrgen(input$n,input$p)})
   biFP  <- eventReactive(input$bih, { visualbigen(input$n,input$tedad,input$p) })
   geoFP <- eventReactive(input$geoh,{ visualgegen(input$n,input$p) })
   expFP <- eventReactive(input$exph,{ visualexpgen(input$n,input$landa) })
   gaFP  <- eventReactive(input$gah, { visualgagen(input$n,input$k,input$landa)})
   poFP  <- eventReactive(input$poh, { visualpogen(input$n,input$landa,input$t) })
   noFP  <- eventReactive(input$noh, { visualnogen(input$n,input$s,input$u) })
   
   
   
   uniFP2 <- eventReactive(input$unih2,{
     x <- c()
     x <- uniFM()
   visualdugen(input$n,x[1],x[2]) })
   
   brFP2  <- eventReactive(input$brh2, { 
     x <- c()
     x <- brFM()
     visualbrgen(input$n,x[1]) })
   
   biFP2  <- eventReactive(input$bih2, {
     x <- c()
     x <- biFM()
     visualbigen(input$n,100,x[1]) }
    )
   geoFP2 <- eventReactive(input$geoh2,{ 
     x <- c()
     x <- geoFM()
     visualgegen(input$n,x[1]) })
   
   expFP2 <- eventReactive(input$exph2,{    
    x <- c()
   x <- expFM()
   visualexpgen(input$n,x[1]) 
   })
   gaFP2  <- eventReactive(input$gah2, { 
     x <- c()
     x <- gaFM()
     visualgagen(input$n,x[1],x[2]) }
     )
   poFP2  <- eventReactive(input$poh2, {
     x <- c()
     x <- poFM()
     visualpogen(input$n,x[1],10) })
   
   noFP2  <- eventReactive(input$noh2, { 
     x <- c()
     x <- noFM()
     visualnogen(input$n,x[1],x[2]) })
   
   
   
 
  
  output$uniP <- renderPlot({
      uniFP()
  })
  output$brP <- renderPlot({
    brFP()
  })
  output$biP <- renderPlot({
    biFP()
  })
  output$geoP <- renderPlot({
    geoFP()
  })
  output$expP <- renderPlot({
    expFP()
  })
  output$gaP <- renderPlot({
    gaFP()
  })
  output$poP <- renderPlot({
    poFP()
  })
  output$noP <- renderPlot({
    noFP()
  })
  
  
  
  
  output$uniP2 <- renderPlot({
    if(input$uniE == 0)
      return()
    uniFP2()
  })
  output$brP2 <- renderPlot({
    if(input$brE == 0)
      return()
    brFP2()
  })
  output$biP2 <- renderPlot({
    if(input$biE == 0)
      return()
    biFP2()
  })
  output$geoP2 <- renderPlot({
    if(input$geoE == 0)
      return()
    geoFP2()
  })
  output$expP2 <- renderPlot({
    if(input$expE == 0)
      return()
    expFP2()
  })
  output$gaP2 <- renderPlot({
    if(input$gaE == 0)
      return()
    gaFP2()
  })
  output$poP2 <- renderPlot({
    if(input$poE == 0)
      return()
    poFP2()
  })
  output$noP2 <- renderPlot({
    if(input$noE == 0)
      return()
    noFP2()
  })
  
  
  uniFT <- eventReactive(input$unig,{ dugen(1,input$low,input$up) })
  brFT  <- eventReactive(input$brg, { brgen(input$p)})
  biFT  <- eventReactive(input$big, { bigen(input$tedad,input$p) })
  geoFT <- eventReactive(input$geog,{ gegen(input$p) })
  expFT <- eventReactive(input$expg,{ expgen(input$landa) })
  gaFT  <- eventReactive(input$gag, { gagen(input$k,input$landa)})
  poFT   <- eventReactive(input$pog, {  pogen(input$landa,input$t) })
  noFT   <- eventReactive(input$nog, {  nogen(input$s,input$u) })
  
  
  
  
  
  
  uniFT2 <- eventReactive(input$unig2,{ 
    x <- c()
    x <- uniFM()
    dugen(1,x[1],x[2]) })
  brFT2  <- eventReactive(input$brg2, { x <- c()
  x <- brFM()
  brgen(x[1])})
  biFT2  <- eventReactive(input$big2, { 
    x <- c()
    x <- biFM()
    bigen(100,x[1])
  })
  geoFT2 <- eventReactive(input$geog2,{ 
    x <- c()
    x <- geoFM()
    gegen(x[1])})
  expFT2 <- eventReactive(input$expg2,{ 
    x <- c()
    x <- expFM()
    expgen(x[1])
  })
  gaFT2  <- eventReactive(input$gag2, { 
    x <- c()
    x <- gaFM()
    gagen(x[1],x[2])
    })
  poFT2   <- eventReactive(input$pog2, {  
    x <- c()
    x <- poFM()
    pogen(x[1],10)})
  noFT2   <- eventReactive(input$nog2, { 
    x <- c()
    x <- noFM()
    nogen(x[1],x[2])
    })
  
  
  
  output$uniT <- renderText({
    paste0("RV = ",uniFT())
  })
  output$brT <- renderText({
    paste0("RV = ",brFT())
  })
  output$biT <- renderText({
    paste0("RV = ",biFT())
  })
  output$geoT <- renderText({
    paste0("RV = ",geoFT())
  })
  output$expT <- renderText({
    paste0("RV = ",expFT())
  })
  output$gaT <- renderText({
    paste0("RV = ",gaFT())
  })
  output$poT <- renderText({
    paste0("RV = ",poFT())
  })
  output$noT <- renderText({ 
    paste0("RV = ",noFT())
  })
  
  
  
  output$uniT2 <- renderText({
    if(input$uniE == 0)
      return()
    paste0("RV = ",uniFT2())
  })
  output$brT2 <- renderText({
    if(input$brE == 0)
      return()
    paste0("RV = ",brFT2())
  })
  output$biT2 <- renderText({
    if(input$biE == 0)
      return()
    paste0("RV = ",biFT2())
  })
  output$geoT2 <- renderText({
    if(input$geoE == 0)
      return()
    paste0("RV = ",geoFT2())
  })
  output$expT2 <- renderText({
    if(input$expE == 0)
      return()
    paste0("RV = ",expFT2())
  })
  output$gaT2 <- renderText({
    if(input$gaE == 0)
      return()
    paste0("RV = ",gaFT2())
  })
  output$poT2 <- renderText({
    if(input$poE == 0)
      return()
    paste0("RV = ",poFT2())
  })
  output$noT2 <- renderText({ 
    if(input$noE == 0)
      return()
    paste0("RV = ",noFT2())
  })
  
  
  uniFM <- eventReactive(input$uniE,{
    x <- c()
    x <- as.numeric(unlist(strsplit(input$uniV,",")))
    mledugen(x)})
  
  brFM  <- eventReactive(input$brE, {
    x <- c()
    x <-as.numeric(unlist(strsplit(input$brV,",")))
    mlebrgen(x)})
  
  biFM  <- eventReactive(input$biE, {
    x <- c()
    x <-as.numeric(unlist(strsplit(input$biV,",")))
    mlebigen(x)})
  
  geoFM  <- eventReactive(input$geoE, {
    x <- c()
    x <-as.numeric(unlist(strsplit(input$geoV,",")))
    mlegegen(x)})

  expFM  <- eventReactive(input$expE, {
    x <- c()
    x <-as.numeric(unlist(strsplit(input$expV,",")))
    mleexpgen(x)})
  
  gaFM  <- eventReactive(input$gaE, {
    x <- c()
    x <-as.numeric(unlist(strsplit(input$gaV,",")))
    mlegagen(x)})
  
  poFM  <- eventReactive(input$poE, {
    x <- c()
    x <-as.numeric(unlist(strsplit(input$poV,",")))
    mlepogen(x)})
  
  noFM  <- eventReactive(input$noE, {
    x <- c()
    x <-as.numeric(unlist(strsplit(input$noV,",")))
    mlenogen(x)})

  
  
  output$uni1 <- renderText({
    if(input$uniV == "")
      return()
    x <- c()
    x <- uniFM()
    
    paste0("low bound = ",x[1])
  })
  output$uni2 <- renderText({
    if(input$uniV == "")
      return()
    x <- c()
    x <- uniFM()
    paste0("up bound = ",x[2])
  })
  output$br1 <- renderText({
    if(input$brV == "")
      return()
    x <- c()
    x <- brFM()
    paste0("p = ",x[1])
  })
  output$bi1 <- renderText({
    if(input$biV == "")
      return()
    x <- c()
    x <- biFM()
    paste0("p = ",x[1])
  })
  output$bi2 <- renderText({
    if(input$biV == "")
      return()
    x <- c()
    x <- biFM()
    paste0("n = ",x[2])
  })

  output$geo1 <- renderText({
    if(input$geoV == "")
      return()
    x <- c()
    x <- geoFM()
    paste0("p = ",x[1])
  })
  output$exp1 <- renderText({
    if(input$expV == "")
      return()
    x <- c()
    x <- expFM()
    paste0("Landa = ",x[1])
  })
  output$ga1 <- renderText({
    if(input$gaV == "")
      return()
    x <- c()
    x <- gaFM()
    paste0("k = ",x[1])
  })
  output$ga2 <- renderText({
    if(input$gaV == "")
      return()
    x <- c()
    x <- gaFM()
    paste0("landa = ",x[2])
  })
  output$po1 <- renderText({
    if(input$poV == "")
      return()
    x <- c()
    x <- poFM()
    paste0("landa = ",x[1])
  })

  output$no1 <- renderText({
    if(input$noV == "")
      return()
    x <- c()
    x <- noFM()
    paste0("s = ",x[1])
  })
  output$no2 <- renderText({ 
    if(input$noV == "")
      return()
    x <- c()
    x <- noFM()
    paste0("u = ",x[2])
  })


}



r <- 10000 # Number of replications... must be ->inf for sampling distribution!

#palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
   #       "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))


shinyApp(ui, server)

