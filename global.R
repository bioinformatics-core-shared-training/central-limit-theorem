#---
#title: "CLT Shiny app"
#author: "Dominique-Laurent Couturier"
#date: '`r format(Sys.time(), "Last modified: %d %b %Y")`'
#output: html_document
#runtime: shiny
#---


#```{r eval=FALSE}
require("shiny")
require("gamlss")


distribution = data.frame(pos        = 1:7,
    id          = c("NO","GA","BE","BI","WEI3","ZIP2","PO"),
    fullname    = c("Gaussian","Gamma","Beta","Bernoulli","Weibull","Zero-inflated Poisson","Poisson"),
    mu.low      = c(-100,1e-2,1e-2,1e-2,1e-2,1e-2,.25),
    mu.high     = c(100,100,1-1e-2,1,100,100,100),
    mu.value    = c(0,10,.5,.5,10,10,10),
    sigma.low   = c(0,1e-2,0.05,NA,2e-2,0,NA),
    sigma.high  = c(100,2,.95,NA,100,1,NA),
    sigma.value = c(1,1,.1,NA,50,.1,NA),
    x.low       = c(-Inf,1e-2,1e-2,0,0,0,0),
    x.high      = c(Inf,Inf,1-1e-2,1,Inf,Inf,Inf),
    row.names   = c("Gaussian","Gamma","Beta","Bernoulli","Weibull","Zero-inflated Poisson","Poisson"),
    mu.name     = c("Theoretical mean value","Theoretical probability of success value")[c(1,1,1,2,1,1,1)],
    sigma.name  = c("Theoretical standard deviation value","Theoretical shape parameter value","Theoretical probability to belong to the clump-at-zero")[c(1,2,2,NA,2,3,NA)],
    discrete    = c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE),
    stringsAsFactors = FALSE
    )[c(1,2,3,5,4,7,6),]

# shinyApp(ui, server)


