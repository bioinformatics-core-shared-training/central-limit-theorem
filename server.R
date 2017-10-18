function(input, output) {

    v <- reactiveValues(simul = FALSE)

    output$choose_distr <- renderUI({
        selectInput(inputId = "distr","Statistical distribution",choices=distribution$fullname)
        })
    
    output$choose_mu <- renderUI({
        if (is.null(input$distr)) return()
        sliderInput("mu",
                    distribution[input$distr,"mu.name"],
                    min = distribution[input$distr,"mu.low"],
                    max = distribution[input$distr,"mu.high"],
                    value = distribution[input$distr,"mu.value"],round=-2)
        })

    output$choose_sigma <- renderUI({
        if (is.null(input$distr)) return()  
        
        if(input$distr!="Bernoulli"&input$distr!="Poisson"){
            sliderInput("sigma",
                        distribution[input$distr,"sigma.name"],
                        min = distribution[input$distr,"sigma.low"],
                        max = distribution[input$distr,"sigma.high"],
                        value = distribution[input$distr,"sigma.value"],round=-2)
            }
        })
        
        
    output$choose_n <- renderUI({
        sliderInput("n",
                    "Sample size of simulated sample",
                    min = 5,
                    max = 1000,
                    value = 50,
                    step = 5)
        })
        
    output$choose_R <- renderUI({
        sliderInput("R",
                    "Number of simulated samples",
                    min = 1000,
                    max = 10000,
                    value = 10000)
        })

    observeEvent(input$sidebar, {
        v$simul <- FALSE
    })  
    
    observeEvent(input$go, {
        v$simul <- input$go
    })
            
    simulationResults <- reactive({
          # input = list(mu=10,sigma=50,distr="Gaussian",R=1000,n=10)
          y_r = lapply(as.list(rep(NA,input$R)),
                   function(x,par){
                       do.call(paste0("r",distribution[input$distr,"id"]),
                               list(n=par$n,mu=par$mu,sigma=par$sigma)[1:(2+!is.na(distribution[input$distr,"sigma.value"]))])
                   },par=input)
          # queen:
          mean_r  = unlist(lapply(y_r,mean))
          var_r   = unlist(lapply(y_r,var))
          y_r     = y_r[1:5]
          # king: coverage of asymp confidence intervals:
          isin = function(ci,theta){if(!any(is.na(ci))){if(theta>=ci[1]&theta<=ci[2]){T}else{F}}else{NA}}
          ci_r = t(apply(cbind(mean_r,sqrt(var_r/input$n)),1,function(x,mu){
                             x[1]+qt(.975,input$n-1)*x[2]*c(-1,1)}))
          #ci_r = t(apply(cbind(mean_r,sqrt(var_r/input$n)),1,function(x,mu){
          #                   x[1]+qnorm(.975)*x[2]*c(-1,1)}))
          coverage_r = apply(ci_r,1,isin,theta=input$mu)
          # output:
          data = list(y_r = y_r, mean_r=mean_r,var_r=var_r,ci_r=ci_r,coverage_r=coverage_r)
          data
    })
    
   output$density <- renderPlot({
        if (v$simul == FALSE) return()

        isolate({
            # data
            data <- simulationResults()
            # axis
            x0range = do.call(paste0("q",distribution[input$distr,"id"]),
                              list(p=c(0.0005,0.9995),mu=input$mu,sigma=input$sigma)[1:(2+!is.na(distribution[input$distr,"sigma.value"]))])
            x0      = if(distribution[input$distr,"discrete"]){
                          seq(max(c(x0range[1],distribution[input$distr,"x.low"])),min(c(x0range[2]),distribution[input$distr,"x.high"]))
                      }else{
                          seq(max(c(x0range[1],distribution[input$distr,"x.low"])),min(c(x0range[2]),distribution[input$distr,"x.high"]),length=1000)
                      }
            d.x0    = do.call(paste0("d",distribution[input$distr,"id"]),
                              list(x0,mu=input$mu,sigma=input$sigma)[1:(2+!is.na(distribution[input$distr,"sigma.value"]))])
            xlim0   = range(c(x0,unlist(data$y_r)))
            ylim0   = max(d.x0)*c(-1,1)#max(d.x0),max(d.x0))
            ## plots:
            par(mfrow=c(1,1))
            # plot 1:
            plot(1,1,xlim=xlim0,ylim=ylim0,pch="",axes=FALSE,
                 xlab="",ylab="")
            
            # axes
            if(distribution[input$distr,"discrete"]){axis(1,pos=0,at=x0)}else{axis(1,pos=0)}
            axis(2,at=c(0,axTicks(2)[axTicks(2)>0]),las=2)
            abline(h=0,col="black")
            axis(4,at=c(-mean(c(ylim0[2]*.3,ylim0[2]*.7)),ylim0[2]/2),
                 labels=c("5 simulated\nsamples",ifelse(distribution[input$distr,"discrete"],"Probability","Density")),tick=FALSE)
            
            # density and real mean
            if(distribution[input$distr,"discrete"]){
                for(i in 1:length(x0)){
                    segments(x0[i],0,x0[i],d.x0[i],col="blue")
                    points(x0[i],d.x0[i],col="blue",pch=19)
                    }
            }else{
                lines(x0,d.x0,col="blue")
            }
            abline(v=input$mu,col="blue",lty=3,lwd=2)
            axis(3,at=input$mu,labels=expression(mu),tick=FALSE,col.axis="blue")
            
            # simulated samples:
            R = length(data$y_r)
            pos.r = seq(-ylim0[2]*.3,-ylim0[2]*.7,length=R) 
            abline(h=pos.r,lwd=.5,col="light gray")
            axis(2,at=pos.r,labels=paste("Sample",1:R),tick=FALSE,las=2,cex.axis=.75)
            for(r in 1:length(data$y_r)){# r=1
                if(distribution[input$distr,"discrete"]){
                    points(jitter(data$y_r[[r]],.05),jitter(rep(pos.r[r],input$n),ylim0[2]*1.5),pch=21,col=paste0(substr(rainbow(R)[r],1,7),30))
                }else{
                    points(data$y_r[[r]],rep(pos.r[r],input$n),pch=21,col=paste0(substr(rainbow(R)[r],1,7),60))
                }
                points(mean(data$y_r[[r]]),pos.r[r],col="black",pch=3,lwd=1.25)
            }
            # legend
            legend("bottom",legend="Sample mean",pch=3,col="black",box.lwd=.2,bg ="white")
            })
        })#,width=750,height=750)

        
   output$mean <- renderPlot({
        if (v$simul == FALSE) return()

        isolate({        
            # data
            data <- simulationResults()
            # prepare
            hist_muhat = hist(data$mean_r,plot=FALSE,nclass=ceiling(sqrt(input$R)))
            mean_muhat = mean(data$mean_r)
            sdev_muhat = sqrt(var(data$mean_r))
            # limits
            xlim1   = input$mu+c(-1,1)*abs(max(data$mean_r-input$mu))
            x1      = seq(xlim1[1],xlim1[2],length=1000)
            d.x1    = dnorm(x1,mean_muhat,sdev_muhat)      
            ylim1   = c(0,max(c(unlist(d.x1),hist_muhat$density))*1.2)
            
            ## plots
            par(mfrow=c(1,2))
            # plot estimation of density of the mean
            plot(1,1,xlim=xlim1,ylim=ylim1,pch="",axes=FALSE,
                 xlab="mean values",ylab="",main="Estimated density of the mean")
            hist(data$mean_r,col="light gray",border="light gray",nclass=ceiling(sqrt(input$R)),add=TRUE,prob=TRUE)
            axis(1,pos=0)
            axis(2,las=2)
            abline(h=0,col="black")
            lines(x1,d.x1,col="blue")
            abline(v=input$mu,col="blue",lty=3,lwd=2)
            axis(3,at=input$mu,labels=expression(mu),tick=FALSE,col.axis="blue",padj=1)
            legend("topright",legend="Normal approximation",lty=1,col="blue",box.lwd=NA,cex=.75)
            # add mean of samples 1 to 5
            R = length(data$y_r)
            for(r in 1:length(data$y_r)){# r=1
                points(mean(data$y_r[[r]]),0,pch=19,col=rainbow(R)[r])
            }        
            # plot qq norm
            qqnorm((data$mean_r-mean_muhat)/sdev_muhat,col="gray",axes=FALSE)
            axis(1)
            axis(2,las=2)
            abline(0,1,col="blue")
            })
        })


   output$coverage <- renderPlot({
        if (v$simul == FALSE) return()

        isolate({                
            # data
            data <- simulationResults()
            # prepare
            # limits
            xlim1   = input$mu+c(-1,1)*max(abs(data$ci_r-input$mu))
            x1      = seq(xlim1[1],xlim1[2],length=1000)
            ylim1   = c(-15,120)
            
            ## plots
            par(mfrow=c(1,1))
            # plot estimation of density of the mean
            plot(1,1,xlim=xlim1,ylim=ylim1,pch="",axes=FALSE,
                 xlab=paste0("Estimated coverage of 95% CI over ",input$R," samples = ",round(mean(data$coverage_r)*100,2),"%"), 
                 ylab="",main="95% asymptotic CI for the mean",col.lab="blue")
            axis(1,pos=15)
            axis(2,at=c(seq(120,21,-10),21),label=c(1,seq(10,100,10)),las=2,tick=FALSE)
            axis(4,at=70,label="100 first simulated samples",tick=FALSE)        
            axis(3,at=input$mu,labels=expression(mu),tick=FALSE,col.axis="blue",padj=1)
            # add mean of samples 1 to 5
            R = 100
            for(r in 1:R){# r=1
                if(data$ci_r[r,2]-data$ci_r[r,1]>(1e-5)){
                    arrows(data$ci_r[r,1],120-r+1,data$ci_r[r,2],120-r+1,code=3,length=.01,angle=90,col=c("red","gray")[data$coverage_r[r]+1])
                    }
            }   
            abline(v=input$mu,col="blue",lty=3,lwd=2)
            legend("bottom",legend=c("CI contains true mean value","CI does not contain true mean value"),lty=c(1,1),col=c("red","gray"),box.lwd=0.2,bg="white",cex=.75)
            })
        })

    }

