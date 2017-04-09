library(shiny)

ui<- fluidPage(
	titlePanel("Central Limit Theorem: Simulation"),
sidebarLayout(
    sidebarPanel(width=3, offset=200, selectInput(inputId="src.dist", label="Distribution", 
	choices=c("Binomial(n,p)"="B","Geometric(p)"="G","Poisson(λ)"="P","Exponential(λ)"="E","Normal(μ,σ²)"="N","Uniform[a,b]"="U"),
	selected="B", multiple=FALSE, selectize=FALSE),
	numericInput(inputId="param1", label="First Parameter for your Distribution:", value=20),
	numericInput(inputId="param2", label="Second Parameter for your Distribution, if any:", value=0.3),
	sliderInput(inputId = "n1", "Sample size (small)", value = 2, min=2,max=40),
	sliderInput(inputId = "n2", "Sample size (large)", value = 200, min=40,max=1000, step=10)),
    mainPanel(width=9, plotOutput(outputId = "plot",height = 750)))
	 ) 

server<- function(input,output){
	output$plot<-renderPlot({
	r = 10000
	nbreaks = 50
	src.dist = input$src.dist
	n1=input$n1
	n2=input$n2
	param1=input$param1
	param2=input$param2
   random.samples <- switch(src.dist,
	"B" = matrix(rbinom(n2*r,param1,param2),r),
	"G" = matrix(rgeom(n2*r,param1),r),
	"P" = matrix(rpois(n2*r,param1),r),
	"E" = matrix(rexp(n2*r,param1),r),
        "N" = matrix(rnorm(n2*r,param1,sqrt(param2)),r),
	"U" = matrix(runif(n2*r,param1,param2),r))
   n1.sample.means <- apply(random.samples[,1:n1],1,mean)   
   n2.sample.means <- apply(random.samples,1,mean)   
   pred.mean<-switch(src.dist,
	"E" = 1/param1,
	"G" = 1/param1 -1,
	"N" = param1,
	"U" = (param1+param2)/2,
	"P" = param1,
	"B" = param1*param2)
   pred.var<-switch(src.dist,
	"E" = 1/param1^2,
	"G" = (1-param1)/param1^2,
	"N" = param2,
	"U" = (param2-param1)^2/12,
	"P" = param1,
	"B" = param1*param2*(1-param2))
   pred.sd1<-sqrt(pred.var/n1)
   pred.sd2<-sqrt(pred.var/n2)
   name.dist<-switch(src.dist,
	"E" = paste("X ~ Exponential(", param1 ,")", sep = ""),
	"G" = paste("X ~ Geometric(", param1 ,")", sep = ""),
	"N" = paste("X ~ Normal(", param1,",",param2,")", sep = ""),
	"U" = paste("X ~ Continuous Uniform [", param1,",",param2 ,"]", sep = ""),
	"P" = paste("X ~ Poisson(", param1 ,")", sep = ""),
	"B" = paste("X ~ Binomial(", param1,",",param2,")", sep = ""))
   par(mfrow=c(3,1))
   hist(random.samples[,1],xlab=name.dist,col="gray",breaks=nbreaks,prob=TRUE,main=paste("One ", name.dist, " random variable. ",r, " samples.", sep=""))
   	abline(v=pred.mean,col="red", lwd=2)
   hist(n1.sample.means,col="gray",breaks=nbreaks,prob=TRUE,xlab=paste(n1," sample means. SD=",round(pred.sd1,digits=4),sep=""),
	main=paste("Sampling Distribution: n  = ",n1,sep=""))
	abline(v=pred.mean,col="red", lwd=2)
   	curve(dnorm(x,mean=pred.mean,sd=pred.sd1), col="darkblue", lwd=3, add=TRUE)
   hist(n2.sample.means,col="gray",breaks=nbreaks,prob=TRUE,xlab=paste(n2," sample means. SD=",round(pred.sd2,digits=4),sep=""),
	main=paste("Sampling Distribution: n = ",n2,sep=""))   
   	curve(dnorm(x,mean=pred.mean,sd=pred.sd2), col="darkblue", lwd=3,  add=TRUE)
   	abline(v=pred.mean,col="red", lwd=2)})

}

shinyApp(ui=ui,server=server)

