library(shiny)
library(ggplot2)
function(input,output,session) {
  values <- reactiveValues(flowData = NULL)
  title <- reactiveValues(theName = NULL)
  thePlot <- reactiveValues(fitPlot = NULL)
  counter <- reactiveValues(i = 0)
  observeEvent(input$flowFile, {
    removeUI(selector='#fileContent')
    removeUI(selector='#fitPlot')
    removeUI(selector='#downloadButtons')
    fname <- substr(input$flowFile$name,0,nchar(input$flowFile$name)-4)
    updateTextInput(session,"plotName",value=fname)
    insertUI(selector='#mainPanel',where="afterBegin",ui=tableOutput("fileContent"))
    values$flowData <- read.csv(input$flowFile$datapath,header=T)
  })

  observeEvent(input$loadExample, {
    removeUI(selector='#fileContent')
    removeUI(selector='#fitPlot')
    removeUI(selector='#downloadButtons')
    updateTextInput(session,'plotName',value="Example data")
    insertUI(selector='#mainPanel',where="afterBegin",ui=tableOutput("fileContent"))
    values$flowData <- data.frame(
		"Time"=c(0,2.5,5,7.5,10,17.5,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,42,44,46,48,50,52.5,55,57.5,60,65,70,90,120),
		"Mean"=c(109062,108992,108366,107211,109432,109101,107033,110182,107742,112929,107229,108659,110952,110403,117027,115246,115533,116406,114927,119756,118932,121096,122183,126516,127832,130947,136491,136876,142713,147730,153156,157113,160295,172801,169340,175852,178582,181765,186750,184419,184663)
		)
	})

  observeEvent(input$fitButton, {
	flow <- values$flowData
    if (!is.null(flow)) {
	counter$i <- counter$i+1
	a <- 100
	b <- 0
	precision <- 1
	tmp<-round(as.numeric(smooth.spline(flow[,2])$y),3)
	flow$stretch <- round((flow[,2]-min(tmp))*((a-b)/(max(tmp)-min(tmp)))+b,precision)
	flow$spline <- round((tmp-min(tmp))*((a-b)/(max(tmp)-min(tmp)))+b,precision)
	y<-flow$stretch
	x<-flow$Time
	boltzmann <- nls(y ~ a+(b-a)/(1+exp((x-c)/d)), start=list(c=25,d=1))
	boltzmannParams=coef(boltzmann)
	flow$Boltzmann<-round(fitted(boltzmann),precision)
	gompertz <- nls(y ~ a*exp(-c*exp(-d*x)), start=list(c=1,d=0.001))
	gompertzParams=coef(gompertz)
	flow$Gompertz<-round(fitted(gompertz),precision)
	values$flowData <- flow
	insertUI(selector='#fileContent',where="beforeBegin",ui=plotOutput("fitPlot", width = 950, height = 550))
	removeUI(selector='#fileContent')
	my.theme<-theme(
		plot.title = element_text(size=rel(2),hjust=0.5,face="bold"),
		plot.background = element_blank(),
		panel.grid.major = element_line(colour="grey",size=.2,linetype = "dashed"),
		panel.grid.minor = element_blank(),
		panel.border = element_rect(colour = "black", fill=NA, size=0.75),
		panel.background = element_blank(),
		axis.line = element_blank(),
		axis.title = element_text(family="Arial",size = rel(1.8)),
		axis.text = element_text(family="Arial",size = rel(1.5)),
		legend.position=c(0.05,0.95),
		legend.justification = c("left", "top"),
		legend.box.background = element_blank(),
		legend.title = element_text(family="Arial",size = rel(1.2),face="bold"),
		legend.text = element_text(family="Arial",size = rel(1.2)),
		legend.key = element_blank(),
		plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
	)
	title$theName <- input$plotName
	Boltzmann <- function(x) a +(b-a)/(1+exp((x-boltzmannParams[1])/boltzmannParams[2]))
	Gompertz <- function(x) a*exp(-gompertzParams[1]*exp(-gompertzParams[2]*x))
	plotObject<-ggplot(values$flowData,aes(x=Time))
	plotObject <- plotObject + geom_line(aes(color="spline",y=spline),linetype="dashed",size=0.75)
	plotObject <- plotObject + stat_function(fun=Boltzmann,mapping = aes(color = "Boltzmann"),size=0.75)
	plotObject <- plotObject + stat_function(fun=Gompertz,mapping = aes(color = "Gompertz"),size=0.75)
	plotObject <- plotObject + geom_point(aes(y=stretch))+ scale_y_continuous(breaks=seq(from=b,to=a,by=(a-b)/10))+scale_x_continuous(breaks=seq(from=0,to=max(values$flowData$Time),by=10),expand=c(0.03,0.03))+ggtitle(paste(title$theName))+my.theme
	plotObject <- plotObject + annotate("text", label = paste0(
			"a=",a," (max, constant)\n
			 b=",b," (min, constant)\n\n
			Boltzmann: a+(b-a)/(1+exp((x-c)/d))\n
			c=",round(boltzmannParams[1],digits=3),", d=",round(boltzmannParams[2],digits=3),
			", RSE: ",round(summary(boltzmann)$sigma,3),"\n\n
			Gompertz: a*exp(-c*exp(-d*x))\n
			c=",round(gompertzParams[1],digits=3),", d=",round(gompertzParams[2],digits=3),
			", RSE: ",round(summary(gompertz)$sigma,3)),
		x = max(values$flowData$Time)*0.98, y = b+((a-b)/25),size = 5,hjust=1,vjust=0,lineheight=0.6)
	plotObject <- plotObject + labs(y="Bulk genome replication, %",x="Time after release, min")
	plotObject <- plotObject + scale_color_manual(name = "Function:", values = c("blue","red","gray45"))
	thePlot$fitPlot <- plotObject
	if (counter$i > 1) {
		removeUI(selector='#downloadButtons',immediate=T)
	}
	insertUI(selector='#fitPlot',where="afterEnd",ui=tags$div(id = 'downloadButtons'))
	insertUI(selector='#downloadButtons',ui=downloadButton("downloadPlot","Download plot"))
	insertUI(selector='#downloadButtons',ui=downloadButton("downloadCSV","Download data"))

	output$downloadPlot <- downloadHandler(
	    filename = function() { paste0(title$theName, '.pdf') },
	    content = function(file) {
		        ggsave(file, plot=plotObject, device=cairo_pdf, width = 12, height = 6, units = "in")
	    }
	)
	output$downloadCSV <- downloadHandler(
	    filename = function() { paste0(title$theName, '.csv') },
	    content = function(file) {
		        write.table(flow,file=file,sep=",",col.names=T,row.names=F)
	    }
	)
    }
  })
  output$fileContent <- renderTable(values$flowData[1:7,])
  output$fitPlot <- renderPlot(thePlot$fitPlot)
}
