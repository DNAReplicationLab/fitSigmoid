library(shiny)
library(ggplot2)
fluidPage(theme="fitSigmoid.css",
  headerPanel("fitSigmoid: parametric and non-parametric fitting of flow cytometry data.", windowTitle="fitSigmoid"),
  sidebarLayout(
    sidebarPanel(
	p('The csv file must contain "Time" column and data (mean or median) column',style="color:gray"),
	fileInput("flowFile", "Load flow data:",multiple=F,accept=".csv",buttonLabel = "Browse...", placeholder="No file selected",width="75%"),
	actionButton("loadExample","Use example data"),
	HTML("<hr>"),
	textInput("plotName", "Plot name:",value="",width="75%"),
	actionButton("fitButton","Fit sigmoid"),
	width=3
    ),
      mainPanel(
		HTML("<div id='mainPanel'></div>"),
		width=9
    )
  ),
  HTML("<div class='footer'><span style='height:50px;line-height:50px;vertical-align:middle;'>Nieduszynski lab</span></div>")
)
