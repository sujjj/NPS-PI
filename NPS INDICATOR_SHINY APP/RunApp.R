library(shiny)
library(DT)
library(tidyverse)
library(RColorBrewer) 
library(mapview)



ui <- fluidPage( 
  
  theme="simplex.min.css",
  tags$style(types="text/css",
             "lable{font-size:12px}",
             ".recalculating{opacity :1.0;}"),
  
  
  titlePanel("NPS P indicator"),
  
  navlistPanel(widths=c(3,9),
    
    "Source Factors", 
    
    tabPanel("Pfer",
             h4("Spatial plot"),
             selectInput('Year1', 'Choose the year to display',Year,selected="2010"),
             plotOutput("Pfer.r",height=300, width=400)),
    
    
    tabPanel("Pman",
             h4("< P excretion coefficients"),
             DT::dataTableOutput("ecc",width=400,height=300),
             hr(),
             selectInput('Year2', 'Choose the year to display',Year,selected="2010"),
             hr(),
             
             h4("< Data: Rural population,livestock types and numbers"),
             DT::dataTableOutput("mytable1",width=400,height=150),
             hr(),
             h4("< Plot:Manure P application rates (kg/ha)"),
             plotOutput("Pman.r",height=300, width=400)),
    
    
    tabPanel("Soil P",
             h4("< OlsenP"),
             plotOutput("Psoi",height=300, width=400),
             hr(),
             h4("< TP"),
             plotOutput("TPsoi",height=300, width=400)),
    
    
    
    "Transport factors",
    
    tabPanel("Rainfall",
             verticalLayout(
               selectInput('Year3', 'Choose the year to display',Year,selected="2010"),
               hr(),
               plotOutput("Rain",height=300, width=400))), 
    
    tabPanel("Runoff",
             verticalLayout(
               selectInput('Year4', 'Choose the year to display',Year,selected="2010"),
               hr(),
               DT::dataTableOutput("mytable2",width=400,height=400))),
    

    "P indicator",
    
    tabPanel("NPS P indicator",
             
             wellPanel( 
               fluidRow(h4("< Parameters"),
                        column(4,
                               sliderInput("Conv.coef",label=h5("Soil OlsenP/Runoff DP conversion coefficient"),min=0.010,max=0.015,value=0.004),
                               sliderInput("DPDR",label=h5("DP delivery ratio"),min=0.5,max=1.0,value=0.5),
                               sliderInput("PDR",label=h5("P distribution ratio between pathways"),min=0.25,max=1,value=0.5),
                               sliderInput("Perc.WSP",label=h5("Water soluble P percentage"),min=0.3,max=0.95,value=0.6)),
                              
                        column(4,
                               sliderInput("Ploss.man",label=h5("Manure DP loss percentage"),min=0.05,max=0.85,value=0.5),
                               sliderInput("Ploss.fer",label=h5("Fertilizer DP loss percentage"),min=0.1,max=0.85,value=0.5),
                               sliderInput("RegER1",label=h5("Coefficient 1 for ER formula"),min=1.5,max=2.5,value=2),
                               sliderInput("RegER2",label=h5("Coefficient 2 for ER formula"),min=0.12,max=0.3,value=0.25)),
                        
                        column(4,
                               sliderInput("RegSDR1",label=h5("Coefficient 1 for ER formula"),min=0,max=0.25,value=0.15),
                               sliderInput("RegSDR2",label=h5("Coefficient 2 for ER formula"),min=0,max=0.25,value=0.15),
                               sliderInput("RegR1",label=h5("Coefficient 1 for R formula"),min=0.25,max=0.55,value=0.40),
                               sliderInput("RegR2",label=h5("Coefficient 2 for R formula"),min=1.19,max=1.82,value=0.55)))),
             
             hr(),
             wellPanel( 
               fluidRow(h4("< Choose the year and output variables to be displayed"),
                        column(5,
                               selectInput('Year5', 'Year',Year,selected="2010")),
                        column(5,offset=1,
                               selectInput('Index', 'Output variable',c("TP","TPP/TP","TDPsur/TDP","A"),selected="TP")))),
             

            hr(),
             wellPanel(
                h4("< Plot: Spatial distribution of P indicators"),
               leafletOutput("PI.r",width=875, height=600)),
              
             
             wellPanel(
               fluidRow(
                 h4("<Plot: Comparison between Simulated and Predicted"),
                 column(5,
                        plotOutput("A",height=300, width=450)),
                 column(5,offset=1,
                        plotOutput("P",height=300, width=450))))
               

    ))
  )



server <- function(input, output,session) {
  
  
  m <- reactive({input$Year1})
  m.fer <- reactive({f.Pfer(m())})
  output$Pfer.r <- renderPlot({
    plot.dig(m.fer())
  }) 
  
  
  output$ecc <- renderDataTable({ 
    DT::datatable(ecc.TP,options=list(paging=FALSE,searching=FALSE))})
  
  
  n <- reactive({input$Year2})
  lvstock <- reactive({subset(livestock, Year==n())})
  output$mytable1 <- renderDataTable({ 
    DT::datatable(lvstock(),options=list(paging=FALSE,searching=FALSE)) 
  }) 
  
  n.man <- reactive({f.Pman(n())})
  output$Pman.r <- renderPlot({
    plot.dig(n.man())
  })
  
  
  output$Psoi <- renderPlot({
    plot.dig1(Olsenp)})
  
  
  output$TPsoi <- renderPlot({
    plot.dig1(TPsoi)})
  
 
  p <- reactive({input$Year3})
  p.run<- reactive({f.Rain(p())})
  output$Rain <- renderPlot({
    plot.dig1(p.run())
  }) 
   
  a <- reactive({input$Year4})
  a.run <- reactive({f.runoff(a())})
  output$mytable2 <- renderDataTable({ 
    DT::datatable(a.run(),options=list(paging=FALSE,searching=FALSE))})
  
  

  param <- reactive({list(input$Conv.coef,input$DPDR,input$PDR,input$Perc.WSP,input$Ploss.man,input$Ploss.fer,input$RegER1,input$RegER2,input$RegSDR1,input$RegSDR2,input$RegR1,input$RegR2)})
  b <- reactive({input$Year5})
  b.pi <- reactive(do.call(f.Pind,list(b(),param())))
  b.sum <- reactive({sapply(b.pi(),FUN=f.summary)})
  
  
  output$PI.r <- renderLeaflet({
    if(input$Index=="TP"){
      f.leaf( b.pi()[[1]])
    } else if (input$Index=="TPP/TP"){
      f.leaf(b.pi()[[2]])
    }
    else if (input$Index=="TDPsur/TDP"){f.leaf(b.pi()[[3]])}
    else {f.leaf(b.pi()[[4]])}
  }) 
  

  
  output$A <- renderPlot({
    f.comp(obs$A.obs,"ton/ha","Erosion Rates",5,b(),b.sum()[4])
  })
  
  
  output$P <- renderPlot({
    f.comp(obs$P.obs,"kg/ha","NPS P loss",0.3,b(),b.sum()[1])
  })
  
}



shinyApp(ui,server)

