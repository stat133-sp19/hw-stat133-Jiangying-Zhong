#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Finanical Productions"),
  
  fluidRow(
    column(3,
           sliderInput("Initial",
                       "Initial Amount:",
                       min = 0,
                       max = 100000,
                       value = 1000,
                       step = 500,
                       pre = "$"),
           sliderInput("Annual",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500,
                       pre = "$")
    ),
    
    column(3,
           sliderInput("Return",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = 0.1),
           sliderInput("Growth",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2)
    ),
    
    column(3,
           sliderInput("Years",
                       "Years",
                       min = 0,
                       max = 50,
                       value = 10,
                       step = 1),
           selectInput("Facet",
                       "Facet?",
                       c("YES","NO"),
                       selected = "NO")
    )
  ),
  
  mainPanel(
    plotOutput("Timelines"),
    tableOutput("Balances")
  )
  
)

# Define server logic required to draw a histogram
server <- function(input,output){
  
  dat <- reactive({
    future_value <- function(amount=0, rate=0, years=0) {
      return(amount*(1+rate)**years)
    }
    annuity <- function(contrib=0, rate=0, years=0) {
      return(contrib*((1+rate)**years-1)/rate)
    }
    growing_annuity <- function(contrib=0, rate=0, growth=0,years=0) {
      return(contrib*((1+rate)**years-(1+growth)**years)/(rate-growth))
    }
    
    amount <- input$Initial
    contrib <- input$Annual
    return_rate <- input$Return / 100
    growth_rate <- input$Growth / 100
    
    year <- c(0:input$Years)
    no_contrib <- c(amount)
    fixed_contrib <- c(amount)
    growing_contrib <- c(amount)
    type <- rep(c("no_contrib","fixed_contrib","growing_contrib"),each=input$Years+1)
    
    for(k in 1:input$Years){
      no_contrib[k+1] <- future_value(amount=amount,rate=return_rate,years=k)
      fixed_contrib[k+1] <- annuity(contrib=contrib,rate=return_rate,years=k)+no_contrib[k+1]
      growing_contrib[k+1] <- growing_annuity(contrib=contrib,rate=return_rate,growth=growth_rate,years=k)+no_contrib[k+1]
    }
    
    dat <- data.frame(years=year,no_contribute=no_contrib,fixed_contribute=fixed_contrib,growing_contribute=growing_contrib)
    
    return(dat)
  })
  
  output$Timelines <- renderPlot({
    type <- rep(c("no_contrib","fixed_contrib","growing_contrib"),each=input$Years+1)
    modalities <- data.frame(year=dat()$year,money=c(dat()$no_contrib,dat()$fixed_contrib,dat()$growing_contrib),type)
    colnames(modalities) <- c("year","money","type")

    graph <- ggplot(modalities)+
      geom_line(aes(x=year,y=money,color=type))+
      geom_point(aes(x=year,y=money,color=type))+
      labs(title = "Three modes of investing")
    
    if(input$Facet == "YES"){
      graph <- graph + 
        facet_wrap(~type) +
        geom_area(aes(x=year,y=money,fill=type,alpha=0.5))
    }
    show(graph)
  })
  
  output$Balances <- renderTable({
    dat()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


