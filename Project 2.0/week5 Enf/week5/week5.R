library(shiny)
library(ggplot2)

dta <- read.csv(file = "data/MLB2008 .csv",
                header = TRUE)
dta$POS <- as.factor(dta$POS)
dta$FR[dta$G <= 163 & dta$G >= 120] <- 'high'
dta$FR[dta$G < 120 & dta$G >= 60] <- 'mid'
dta$FR[dta$G < 60] <- 'low'

dta$PAY[dta$SALARY <= 5000000] <- 'low'
dta$PAY[dta$SALARY <= 10000000 & dta$SALARY > 5000000] <- 'mid'
dta$PAY[dta$SALARY <= 20000000 & dta$SALARY > 10000000] <- 'high'
dta$PAY[dta$SALARY > 20000000] <- 'great'

dta$POS <- as.factor(dta$POS)
dta$PAY <- as.factor(dta$PAY)
dta$PAY <- factor(dta$PAY, levels = c("low", "mid", "high", "great"))
dta$FR <- as.factor(dta$FR)
dta$SALARY1 <- as.numeric(dta$SALARY)

choice.type <-
  c('FR', 'POS', 'PAY')
choice.value <-
  c(
    'AVG',
    'OBP',
    'SLG'
  )

# UI
ui <- navbarPage(
  "Shiny Example",
  tabPanel(
    "Introduction",
    tags$h1("This is an example for making a shiny app."),
    tags$p("Let's use the dataset same with week4 task example.")
  ),
  tabPanel(
    "Raw Data",
    tags$h1("Let's take a look at the dataset."),
    br(),
    fluidRow(column(
      8,
      tabPanel("Table",
               DT::dataTableOutput("data.raw"))
    ))
  ),
  
  tabPanel(
    "Single Variable",
    tags$h1("Summrizing time!"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),
    
    tags$h1("Summary"),
    verbatimTextOutput("summary")
    
  ),
  
  tabPanel(
    "PartA.",
    tags$h1("Box Plot"),
    sidebarLayout(
      sidebarPanel(
        selectInput('PA.type', 'type', choice.type, selectize = TRUE),
        selectInput('PA.value', 'Value', choice.value, selectize =
                      TRUE)
      ),
      mainPanel(plotOutput("PA.plot"))
    ),
    h1("T Test / ANOVA"),
    verbatimTextOutput("t.test.anova")
  ),
  #NEW
  tabPanel(
    "Plot",
    tags$h1("Lines"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('LN.input', 'value', c(choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("LN.plot"))
    )
  ),
  # tabPanel("Summary"),
  
  navbarMenu("More",
             plotOutput("plot"))
)

# server
server <- function(input, output, session) {
  output$SV.plot <- renderPlot({
    if( is.element(input$SV.input, choice.type) ){
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_bar() +
        labs(y = "count", x = input$SV.input)
    }
    else{
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_histogram() +
        labs(y = "count", x = input$SV.input)
    }
  })
  
  output$PA.plot <- renderPlot({
    ggplot(data = dta, aes_string(x = input$PA.type, y = input$PA.value)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.type)
    
  })
  
  output$summary <- renderPrint({
    summary(dta)
  })
  
  #NEW
  output$LN.plot <- renderPlot({
    ggplot(data = dta, aes_string(group = dta$PAY, x = dta$SALARY, y = input$LN.input)) +
      geom_point() + geom_smooth(method = lm) +
      labs(y = input$LN.input, x = "SALARY")
    
  })
  
  output$data.raw <- DT::renderDataTable({
    DT::datatable(dta)
  })
  
  output$data.summary <- DT::renderDataTable({
    DT::datatable(summary(dta))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

