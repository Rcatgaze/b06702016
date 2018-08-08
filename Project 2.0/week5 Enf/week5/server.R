library(shiny)
library(ggplot2)

dta <- read.csv(file = "data/MLB2008 .csv",
                header = TRUE)
dta$FR[dta$G <= 163 & dta$G >= 120] <- 'high'
dta$FR[dta$G < 120 & dta$G >= 60] <- 'mid'
dta$FR[dta$G < 60] <- 'low'

dta$PAY[dta$SALARY <= 5000000] <- 'low'
dta$PAY[dta$SALARY <= 10000000 & dta$SALARY > 5000000] <- 'mid'
dta$PAY[dta$SALARY <= 20000000 & dta$SALARY > 10000000] <- 'high'
dta$PAY[dta$SALARY > 20000000] <- 'great'

dta$POS <- as.factor(dta$POS)
dta$FR <- as.factor(dta$FR)
dta$PAY <- as.factor(dta$PAY)
dta$PAY <- factor(dta$PAY, levels = c("low", "mid", "high", "great"))
  
choice.type <-
  c('FR', 'POS', 'PAY')
choice.value <-
  c(
    'AVG',
    'OBP',
    'SLG'
  )

function(input, output, session) {
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