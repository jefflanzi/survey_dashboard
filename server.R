#server.R
library(shiny)

#server function
shinyServer(
  function(input, output) {
     
     #retrive qids from descriptive shortnames displayed in dropdowns
     segment <- reactive({
         if(input$segment == "none") {return("overall")}         
         q_types[q_types$shortname == input$segment, "qid"]
      })
      
      qid <- reactive({
         q_types[q_types$shortname == input$question, "qid"]
      })

     #generate outputs
     output$qtext <- renderText({        
        as.character(qoverview[qoverview$name == qid(), "text"])
     })
     
     output$qtable <- renderTable({
        svy_table(data, qid(), segment())
     })
     
     output$qplot <- renderPlot({
        svy_plot(data, qid(), segment())       
     })
     
  }
)