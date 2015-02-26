# ui.R

shinyUI(fluidPage(   
   titlePanel(survey_title),
   fluidRow(
      
      column(4, 
             selectInput("question",
                         label = "Choose a question",
                         choices = q_types$shortname, #defined in Data Prep
                         selected = q_types$shortname[1])
      ),  
      
      column(4,
             
             selectInput("segment", 
                         label = "Segment results by:",
                         choices = segments, #defined in Data Prep                         
                         selected = "none")
      )
      
      #       column(4, 
      #              checkboxGroupInput("series", 
      #                                 label = "Display data series for:",
      #                                 choices = c("Overall", "test1", "test1", "test3")
      #              )
      #       )
   ),
   
   hr(), 
   
   fluidRow(
      
      h4(textOutput("qtext")),
      
      tabsetPanel(
         tabPanel("Plot", plotOutput("qplot")),
         tabPanel("Table", tableOutput("qtable"))
      )
   )  
   
))