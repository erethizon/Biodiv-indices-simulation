#load packages
library(shiny)
library(bslib)
library(tidyverse)

#load data
beads <- read_csv("Data/one_bead_set_per_community_type.csv")

#source helper functions



# Define UI ----
ui <- page_sidebar(
  title = "Exploring biodiversity indices",
  sidebar = sidebar(
   card(
     card_header("Simulation type"),
     radioButtons(
       inputId = "sim_type",
       label = NULL,
       choices = c("simulate a set of beads", 
       "simulate a more natural community"),
     selected = "simulate a set of beads")
   ),
   card(
     card_header("Community structure"),
     radioButtons(
       inputId = "com_type", 
       label = NULL,
       choices = c("one dominant species",
                   "species evenly distributed",
                   "pseudorandom structure"), 
       selected = "one dominant species")
       ),
   card(
     card_header("Number of trials (max 10)"),
     sliderInput(
       "num_trials",
       NULL,
       min = 1,
       max = 10, 
       value = 5
     )
   ),
   card(
     card_header("Which index or indices will you use?"),
     checkboxGroupInput(
       "index_choice",
       "Select all that apply",
       choices = c("Species richness",
                      "Simpson's index",
                      "Shannon index")
     )
   )
   ),

  card(
    card_header("First card title"), 
    textOutput("selected_sim"),
    textOutput("selected_com"),
    textOutput("selected_trials"),
    textOutput("selected_index"),
    card_footer("Footer text here")
  )
  
)

# Define server logic ----
server <- function(input, output) {
  #switch to determine which part of data to use
 #  my_sim <- switch(input$sim_type, 
 #                   "simulate a set of beads" = beads, #use beads data set
 #                   "simulate a more natural community" = constructed_data)
 #  
 #  my_com <- switch(input$com_type,
 #                   "one dominant species" = 
 #                   "species evenly distributed",
 #                   "pseudorandom structure")
 #  
 #  ##need reactive expression to generate the data to use
 #  #beadsInput <- reactive({
 #    
 # # })
  
  output$selected_sim <- renderText({
    paste("You have selected to ", input$sim_type)
  })
  
  output$selected_com <- renderText({
    paste("You have selected ", input$com_type)
  })
  
  output$selected_trials <- renderText({
    paste0("You have chosen to complete ", input$num_trials, " trials")
  })
  
  output$selected_index <- renderText({
    print("You have chosen to compute:")  
    input$index_choice
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)