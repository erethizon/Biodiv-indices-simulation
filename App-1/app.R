#load packages
library(shiny)
library(bslib)
library(tidyverse)

##copy to console to do error checking
#options(shiny.error = browser)
#options(shiny.reactlog = TRUE)
#options(shiny.autoload.r = FALSE)


#load data
beads <- read_csv("Data/one_bead_set_per_community_type.csv")

#source helper functions


# Define UI ----
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "cosmo"
  ),
  ##app title ---
  
  ##create tabs ---
  
  navset_card_tab(
    #assign id to track active tab from server
    id = "card_tabs",
    
    ##first tab ---
    nav_panel("Simulation with sets of beads",
  
             layout_sidebar(
                sidebar = sidebar("Parameters",
                                  
                                    card(
                                      card_header("Community structure"),
                                      radioButtons(
                                        inputId = "com_type",
                                        label = NULL,
                                        choices = as.list(unique(beads$Treatment)),
                                        selected = "one dominant species")
                                    ),
                                    
                                    card(
                                      card_header("Number of trials (max 10)"),
                                      sliderInput(
                                        "num_trials",
                                        NULL,
                                        min = 1,
                                        max = 10,
                                        value = 5)
                                    ),
                                    
                                    card(
                                      card_header("Which index or indices will you use?"),
                                      checkboxGroupInput(
                                        "index_choice",
                                        "Select all that apply",
                                        choices = c("Species richness",
                                                    "Simpson's index",
                                                    "Shannon index"), 
                                        selected = "Species richness", 
                                        inline = FALSE
                                      )
                                    )
                                    
                                                
                                    
                                    ),
                
                ##main output
                ##first card to show parameter selections
                card(
                  fill = TRUE,
                  height = "100px",
                  card_header("Your chosen settings"),
                  #textOutput("selected_sim"), turn on if you keep whole app on one tab
                  textOutput("selected_com"),
                  textOutput("selected_trials"),
                  textOutput("choice_text"),
                  textOutput("selected_index")
                  
                ),
                
                ##second card to show results
                card(
                  card_header("Results"),
                 plotOutput("results_figure"),
                  
                  card_footer("Simulation results")
                )
             )
             
        )
    
    ##second tab
    # nav_panel("Simulation with more realistic data"
    #   
    # )
  )
)



# Define server logic ----
server <- function(input, output) {
  
  ##report list of parameter choices
  output$selected_sim <- renderText({
    paste("You have selected to ", input$sim_type)
  })
  
  output$selected_com <- renderText({
    paste("You have selected ", input$com_type)
  })
  
  output$selected_trials <- renderText({
    paste0("You have chosen to complete ", input$num_trials, " trials")
  })
  
  #report selected indices
  
  selected_indices <- reactive({input$index_choice})
  
  output$choice_text <- renderText({
   "You have chosen to compute:"
   })
  
  output$selected_index <- renderText({
    selected_indices <- input$index_choice
    paste(selected_indices, collapse = ", ")
  })
  
  
  ##now render the plot for the output
    #start by processing data
  processed_data <- reactive({
    req(input$com_type)
    
    #now filter
    beads_filtered <- beads |>
      filter(Treatment == input$com_type)
    
  })
  
  
    ##now make plot
    output$results_figure <- renderPlot(
      
      
      
      ggplot(processed_data(), aes(x = Num_beads))+
        geom_histogram()+
        theme_bw()
    
                   )
    
    
    
    
    
    
    
    
    
    
    
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)