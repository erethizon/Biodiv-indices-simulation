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

## make very basic version that just outputs a processed data set as a table.

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
              selected = "One dominant species")
          ),
        ),
        
                ##main output
      
                ##card to show results
                card(
                 tableOutput("results_table")
                 #plotOutput("results_figure")
                  
                )
             )
        )
  )
)


# Define server logic ----
server <- function(input, output) {
  
  ##render the plot for the output
    #start by processing data
  processed_data <- reactive({
    req(input$com_type)
    
    #now filter
    beads |>
      filter(Treatment == input$com_type)
    
    
  })
  
  
    ##now make plot
    # output$results_figure <- renderPlot(
    # 
    #   ggplot(processed_data(), aes(x = Num_beads))+
    #     geom_histogram()+
    #     theme_bw()
    # 
    #                )
    
    ##now make table
    output$results_table <- renderTable(
     processed_data()
    )
    
}

# Run the app ----
shinyApp(ui = ui, server = server)