#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$casesText <- renderUI({HTML(paste("Here will be a time series plot of new cases 
                                   from vaccination start to Sept 30, 2021. There 
                                   will also be a cumulative cases plot and some 
                                   text detailing the number of new cases from beginning to Sept 30, 2021.",
                   "Sidebar Note: Unsure if there is enough info about the variation strains to include them",sep ="<br/>"))})
    output$deathText <- renderText("This tab is the exact same as the Projected Cases tab except with deaths.")
    output$vaccText <- renderText("Another time series plot along with a table of fully and partially vaccinated 
                                  individuals. One plot details 2 dose individuals and the partial is 1 dose individuals.")
    output$mapsText <- renderText("Map visualizations")
    output$dataLimText <- renderUI({HTML(paste("Any and all data manipulation, limitations, and assumptions are here. Data credit will also be provided.",
                                     "Current Data Credit: variation names from CNN https://www.cnn.com/2021/02/02/health/variant-mutation-e484k/index.html.",
                                     "Current known limitations:",
                                     "0-15 yr olds cannot be vaccinated by either vaccine 
                                     and 16-17 yr olds can only be vaccinated with Pfizer. Census population data groups 15 
                                     yr olds with 16-19 yr olds and that data must be divided appropriately.
                                     Governmnet organizations never specify who the 'general population' is, 
                                     current solution: subtract healthcare workforce from total adult population.", sep = "<br/>"))})
    output$authorText <- renderText("Little blurb about me with photo and links to linkedin and github")
    
    
    
    
    
})
