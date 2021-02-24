#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 Ontario Vaccination Strategy"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            numericInput("originalVirusInfections", "SARS-CoV-2 Initial Infections Percentage", 0.02078),
            
            numericInput("genericVac", "Pfizer Vaccines Administered per Week", 4628), #proportional to ontario's current numbers
            numericInput("modernaVac", "Moderna Vaccines Administered per Week", 1540),
            
            selectInput("strat", "Priority Vaccination", 
                        c("Random", "High Population Density", "Oldest to Youngest", "Youngest to Oldest")),
            radioButtons("doses", "Number of Doses", c("1", "2")),
            useShinyalert(),
            actionButton("submit", "Start Simulation")
            
        ),

        mainPanel(
                        ## note on the first tab - it shows an error before showing up, just a byrpoduct of the hiding/showing of tabs
                        ## give it a few seconds to show up
                tabsetPanel(id = "tabs",
                        tabPanel("Strategy Effectiveness", tableOutput("vaccMatrix"), value = 4),
                        tabPanel("Projected Cases", verticalLayout(plotlyOutput("activeCasesPlot"), plotlyOutput("newCasesPlot"), plotlyOutput("cumulativeCasesPlot")), value = 1),
                        tabPanel("Projected Deaths", verticalLayout(plotlyOutput("newDeathsPlot"), plotlyOutput("cumulativeDeathsPlot")), value = 2),
                        tabPanel("Vaccinated Individuals", verticalLayout(plotlyOutput("vaccinationPlot")), value = 3),
                        tabPanel("Population Breakdowns", tableOutput("populationTable"), value = 6),
                        tabPanel("Data Manipulation and Limitations", htmlOutput("dataLimText"), value = 5),
                        tabPanel("Author",source("about.R")$value(), value = 7)
                )
            
        )
    )
))
