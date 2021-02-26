#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)
library(plotly)
library(tidyverse)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Ontario COVID-19 Vaccination Strategy"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            numericInput("originalVirusInfections", "Percentage of Population Currently Infected with COVID-19", 0.112),

            strong("Vaccines To Be Admistered"),
            strong("(per week per 1M people)"),
            h5(""),
            numericInput("genericVac", h5("Pfizer"), 0), #proportional to ontario's current numbers
            numericInput("modernaVac", h5("Moderna"), 0),

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
                        tabPanel("Projected Cases", verticalLayout(htmlOutput("casesText"),plotlyOutput("activeCasesPlot"), plotlyOutput("newCasesPlot"), plotlyOutput("cumulativeCasesPlot")), value = 1),
                        tabPanel("Projected Deaths", verticalLayout(htmlOutput("deathsText"), plotlyOutput("newDeathsPlot"), plotlyOutput("cumulativeDeathsPlot")), value = 2),
                        tabPanel("Vaccinated Individuals", verticalLayout(plotlyOutput("vaccinationPlot")), value = 3),
                        tabPanel("Population Breakdowns", tableOutput("populationTable"), value = 6),
                        tabPanel("About the Dashboard", htmlOutput("introduction"), value = 8),
                        tabPanel("Methods and Limitations", htmlOutput("manipulation"), value = 5),
                        tabPanel("About the Author",source("about.R")$value(), value = 7)
                )
            
        )
    )
))
