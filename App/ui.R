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



shinyUI(fluidPage(

    
    titlePanel("Ontario COVID-19 Vaccination Strategy"),

    
    sidebarLayout(
        sidebarPanel(

            numericInput("originalVirusInfections", "Percentage of Population Currently Infected with COVID-19", 0.112),

            strong("New Individuals To Be Vaccinated"),
            strong("(per week per 1M people)"),
            h5(""),
            numericInput("genericVac", h5("Pfizer Vaccine"), 0), 
            numericInput("modernaVac", h5("Moderna Vaccine"), 0),

            selectInput("strat", "Vaccination Priority",
                        c("Random", "High Population Density", "Oldest to Youngest", "Youngest to Oldest")),
            radioButtons("doses", "Number of Doses", c("1", "2")),
            useShinyalert(),
            actionButton("submit", "Start Simulation")

        ),

        mainPanel(
                tabsetPanel(id = "tabs",
                        tabPanel("Strategy Effectiveness", tableOutput("vaccMatrix"), value = 4),
                        tabPanel("Projected Cases", verticalLayout(htmlOutput("casesText"),plotlyOutput("activeCasesPlot"), plotlyOutput("newCasesPlot"), plotlyOutput("cumulativeCasesPlot")), value = 1),
                        tabPanel("Projected Deaths", verticalLayout(htmlOutput("deathsText"), plotlyOutput("newDeathsPlot"), plotlyOutput("cumulativeDeathsPlot")), value = 2),
                        tabPanel("Vaccinated Individuals", verticalLayout(plotlyOutput("vaccinationPlot")), value = 3),
                        tabPanel("Population Breakdown", tableOutput("populationTable"), value = 6),
                        tabPanel("About the Dashboard", htmlOutput("introduction"), value = 8),
                        tabPanel("Methods and Limitations", htmlOutput("manipulation"), value = 5),
                        tabPanel("About the Author",source("about.R")$value(), value = 7)
                )
            
        )
    )
))
