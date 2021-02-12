#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 Ontario Vaccination Strategy"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            numericInput("startingPop", "Total Population", 100000),
            numericInput("originalVirusInfections", "SARS-CoV-2 Current Infections", 2078),
            
            numericInput("genericVac", "Vaccinations Administered per Week", 203), #proportional to ontario's current numbers
            
            selectInput("strat", "Priority Vaccination", 
                        c("High Population Density", "Oldest to Youngest", "Youngest to Oldest", 
                          "Random")),
            radioButtons("doses", "Number of Doses", c("1", "2")),
         
            numericInput("iter", "Simulation Length (Iterations)", 26),
            
            actionButton("submit", "Start Simulation")
            
        ),

        mainPanel(
            
                tabsetPanel(id = "tabs",tabPanel("Projected Cases", verticalLayout(plotlyOutput("activeCasesPlot"), plotlyOutput("newCasesPlot"), plotlyOutput("cumulativeCasesPlot")), value = 1),
                        tabPanel("Projected Deaths", verticalLayout(plotlyOutput("newDeathsPlot"), plotlyOutput("cumulativeDeathsPlot")), value = 2),
                        tabPanel("Vaccinated Individuals", verticalLayout(plotlyOutput("vaccinationPlot")), value = 3),
                        tabPanel("Strategy Effectiveness", tableOutput("vaccMatrix"), value = 4),
                        tabPanel("Population Breakdowns", tableOutput("populationTable"), value = 6),
                        tabPanel("Data Manipulation and Limitations", htmlOutput("dataLimText"), value = 5),
                        tabPanel("Author",source("about.R")$value(), value = 7)
                )
            
        )
    )
))
