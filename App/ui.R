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
            #titlePanel("Strategy Settings"),
            #radioButtons("minimizationChoice", "Minimize", c("Deaths", "Cases")),
            
            #h3("Initial Populations"),
            numericInput("startingPop", "Total Population", 100000),
            numericInput("originalVirusInfections", "SARS-CoV-2 Current Infections", 2078),
            #unsure if i want to keep the variations
            #numericInput("UKVirusInfections", "SARS-CoV-2 variant B.1.1.7 (UK Strain)", 0),
            #numericInput("southAfricaVirusInfections", "SARS-CoV-2 variant B.1.351 (South African Strain)", 0),
            
            #h3("Vaccines Administered per Week"), #find out if you want per week or daily
            numericInput("genericVac", "Vaccinations Administered per Week", 203), #proportional to ontario's current numbers
            #numericInput("PfizerVac", "Pfizer", 0),
            #numericInput("ModernaVac", "Moderna", 0), 
            #add a conditional warning if specified vaccine doses will not cover the gen pop by Sept 30, 2021
            
            #h3("Strategy"),
            selectInput("strat", "Priority Vaccination", 
                        c("High Population Density", "Oldest to Youngest", "Youngest to Oldest", 
                          "Random")),
            radioButtons("doses", "Number of Doses", c("1", "2")),
            #numericInput("alreadyVac", "Number of People Already Vaccinated", 0),
            
            #h3("Simulation Length"),
            numericInput("iter", "Simulation Length (Iterations)", 26),
            
            actionButton("submit", "Start Simulation")
            
        ),

        # Show a plot of the generated distribution
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
