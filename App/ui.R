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
            titlePanel("Strategy Settings"),
            radioButtons("minimizationChoice", "Minimize", c("Deaths", "Cases")),
            
            h3("Initial Populations"),
            numericInput("startingPop", "Total Population", 10000),
            numericInput("originalVirusInfections", "SARS-CoV-2", 500),
            #unsure if i want to keep the variations
            #numericInput("UKVirusInfections", "SARS-CoV-2 variant B.1.1.7 (UK Strain)", 0),
            #numericInput("southAfricaVirusInfections", "SARS-CoV-2 variant B.1.351 (South African Strain)", 0),
            
            h3("Vaccines Administered per Week"), #find out if you want per week or daily
            numericInput("genericVac", "Vaccinations", 0),
            #numericInput("PfizerVac", "Pfizer", 0),
            #numericInput("ModernaVac", "Moderna", 0), 
            #add a conditional warning if specified vaccine doses will not cover the gen pop by Sept 30, 2021
            
            h3("Strategy"),
            selectInput("strat", "Priority Vaccination", 
                        c("High Population Density", "Oldest to Youngest", "Youngest to Oldest", 
                          "Random")),
            radioButtons("doses", "Number of Doses", c("1", "2")),
            #numericInput("alreadyVac", "Number of People Already Vaccinated", 0),
            
            h3("Simulation Length"),
            numericInput("iter", "Iterations", 5),
            
            actionButton("submit", "Start Simulation")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
                tabsetPanel(id = "tabs",tabPanel("Projected Cases", verticalLayout(plotOutput("activeCasesPlot"), plotOutput("newCasesPlot"), plotlyOutput("cumulativeCasesPlot")), value = 1),
                        tabPanel("Projected Deaths", textOutput("deathText"), value = 2),
                        tabPanel("Vaccinated Individuals", textOutput("vaccText"), value = 3),
                        tabPanel("Visualization", textOutput("mapsText"), value = 4),
                        tabPanel("Data Manipulation and Limitations", htmlOutput("dataLimText"), value = 5),
                        tabPanel("Projected New Cases 2", tableOutput("vaccMatrix"), value = 6),
                        tabPanel("Author", textOutput("authorText"), value = 7)
                )
            
        )
    )
))
