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
            
            h3("Initial Active Cases"),
            numericInput("originalVirusInfections", "SARS-CoV-2 (Original Strain)", 0),
            #unsure if i want to keep the variations
            numericInput("UKVirusInfections", "SARS-CoV-2 variant B.1.1.7 (UK Strain)", 0),
            numericInput("southAfricaVirusInfections", "SARS-CoV-2 variant B.1.351 (South African Strain)", 0),
            
            h3("Vaccines Administered per Unit Time"), #find out if you want per week or daily
            numericInput("PfizerVac", "Pfizer", 0),
            numericInput("ModernaVac", "Moderna", 0), 
            #add a conditional warning if specified vaccine doses will not cover the gen pop by Sept 30, 2021
            
            h3("Strategy"),
            selectInput("strat", "Priority Vaccination", 
                        c("Older Generations", "Frontline Workers", "Marginalized Populations", 
                          "Highest Population Density", "Random")),
            radioButtons("doses", "Number of Doses", c("1", "2")),
            numericInput("alreadyVac", "Number of People Already Vaccinated", 0),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(tabPanel("Projected New Cases", htmlOutput("casesText")),
                        tabPanel("Projected Deaths", textOutput("deathText")),
                        tabPanel("Vaccinated Individuals", textOutput("vaccText")),
                        tabPanel("Visualization", textOutput("mapsText")),
                        tabPanel("Data Manipulation and Limitations", htmlOutput("dataLimText")), 
                        tabPanel("Author", textOutput("authorText")))
        )
    )
))
