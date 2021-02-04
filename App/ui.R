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
    titlePanel("COVID-19 Canadian Vaccination Strategy"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            titlePanel("Strategy Settings"),
            radioButtons("minimizationChoice", "Minimize", c("Deaths", "Cases")),
            
            h3("Initial Active Cases"),
            numericInput("originalVirusInfections", "Original Strain", 0),
            numericInput("UKVirusInfections", "UK Strain", 0),
            numericInput("southAfricaVirusInfections", "South African Strain", 0),
            numericInput("BrazilVirusInfections", "Brazilian Strain", 0),
            
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
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(tabPanel("Tab 1", plotOutput("distPlot")))
                        #tabPanel("Tab 2", plotOutput("distPlot")),
                        #tabPanel("Tab 3", plotOutput("distPlot")),
                        #tabPanel("Tab 4", plotOutput("distPlot")),
                        #tabPanel("Tab 5", plotOutput("distPlot"))),
        )
    )
))
