#Library Loading
library(shiny)
library(tidyverse)
library(cancensus)


shinyServer(function(input, output) {
    
    #################### Accessing Data and Cleaning
    
    ### Accessing Ontario census data 
    options(cancensus.api_key='CensusMapper_d368730679bb25c2becf6fae8a73d5db')
    options(cancensus.cache_path = "~/uni/4th year/stat5702/Census Data")
    
    census_data <- get_census(dataset='CA16', regions=list(PR="35"), 
                              vectors=c("v_CA16_401","v_CA16_4", "v_CA16_64","v_CA16_82",
                                        "v_CA16_100","v_CA16_118","v_CA16_136",
                                        "v_CA16_154","v_CA16_172","v_CA16_190",
                                        "v_CA16_208","v_CA16_226","v_CA16_247",
                                        "v_CA16_265","v_CA16_283","v_CA16_301",
                                        "v_CA16_322","v_CA16_340","v_CA16_358",
                                        "v_CA16_376"), labels="detailed", 
                              geo_format=NA, level='Regions')
    
    ### Cleaning Census Data
    populationData = census_data %>% rename(Age0to14 = `v_CA16_4: 0 to 14 years`, Age15to19 = `v_CA16_64: 15 to 19 years`,
                                            Age20to24 = `v_CA16_82: 20 to 24 years`, Age25to29 = `v_CA16_100: 25 to 29 years`, 
                                            Age30to34 = `v_CA16_118: 30 to 34 years`, Age35to39 = `v_CA16_136: 35 to 39 years`, 
                                            Age40to44 = `v_CA16_154: 40 to 44 years`, Age45to49 = `v_CA16_172: 45 to 49 years`, 
                                            Age50to54 = `v_CA16_190: 50 to 54 years`, Age55to59 = `v_CA16_208: 55 to 59 years`, 
                                            Age60to64 = `v_CA16_226: 60 to 64 years`, Age65to69 = `v_CA16_247: 65 to 69 years`, 
                                            Age70to74 = `v_CA16_265: 70 to 74 years`, Age75to79 = `v_CA16_283: 75 to 79 years`, 
                                            Age80to84 = `v_CA16_301: 80 to 84 years`, Age85to89 = `v_CA16_322: 85 to 89 years`, 
                                            Age90to94 = `v_CA16_340: 90 to 94 years`, Age95to99 = `v_CA16_358: 95 to 99 years`,
                                            Age100s = `v_CA16_376: 100 years and over`, LandAreaSqKM = `Area (sq km)`) %>%
        mutate(eligiblePopulation = Population - Age0to14 - (0.2*Age15to19), Age16to19 = 0.8*Age15to19,
               Age20s = Age20to24+Age25to29, Age30s = Age30to34+Age35to39, Age40s = Age40to44+Age45to49, 
               Age50s = Age50to54+Age55to59, Age60s = Age60to64+Age65to69, Age70s = Age70to74+Age75to79,
               Age80s = Age80to84+Age85to89, Age90s = Age90to94+Age95to99) %>%
        select(Population,  eligiblePopulation, Age16to19, Age20s, Age30s, Age40s, Age50s, Age60s, Age70s, Age80s, Age90s, Age100s)
    
    
    #### Setting User Inputs
    startingPop = reactive({input$startingPop})
    startingCases = reactive({input$originalVirusInfections})
    time = reactive({input$iter})
    
    priorityType <- reactive({
        if(input$strat == "High Population Density"){
            1
        }
        else if(input$strat == "Oldest to Youngest"){
            2
        }
        else if(input$strat == "Youngest to Oldest"){
            3
        }
        else if(input$strat == "Random"){
            0
        }
    })
    # 
    observe({print(priorityType())})
    doses = reactive({as.numeric(input$doses)})
    vaccPerTime = reactive({input$genericVac})
    
    
    ### Setting constants
    caseProbability = 279472/populationData$Population #ontario case data as of Monday Feb 8th 2021
    deathProbability = 6538/279472 #ontario death data as of Monday Feb 8th
    
    set.seed(29)
    populationMatrix = reactive({
        tibble("PersonID" = NA, "Vaccinated" = NA, "Status" = NA, "PopDensity" = NA, "Age" = NA, "LTC" = NA, "VaccineType" = NA, "Suseptibility" = NA, .rows = startingPop()) %>%
            mutate(PersonID = c(1:startingPop()), Vaccinated = 0, Status = 0, PopDensity = rbinom(startingPop(), 1, 0.15), Age = round(runif(startingPop(), min = 1, max = 10)), LTC = 0, VaccineType = 0, Suseptibility = 1)})
    output$popMatrix <- renderTable(populationMatrix())
    ### Vaccinated - 0 none
    ###             1 one dose
    ###             2 one dose
    ###
    ### Status -    0 alive and well
    ###             1 alive but sick and first week infectious
    ###             2 alive and second week sick
    ###             3 dead
    ###
    ### Pop Density -  1 over high pop dens
    ###                0 not
    ###
    ### Age - first digit of age (1 - teens, 2 - 20s etc)
    ###
    ### LTC - indicator for LTC homes - 1 true/0 false
    ###
    ### Vaccine Type - 0 unvacc
    ###               1-4 pfizer (1, just vacc, 2 vacc last week, 3 vacc two weeks ago, 4 third weeks ago and now fully vaccinated)
    ###               5-9 moderna (5, just vacc, 6 vacc last week, 7 vacc two weeks ago, 8 third weeks ago, 9 four weeks ago and now fully vaccinated)
    ### Suseptiblilty- 0 not vacc
    ###               0.48, one dose of pfizer (1 dose has 52% efficacy)
    ###               0.198, one dose of moderna (1 does has 80.2% efficacy)
    ###               0.05, two dose pfizer (95% eff)
    ###               0.059, two dose moderna (95% eff)
    
    
    #### Plotting Tables
    vaccPop = tibble("newVaccinatedPop" = NA, "cumulativeVaccPop" = NA, "time"= NA) %>% remove_missing(na.rm = TRUE)
    covidCases = tibble("newCases" = NA, "currentCases" = NA, "cumulativeCases" = NA, "time" = NA)%>% remove_missing(na.rm = TRUE)
    covidDeaths = tibble("newDeaths" = NA, "cumulativeDeaths" = NA, "time" = NA)%>% remove_missing(na.rm = TRUE)
    

    
    ### Establishing vaccination order 
    vaccinationOrderMatrix <- reactive({
        if(priorityType() == 0){ #random
            populationMatrix() %>% filter(Vaccinated == 0) %>% 
                mutate(VaccOrder = sample(PersonID, startingPop())) %>% 
                arrange(VaccOrder)
        }
    
        else if(priorityType() == 1){ #pop density
            populationMatrix() %>% filter(Vaccinated == 0) %>% 
                arrange(desc(PopDensity)) %>% 
                mutate(vaccOrder = c(1:startingPop()))
        }
    
        else if(priorityType() == 2){ #oldest first
            populationMatrix %>% filter(Vaccinated == 0) %>% 
                arrange(desc(Age)) %>% 
                mutate(vaccOrder = c(1:startingPop()))
        }
    
        else if(priorityType() == 3){ #youngest first
            populationMatrix %>% filter(Vaccinated == 0) %>% 
                arrange(Age) %>% 
                mutate(vaccOrder = c(1:startingPop()))
        }
    })
    
    
    output$vaccMatrix <- renderTable(vaccinationOrderMatrix())
    
    
    
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
