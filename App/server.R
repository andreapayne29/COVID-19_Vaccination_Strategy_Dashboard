#Library Loading
library(shiny)
library(tidyverse)
library(cancensus)
library(plotly)


shinyServer(function(input, output) {
    
    hideTab(inputId = "tabs", target = "1")
    hideTab("tabs", "2")
    hideTab("tabs", "3")
    hideTab("tabs", "4")
    hideTab("tabs", "6")
    

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
    
    
    
    values = reactiveValues()
    
    observeEvent(input$submit, {
       
        
        
        #### Setting User Inputs
        startingPop = input$startingPop
        startingCases = input$originalVirusInfections
        time = input$iter 
        doses = as.numeric(input$doses)
        vaccPerTime = input$genericVac
    
    
        ### Setting constants
        caseProbability = 279472/populationData$Population #ontario case data as of Monday Feb 8th 2021
        deathProbability = 6538/279472 #ontario death data as of Monday Feb 8th
        highPopDensity = 3713811/populationData$Population #population of toronto and mississauga - see helper file
        
        ## determining age distributions
        teenProb = round(startingPop*(populationData$Age16to19/populationData$eligiblePopulation))
        twentyProb = round(startingPop*(populationData$Age20s/populationData$eligiblePopulation))
        thirtiesProb = round(startingPop*(populationData$Age30s/populationData$eligiblePopulation))
        fourtiesProb = round(startingPop*(populationData$Age40s/populationData$eligiblePopulation))
        fiftiesProb = round(startingPop*(populationData$Age50s/populationData$eligiblePopulation))
        sixtiesProb = round(startingPop*(populationData$Age60s/populationData$eligiblePopulation))
        seventiesProb = round(startingPop*(populationData$Age70s/populationData$eligiblePopulation))
        eightiesProb = round(startingPop*(populationData$Age80s/populationData$eligiblePopulation))
        nintiesProb = round(startingPop*(populationData$Age90s/populationData$eligiblePopulation))
        hundredsProb = round(startingPop*(populationData$Age100s/populationData$eligiblePopulation))
        
        ageVector = c(rep.int(1, teenProb), rep.int(2, twentyProb), rep.int(3, thirtiesProb), 
                      rep.int(4, fourtiesProb), rep.int(5, fiftiesProb), rep.int(6, sixtiesProb), 
                      rep.int(7, seventiesProb), rep.int(8, eightiesProb), rep.int(9, nintiesProb), 
                      rep.int(10, hundredsProb))

        ageVector = sample(ageVector, length(ageVector))
        ageVector = ageVector[1:startingPop]

        
        populationMatrix = tibble("PersonID" = NA, "Vaccinated" = NA, "Status" = NA, "PopDensity" = NA, "Age" = NA, "LTC" = NA, "VaccineType" = NA, "Suseptibility" = NA, .rows = startingPop) %>%
            mutate(PersonID = c(1:startingPop), Vaccinated = 0, Status = 0, PopDensity = rbinom(startingPop, 1, highPopDensity), Age = ageVector, LTC = 0, VaccineType = 0, Suseptibility = 1)
        noVaccPopulationMatrix = populationMatrix
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
        vaccPop = tibble("newVaccinatedPop" = NA, "cumulativeVaccPop" = NA, "week"= NA) %>% remove_missing(na.rm = TRUE)
        covidCases = tibble("newCases" = NA, "currentCases" = NA, "cumulativeCases" = NA, "week" = NA)%>% remove_missing(na.rm = TRUE)
        covidDeaths = tibble("newDeaths" = NA, "cumulativeDeaths" = NA, "week" = NA)%>% remove_missing(na.rm = TRUE)
        covidCasesNoVacc = tibble("newCases" = NA, "currentCases" = NA, "cumulativeCases" = NA, "week" = NA)%>% remove_missing(na.rm = TRUE)
        covidDeathsNoVacc = tibble("newDeaths" = NA, "cumulativeDeaths" = NA, "week" = NA)%>% remove_missing(na.rm = TRUE)
        

    
        ### Establishing vaccination order 
        if(input$strat == "Random"){ #random
            populationMatrix = populationMatrix %>% filter(Vaccinated == 0) %>% 
                mutate(VaccOrder = sample(PersonID, startingPop)) %>% 
                arrange(VaccOrder)
        } else if(input$strat == "High Population Density"){ #pop density
            populationMatrix = populationMatrix %>% filter(Vaccinated == 0) %>% 
                arrange(desc(PopDensity)) %>% 
                mutate(vaccOrder = c(1:startingPop))
        } else if(input$strat == "Oldest to Youngest"){ #oldest first
            populationMatrix = populationMatrix %>% filter(Vaccinated == 0) %>% 
                arrange(desc(Age)) %>% 
                mutate(vaccOrder = c(1:startingPop))
        } else if(input$strat == "Youngest to Oldest"){ #youngest first
            populationMatrix = populationMatrix %>% filter(Vaccinated == 0) %>% 
                arrange(Age) %>% 
                mutate(vaccOrder = c(1:startingPop))
        }
    
    
    

    ### Simulation Time
     for (currentTime in 1:time){
         
         if (currentTime == 1){
             susceptiblePopulation = reactive({populationMatrix %>% filter(Status == 0)})
             newInfected = reactive({sample(susceptiblePopulation()$PersonID, startingCases)})
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = PersonID %in% sample(susceptiblePopulation()$PersonID, startingCases), yes = 1, no = Status))
             
             
             #no vaccination as control
             noVaccSusceptiblePopulation = reactive({noVaccPopulationMatrix %>% filter(Status == 0)})
             noVaccNewInfected = reactive({sample(noVaccSusceptiblePopulation()$PersonID, startingCases)})
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = PersonID %in% sample(noVaccSusceptiblePopulation()$PersonID, startingCases), yes = 1, no = Status))
             
             
         }
         else{
             ### Infection Round
             currentCasesMatrix = populationMatrix %>% filter(Status == 1 | Status == 2)
             currentCases = as.numeric(count(currentCasesMatrix))
             
             ## deaths
             deathsThisWeek = rbinom(1, currentCases, deathProbability)
             newDeaths = sample(currentCasesMatrix$PersonID, deathsThisWeek)
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = PersonID %in% newDeaths, yes = 3, no = Status))
             
             ## people get better
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = Status == 2, yes = 0, no = Status),
                                                            Status = ifelse(test = Status == 1, yes = 2, no = Status))
             
             ## infected
             infectedThisWeek = rpois(1, lambda = 1.05*currentCases*(1/2)) #1.05 is ontario's median Re value
             currentHealthy = as.numeric(count(populationMatrix %>% filter(Status == 0)))
             if (infectedThisWeek > currentHealthy){
                 infectedThisWeek = currentHealthy
             }
             
             
             susceptiblePopulation = populationMatrix %>% filter(Status == 0)
             contactIDs= sample(susceptiblePopulation$PersonID, infectedThisWeek)
             contactPopulation = populationMatrix %>% filter(PersonID %in% contactIDs) %>% 
                 mutate(Infected = rbinom(contactIDs, 1, Suseptibility))
             infectedIDs = contactPopulation %>% filter(Infected == 1)
             
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = PersonID %in% infectedIDs$PersonID, yes = 1, no = Status))
             
             
          ### NO VACC 
             
             ### Infection Round
             noVaccCurrentCasesMatrix = noVaccPopulationMatrix %>% filter(Status == 1 | Status == 2)
             noVaccCurrentCases = as.numeric(count(noVaccCurrentCasesMatrix))
             
             ## deaths
             noVaccDeathsThisWeek = rbinom(1, noVaccCurrentCases, deathProbability)
             noVaccNewDeaths = sample(currentCasesMatrix$PersonID, deathsThisWeek)
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = PersonID %in% noVaccNewDeaths, yes = 3, no = Status))
             
             ## people get better
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = Status == 2, yes = 0, no = Status),
                                                            Status = ifelse(test = Status == 1, yes = 2, no = Status))
             
             ## infected
             noVaccInfectedThisWeek = rpois(1, lambda = 1.05*noVaccCurrentCases*(1/2)) #1.05 is ontario's median Re value
             noVaccCurrentHealthy = as.numeric(count(noVaccPopulationMatrix %>% filter(Status == 0)))
             if (noVaccInfectedThisWeek > noVaccCurrentHealthy){
               noVaccInfectedThisWeek = noVaccCurrentHealthy
             }
             
             
             noVaccSusceptiblePopulation = noVaccPopulationMatrix %>% filter(Status == 0)
             noVaccContactIDs= sample(noVaccSusceptiblePopulation$PersonID, noVaccInfectedThisWeek)
             noVaccContactPopulation = noVaccPopulationMatrix %>% filter(PersonID %in% noVaccContactIDs) %>% 
               mutate(Infected = rbinom(noVaccContactIDs, 1, Suseptibility))
             noVaccInfectedIDs = noVaccContactPopulation %>% filter(Infected == 1)
             
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = PersonID %in% noVaccInfectedIDs$PersonID, yes = 1, no = Status))
             
         }
         
         ### Vaccination Round - assumption 2nd dose does not impact weekly numbers
         # currently only built on Pfizer
         if(vaccPerTime !=0){
            toVaccinate = populationMatrix %>% filter(Vaccinated == 0 & Status == 0) %>% slice(1:(vaccPerTime))
             populationMatrix = populationMatrix %>% 
                mutate(Vaccinated = ifelse(test = PersonID %in% toVaccinate$PersonID, yes = 1, no = Vaccinated),
                        Suseptibility = ifelse(test = PersonID %in% toVaccinate$PersonID, yes = 0.48, no = Suseptibility), 
                        VaccineType = ifelse(test = PersonID %in% toVaccinate$PersonID, yes = 1, no = VaccineType))
         
         
         
            ##second round vaccinations
            if(doses == 2){
                populationMatrix = populationMatrix %>% mutate(VaccineType = ifelse(test = VaccineType > 0 & VaccineType < 4, yes = VaccineType+1, no = VaccineType), #pfizer boost
                                                                Vaccinated = ifelse(test = VaccineType == 4, yes = 2, no = Vaccinated),
                                                                Suseptibility = ifelse(test = VaccineType == 4, yes = 0.05, no = Suseptibility),
                                                                VaccineType = ifelse(test = PersonID %in% toVaccinate, yes = 1, no = VaccineType), #those just vaccinated
                                                                
                                                                VaccineType = ifelse(test =  VaccineType > 4 & VaccineType < 9, yes = VaccineType+1, no = VaccineType), #moderna Boost
                                                                Vaccinated = ifelse(test = VaccineType == 9, yes = 2, no = Vaccinated),
                                                                Suseptibility = ifelse(test = VaccineType == 9, yes = 0.059, no = Suseptibility))
             
            }
         }
         
         ## Updating vaccination tables
         cumulativeVaccinations = as.numeric(count(populationMatrix %>% filter(VaccineType!=0)))
         vaccPop = vaccPop %>% add_row("newVaccinatedPop" = vaccPerTime, "cumulativeVaccPop" = cumulativeVaccinations, "week" = currentTime)
         
         newCasesINT = as.numeric(count(populationMatrix %>% filter(Status == 1)))
         activeCases = as.numeric(count(populationMatrix %>% filter(Status == 1 | Status == 2)))
         if(currentTime == 1){
             cumulativeCases = newCasesINT
         }  else{
             cumulativeCases = as.numeric(covidCases$cumulativeCases[currentTime-1])+newCasesINT
         }
         covidCases = covidCases %>% add_row("newCases" = newCasesINT, "currentCases" = activeCases, "cumulativeCases" = cumulativeCases, "week" = currentTime)
         
         
         if(currentTime == 1){
             newDeathsINT = 0
         }  else{
             newDeathsINT= length(newDeaths)
         }
         deaths = populationMatrix %>% filter(Status == 3)
         cumulativeDeathsINT = as.numeric(count(deaths))
         covidDeaths = covidDeaths %>% add_row("newDeaths" = newDeathsINT, "cumulativeDeaths" = cumulativeDeathsINT, "week" = currentTime)
         
         
         
         ##Updating non vacc tables
         noVaccNewCasesINT = as.numeric(count(noVaccPopulationMatrix %>% filter(Status == 1)))
         noVaccActiveCases = as.numeric(count(noVaccPopulationMatrix %>% filter(Status == 1 | Status == 2)))
         if(currentTime == 1){
           noVaccCumulativeCases = noVaccNewCasesINT
         }  else{
           noVaccCumulativeCases = as.numeric(covidCasesNoVacc$cumulativeCases[currentTime-1])+noVaccNewCasesINT
         }
         covidCasesNoVacc = covidCasesNoVacc %>% add_row("newCases" = noVaccNewCasesINT, "currentCases" = noVaccActiveCases, "cumulativeCases" = noVaccCumulativeCases, "week" = currentTime)


         if(currentTime == 1){
           noVaccNewDeathsINT = 0
         }  else{
           noVaccNewDeathsINT= length(noVaccNewDeaths)
         }
         noVaccDeaths = noVaccPopulationMatrix %>% filter(Status == 3)
         noVaccCumulativeDeathsINT = as.numeric(count(noVaccDeaths))
         covidDeathsNoVacc = covidDeathsNoVacc %>% add_row("newDeaths" = noVaccNewDeathsINT, "cumulativeDeaths" = noVaccCumulativeDeathsINT, "week" = currentTime)

     }
        
        #### creating a table to compare the vacc scenario to nonvacc
    caseComparison = tibble("Vaccination" = NA, "No Vaccination" = NA, .rows = 2)
    caseComparison$Vaccination = c(covidCases$cumulativeCases[time], covidDeaths$cumulativeDeaths[time])
    caseComparison$`No Vaccination` = c(covidCasesNoVacc$cumulativeCases[time], covidDeathsNoVacc$cumulativeDeaths[time])
    caseComparison = caseComparison %>%
           mutate("Difference" = `No Vaccination` - Vaccination)
    rownames(caseComparison) <- c("Cases", "Deaths")
         
    
    
    #### creating table to provide population counts
    populationCounts = tibble("Eligible Population" = NA, "16-19 yr olds" = NA, "20-29 yr olds" = NA, "30-39 yr olds" = NA,
                              "40-49 yr olds" = NA, "50-59 yr olds" = NA, "60-69 yr olds" = NA, 
                              "70-79 yr olds" = NA, "80-89 yr olds" = NA, "90-99 yr olds" = NA,
                              "100+ yr olds" = NA, "High Population Density" = NA,
                              .rows = 1)
    populationVector = c()
    for(i in 1:10){
        populationVector = c(populationVector, as.numeric(tally(populationMatrix %>% filter(Age == i))))
    }
    populationCounts$`Eligible Population` = populationData$eligiblePopulation
    populationCounts$`16-19 yr olds` = populationVector[1]
    populationCounts$`20-29 yr olds` = populationVector[2]
    populationCounts$`30-39 yr olds` = populationVector[3]
    populationCounts$`40-49 yr olds` = populationVector[4]
    populationCounts$`50-59 yr olds` = populationVector[5]
    populationCounts$`60-69 yr olds` = populationVector[6]
    populationCounts$`70-79 yr olds` = populationVector[7]
    populationCounts$`80-89 yr olds` = populationVector[8]
    populationCounts$`90-99 yr olds` = populationVector[9]
    populationCounts$`100+ yr olds` = populationVector[10]
    populationCounts$`High Population Density` = as.numeric(tally(populationMatrix %>% filter(PopDensity == 1)))
    
    ### setting output reactive variables
    values$populationMatrix = populationMatrix
    values$covidCases = covidCases
    values$covidDeaths = covidDeaths
    values$vaccPop = vaccPop
    values$caseComparison = caseComparison
    values$populationCounts = populationCounts
    showTab("tabs", "1")
    showTab("tabs", "2")
    showTab("tabs", "3")
    showTab("tabs", "4")
    showTab("tabs", "6")
    })
    
    #### TABLE OUTPUTS
    output$vaccMatrix <- renderTable(values$caseComparison, rownames = TRUE)
    output$populationTable <- renderTable(values$populationCounts)
    
    
    #### CASES PLOTS
    output$newCasesPlot <- renderPlotly({
        plot = values$covidCases %>%  ggplot(aes(x=week, y=newCases)) +
            geom_line(col = "blue")+
            geom_point(col = "blue")+
            xlab("Week")+
            ylab("New Cases")+
            ggtitle("New Cases Per Week")
        ggplotly(plot) %>% config(displayModeBar = FALSE)
    })
    
    output$cumulativeCasesPlot <- renderPlotly({
        plot = values$covidCases %>%  ggplot(aes(x=week, y=cumulativeCases)) +
            geom_line(col = "blue")+
            geom_point(col = "blue")+
            xlab("Week")+
            ylab("Cumulative Cases")+
            ggtitle("Cumulative Cases Per Week")
        ggplotly(plot) %>% config(displayModeBar = FALSE)
    })
    
    output$activeCasesPlot <- renderPlotly({
        plot = values$covidCases %>%  ggplot(aes(x=week, y=currentCases)) +
            geom_line(col = "blue")+
            geom_point(col = "blue")+
            xlab("Week")+
            ylab("Active Cases")+
            ggtitle("Active Cases Per Week")
        ggplotly(plot) %>% config(displayModeBar = FALSE)
    })
    
    #### DEATH PLOTS
    
    output$newDeathsPlot <- renderPlotly({
      plot = values$covidDeaths %>%  ggplot(aes(x=week, y=newDeaths)) +
        geom_line(col = "red")+
        geom_point(col = "red")+
        xlab("Week")+
        ylab("New Deaths")+
        ggtitle("New Deaths Per Week")
      ggplotly(plot) %>% config(displayModeBar = FALSE)
    })
    
    output$cumulativeDeathsPlot <- renderPlotly({
      plot = values$covidDeaths %>%  ggplot(aes(x=week, y=cumulativeDeaths)) +
        geom_line(col = "red")+
        geom_point(col = "red")+
        xlab("Week")+
        ylab("Cumulative Deaths")+
        ggtitle("Cumulative Deaths Per Week")
      ggplotly(plot) %>% config(displayModeBar = FALSE)
    })
    
    ### VACCINATION PLOTS
    
    output$vaccinationPlot <- renderPlotly({
      plot = values$vaccPop %>% ggplot(aes(x=week, y=cumulativeVaccPop)) +
        geom_line(col = "darkgreen")+
        geom_point(col = "darkgreen")+
        xlab("Week")+
        ylab("Cumulative Vaccination")+
        ggtitle("Cumulative Vaccinations Per Week")
      ggplotly(plot) %>% config(displayModeBar = FALSE)
    })
    
    
    
    output$dataLimText <- renderUI({HTML(paste(h3("Introduction"), "This dashboard intends to show the impact of different vaccination strategies 
    with the ultimate goal of reducing the number of COVID-19 cases across the province. The initial values in the sidebar are scaled to Ontario's current situation.",
                                               "As of February 8th, Ontario had 279,472 COVID-19 cumulative cases, 6,538 deaths, and are predicting the delivery of 27,300 vaccine doses for the week of Feb. 8 - Feb. 14.",
                                               
                                               h3("Methods"), "The population for this dashboard is simulated based on the demographics in Ontario. The ages in the simulation are randomly assigned based on the percentage breakdown of ages in Ontario. 'High Population Density' was assigned using a binomial distribution and the probability was calculated using the percentage of Ontarians who live in Toronto and Mississauga.",
                                               "As for the spread of COVID-19, Ontario's median effective reproduction number (Re = 1.05) was utilized. Re is the number of people one sick individual can be expected to infect. Since a COVID-19 infection, to the best of our knowledge, lasts 2 weeks on average, this Re values was divided by two to account of the weekly nature of the simulation. This 0.5Re value was used to simulate infections 
using a Poisson distribution with 0.5Re*(current Active Cases) as the rate. Anyone who is currently well is susceptible, including those vaccianted, has the potenial to be exposed and which people are exposed is done randomly via a sample. To determine if this exposure results in an infection, a binomial distribution is run with probability equal to 1 minus the efficiacy of any vaccine they may have gotten. Deaths 
are also calcuated with a binomal distribution, where the death probability was calculated by dividing Ontario's actual deaths by the actual cumulative cases. Determining which individual dies is done via a sample of all active cases. This current dashboard is built using Pfizer's vaccine exclusively which has a one dose efficacy of 52% and a two dose efficacy of 95%, according to the BBC.", strong("High Population Density Vaccination Strategy:"), "Those who live in dense urban areas are first priority, then it is random.", 
                                               strong("Oldest to Youngest/Youngest to Oldest Vaccination Strategy:"), "The entire population is grouped and sorted via decade and vaccinations are done oldest to youngest or youngest to oldest", strong("Random Vaccination Strategy:"), "The order is determined by a full sample.", 
                                               h3("Limitations"), "Currently, this dashboard only takes the Pfizer vaccine into account, as it has less age restrictions as the Moderna vaccine. The Pfizer vaccine has been approved for ages 16 and up whereas the Moderna vaccine has only been approved for 18+. 
This also causes some minor limitations as census population data is collected in 5 year sections, where late teens (15-19) are grouped together. To account for this, the late teens group was multiplied by 4/5 to elimiate the 15 year olds.",
                                               
                                               h3("Sources"), "Ontario's COVID-19 figures were retrieved on Feb. 8, 2021 from Public Health Ontario's COVID-19 Data Tool and vaccine delivery schedule was reteived from the Government of Canada's Vaccines and Treatments for COVID-19: Vaccine Rollout webpage.", "Ontario's Re value was retrived from Ontario's Open Data Catalouge on Feb. 8th.",  "The population data was retrieved from the 2016 Canadian Census via the CensusMapper API.", 
                                               "The population density metric was based on the percentage of the population living in Toronto and Mississauga. These cities were chosen as they were noted by a study done by the Fraser Institute for having the highest population density in Ontario, as reported on by Global News in 2018. The population figures for these cities come from their respective municipal websites.", "Vaccine efficacy data was retrieve from the BBC.",
                                               "", "", strong("References"), "",
                                               "CensusMapper. (2016). 2016 Canada Census [Data file]. Retrieved from https://censusmapper.ca/api.", 
                                               "City of Toronto. (n.d.). Toronto at a glance. Retrieved February 12, 2021, from https://www.toronto.ca/city-government/data-research-maps/toronto-at-a-glance/",
                                               "Government of Canada. (2021, February 05). Vaccines and treatments for COVID-19: Vaccine rollout. Retrieved February 08, 2021, from https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/prevention-risks/covid-19-vaccine-treatment/vaccine-rollout.html#a4", 
                                               "Region of Peel. (n.d.). Population Change. Retrieved February 12, 2021, from https://www.peelregion.ca/finance/economic-indicators/population-change.asp", 
                                               "Ontario COVID-19 data tool. (2021). Retrieved February 08, 2021, from https://www.publichealthontario.ca/en/data-and-analysis/infectious-disease/covid-19-data-surveillance/covid-19-data-tool?tab=summary", 
                                               "Ontario Data Catalogue. (2021). Effective Reproduction Number (Re) [Data file]. Retrieved from https://data.ontario.ca/dataset/effective-reproduction-number-re-for-covid-19-in-ontario/resource/1ffdf824-2712-4f64-b7fc-f8b2509f9204.", 
                                               "Vella, E. (2018, January 10). Population density in Toronto significantly less compared to other major cities: Fraser Institute. Retrieved February 12, 2021, from https://globalnews.ca/news/3954609/population-density-in-toronto-fraser-institute/", "", "",
                                               
                                               
                                               sep = "<br/>"))})

    
    
    
    
})
