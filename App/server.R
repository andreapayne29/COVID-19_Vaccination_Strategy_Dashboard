#Library Loading


library(plotly)

library(shiny)

library(shinyalert)

library(tidyverse)






shinyServer(function(input, output) {
    
    hideTab(inputId = "tabs", target = "1")
    hideTab("tabs", "2")
    hideTab("tabs", "3")
    hideTab("tabs", "4")
    hideTab("tabs", "6")
    

    #################### Accessing Data and Cleaning

    census_data = read_csv("Data/censusData.csv")
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
      
      withProgress(message = 'Simulating', value = 0, {
       
        
        
        #### Setting User Inputs
        startingPop = 1000000
        startingCases = round((input$originalVirusInfections/100)*startingPop)
        doses = as.numeric(input$doses)
        pfizerVaccPerTime = input$genericVac
        modernaVaccPerTime = input$modernaVac
    
        
        
        ### Setting constants
        time = 31 #31 weeks between march 1st and Sept 30th
        
        casesScaling = 298569/populationData$Population #ontario case data as of Fri Feb 26th 2021
        cumulativeCasesPreSimulation = round(casesScaling*startingPop)
        
        deathProbability = 6944/298569 #ontario death data as of Fri Feb 26th 2021
        cumulativeDeathsPreSimulation = round(deathProbability*startingPop)
        
        highPopDensity = round((3713811/populationData$Population)*startingPop) #population of toronto and mississauga - see helper file
        mediumPopDensity = round((3534229/populationData$Population)*startingPop) #population of top 10 populated cities in Ontario (not incl toronto and mississauga) - see helper file
        lowPopDensity = ceiling(startingPop-highPopDensity-mediumPopDensity)
        
        highestRe = 1.19 # see helper function for Re calculations
        middleRe = 1.045
        lowestRe = 0.875
        medianRe = 1.04
        
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
        ageVector = ageVector[1:(startingPop)]
        teenageSplit = rbinom(teenProb, 1, 0.5) #for moderna vaccinations
        
        populationDensityVector = c(rep.int(2, highPopDensity), rep.int(1, mediumPopDensity), rep.int(0, lowPopDensity))
        populationDensityVector = sample(populationDensityVector, length(populationDensityVector))
        
        
        populationMatrix = tibble("PersonID" = NA, "Vaccinated" = NA, "Status" = NA, "PopDensity" = NA, "Age" = NA, "VaccineType" = NA, "Suseptibility" = NA, "Re" = NA, .rows = startingPop) %>%
            mutate(PersonID = c(1:startingPop), Vaccinated = 0, Status = 0, PopDensity = populationDensityVector, Age = ageVector, LTC = 0, VaccineType = 0, Suseptibility = 1) %>%
            mutate(ModernaEligible = ifelse(test = Age == 1, yes = teenageSplit, no = 1))
        populationMatrix = populationMatrix %>% mutate(Re = ifelse(PopDensity == 2, yes = highestRe, no = Re)) %>% 
          mutate(Re = ifelse(PopDensity == 1, yes = middleRe, no = Re)) %>%
          mutate(Re = ifelse(PopDensity == 0, yes = lowestRe, no = Re))
          
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
        vaccPop = tibble("newVaccinatedPop" = NA, "Total Vaccinations" = NA, "Pfizer Vaccinations" = NA, "Moderna Vaccinations" = NA, "Re" = NA, "week"= NA) %>% remove_missing(na.rm = TRUE)
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
       
       #for the progress bar
       incProgress(1/31, detail = paste("Simulating Week", (currentTime)))
       
         
         if (currentTime == 1){
             susceptiblePopulation = populationMatrix %>% filter(Status == 0)
             newInfected = sample(susceptiblePopulation$PersonID, startingCases)
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = PersonID %in% sample(susceptiblePopulation$PersonID, startingCases), yes = 1, no = Status))
             
             
             #no vaccination as control
             noVaccSusceptiblePopulation = noVaccPopulationMatrix %>% filter(Status == 0)
             noVaccNewInfected = sample(noVaccSusceptiblePopulation$PersonID, startingCases)
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = PersonID %in% sample(noVaccSusceptiblePopulation$PersonID, startingCases), yes = 1, no = Status))
             
             
         }
         else{
             ### Infection Round
             currentCasesMatrix = populationMatrix %>% filter(Status == 1 | Status == 2)
             reValue = mean(currentCasesMatrix$Re)
             if(is.na(reValue)){reValue =0}
             currentCases = as.numeric(count(currentCasesMatrix))
             
             ## deaths
             deathsThisWeek = rbinom(1, currentCases, deathProbability)
             if(deathsThisWeek != 0){
               newDeaths = sample(currentCasesMatrix$PersonID, deathsThisWeek)
             }else{newDeaths = NULL}
            
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = PersonID %in% newDeaths, yes = 3, no = Status))
             
             ## people get better
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = Status == 2, yes = 0, no = Status),
                                                            Status = ifelse(test = Status == 1, yes = 2, no = Status))
             
             ## infected
             infectedThisWeek = rpois(1, lambda = reValue*currentCases*(1/2)) 
             currentHealthy = as.numeric(count(populationMatrix %>% filter(Status == 0)))
             if (infectedThisWeek > currentHealthy){
                 infectedThisWeek = currentHealthy
             }
             
             susceptiblePopulation = populationMatrix %>% filter(Status == 0)
             contactIDs= sample(susceptiblePopulation$PersonID, infectedThisWeek)
             contactPopulation = populationMatrix %>% filter(PersonID %in% contactIDs) %>% 
                mutate(Infected = rbinom(length(contactIDs), 1, Suseptibility))
             infectedIDs = contactPopulation %>% filter(Infected == 1)
             
             populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = PersonID %in% infectedIDs$PersonID, yes = 1, no = Status))
             
             
          ### NO VACC 
             
             ### Infection Round
             noVaccCurrentCasesMatrix = noVaccPopulationMatrix %>% filter(Status == 1 | Status == 2)
             noVaccCurrentCases = as.numeric(count(noVaccCurrentCasesMatrix))
             
             ## deaths
             noVaccDeathsThisWeek = rbinom(1, noVaccCurrentCases, deathProbability)
             noVaccNewDeaths = sample(noVaccCurrentCasesMatrix$PersonID, noVaccDeathsThisWeek)
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = PersonID %in% noVaccNewDeaths, yes = 3, no = Status))
             
             ## people get better
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = Status == 2, yes = 0, no = Status),
                                                            Status = ifelse(test = Status == 1, yes = 2, no = Status))
             
             ## infected
             reValueNoVacc = mean(noVaccCurrentCasesMatrix$Re)
             if(is.na(reValueNoVacc)){reValue =0}
             
             noVaccInfectedThisWeek = rpois(1, lambda = reValueNoVacc*noVaccCurrentCases*(1/2)) 
             noVaccCurrentHealthy = as.numeric(count(noVaccPopulationMatrix %>% filter(Status == 0)))
             if (noVaccInfectedThisWeek > noVaccCurrentHealthy){
               noVaccInfectedThisWeek = noVaccCurrentHealthy
             }
             
             
             noVaccSusceptiblePopulation = noVaccPopulationMatrix %>% filter(Status == 0)
             noVaccContactIDs= sample(noVaccSusceptiblePopulation$PersonID, noVaccInfectedThisWeek)
             noVaccContactPopulation = noVaccPopulationMatrix %>% filter(PersonID %in% noVaccContactIDs) %>% 
               mutate(Infected = rbinom(length(noVaccContactIDs), 1, Suseptibility))
             noVaccInfectedIDs = noVaccContactPopulation %>% filter(Infected == 1)
             
             noVaccPopulationMatrix = noVaccPopulationMatrix %>% mutate(Status = ifelse(test = PersonID %in% noVaccInfectedIDs$PersonID, yes = 1, no = Status))
             
         }
         
         ### Vaccination Round - assumption 2nd dose does not impact weekly numbers
         # currently only built on Pfizer
         if((pfizerVaccPerTime+modernaVaccPerTime) !=0){
           
          
           #pfizer vaccinations
           
            #checking for min of nonvaccinated population and vaccs available
            waitingForVaccinations = populationMatrix %>% filter(Vaccinated == 0 & Status == 0)
            countWaitingForVaccinations = as.numeric(count(waitingForVaccinations))
            pfizerVaccinations = min(countWaitingForVaccinations, pfizerVaccPerTime)
            
            pfizerToVaccinate = populationMatrix %>% filter(Vaccinated == 0 & Status == 0) %>% slice(1:(pfizerVaccinations))
            populationMatrix = populationMatrix %>% 
              mutate(Vaccinated = ifelse(test = PersonID %in% pfizerToVaccinate$PersonID, yes = 1, no = Vaccinated),
                      Suseptibility = ifelse(test = PersonID %in% pfizerToVaccinate$PersonID, yes = 0.48, no = Suseptibility), 
                      VaccineType = ifelse(test = PersonID %in% pfizerToVaccinate$PersonID, yes = 1, no = VaccineType))
            
            #moderna vaccinations
            
            #checking for min of nonvaccinated population and vaccs available
            modernaWaitingForVaccinations = populationMatrix %>% filter(Vaccinated == 0 & Status ==0 & ModernaEligible == 1)
            modernaCountWaitingForVaccinations = as.numeric(count(waitingForVaccinations))
            modernaVaccinations = min(modernaCountWaitingForVaccinations, modernaVaccPerTime)
            
            
            modernaToVaccinate = populationMatrix %>% filter(Vaccinated == 0 & Status == 0 & ModernaEligible == 1) %>% slice(1:(modernaVaccinations))
            populationMatrix = populationMatrix %>% 
              mutate(Vaccinated = ifelse(test = PersonID %in% modernaToVaccinate$PersonID, yes = 1, no = Vaccinated),
                     Suseptibility = ifelse(test = PersonID %in% modernaToVaccinate$PersonID, yes = 0.198, no = Suseptibility),
                     VaccineType = ifelse(test = PersonID %in% modernaToVaccinate$PersonID, yes = 5, no = VaccineType))
         
         
            ##second round vaccinations
            if(doses == 2){
                populationMatrix = populationMatrix %>% mutate(VaccineType = ifelse(test = VaccineType > 0 & VaccineType < 4, yes = VaccineType+1, no = VaccineType), #pfizer boost
                                                                Vaccinated = ifelse(test = VaccineType == 4, yes = 2, no = Vaccinated),
                                                                Suseptibility = ifelse(test = VaccineType == 4, yes = 0.05, no = Suseptibility),
                                                                VaccineType = ifelse(test = PersonID %in% pfizerToVaccinate$PersonID, yes = 1, no = VaccineType), #those just vaccinated
                                                                
                                                                VaccineType = ifelse(test =  VaccineType > 4 & VaccineType < 9, yes = VaccineType+1, no = VaccineType), #moderna Boost
                                                                Vaccinated = ifelse(test = VaccineType == 9, yes = 2, no = Vaccinated),
                                                                Suseptibility = ifelse(test = VaccineType == 9, yes = 0.059, no = Suseptibility),
                                                                VaccineType = ifelse(test = PersonID %in% modernaToVaccinate$PersonID, yes = 5, no = VaccineType)) #those just vaccinated)
             
            }
         }
         
         ## Updating vaccination tables
         cumulativeVaccinations = as.numeric(count(populationMatrix %>% filter(VaccineType!=0)))
         newPfizerVaccinations = as.numeric(count(populationMatrix%>% filter(VaccineType == 1)))
         newModernaVaccinations = as.numeric(count(populationMatrix%>% filter(VaccineType == 5)))
         
         totalPfizerVaccinations = as.numeric(count(populationMatrix%>% filter(VaccineType == 1 | VaccineType == 2 | VaccineType == 3 | VaccineType == 4)))
         totalModernaVaccinations = as.numeric(count(populationMatrix%>% filter(VaccineType == 5 | VaccineType == 6 | VaccineType == 7 | VaccineType == 8 | VaccineType == 9)))
         
         newVaccinations = newPfizerVaccinations + newModernaVaccinations
         vaccPop = vaccPop %>% add_row("newVaccinatedPop" = newVaccinations, "Total Vaccinations" = cumulativeVaccinations, "Pfizer Vaccinations" = totalPfizerVaccinations, "Moderna Vaccinations" = totalModernaVaccinations,  "week" = currentTime)
         
         newCasesINT = as.numeric(count(populationMatrix %>% filter(Status == 1)))
         activeCases = as.numeric(count(populationMatrix %>% filter(Status == 1 | Status == 2)))
         if(currentTime == 1){
             cumulativeCases = newCasesINT + cumulativeCasesPreSimulation
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
         cumulativeDeathsINT = as.numeric(count(deaths)) + cumulativeDeathsPreSimulation
         covidDeaths = covidDeaths %>% add_row("newDeaths" = newDeathsINT, "cumulativeDeaths" = cumulativeDeathsINT, "week" = currentTime)
         
         
         
         ##Updating non vacc tables
         noVaccNewCasesINT = as.numeric(count(noVaccPopulationMatrix %>% filter(Status == 1)))
         noVaccActiveCases = as.numeric(count(noVaccPopulationMatrix %>% filter(Status == 1 | Status == 2)))
         if(currentTime == 1){
           noVaccCumulativeCases = noVaccNewCasesINT + cumulativeCasesPreSimulation
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
         noVaccCumulativeDeathsINT = as.numeric(count(noVaccDeaths)) +cumulativeDeathsPreSimulation
         covidDeathsNoVacc = covidDeathsNoVacc %>% add_row("newDeaths" = noVaccNewDeathsINT, "cumulativeDeaths" = noVaccCumulativeDeathsINT, "week" = currentTime)
          print(currentTime)
     }
        
        #### creating a table to compare the vacc scenario to nonvacc
    caseComparison = tibble("Vaccination" = NA, "No Vaccination" = NA, .rows = 2)
    caseComparison$Vaccination = c(covidCases$cumulativeCases[time], covidDeaths$cumulativeDeaths[time])
    caseComparison$`No Vaccination` = c(covidCasesNoVacc$cumulativeCases[time], covidDeathsNoVacc$cumulativeDeaths[time])
    caseComparison = caseComparison %>%
           mutate("Difference" = `No Vaccination` - Vaccination)
    rownames(caseComparison) <- c("Cases", "Deaths")
    
    
    
    #### creating table to provide population counts
    populationCounts = tibble("Death Proportion in Ontario (%)" = NA, "Eligible Population" = NA, "16-19 yr olds" = NA, "20-29 yr olds" = NA, "30-39 yr olds" = NA,
                              "40-49 yr olds" = NA, "50-59 yr olds" = NA, "60-69 yr olds" = NA, 
                              "70-79 yr olds" = NA, "80-89 yr olds" = NA, "90-99 yr olds" = NA,
                              "100+ yr olds" = NA, "High Population Density" = NA,
                              .rows = 1)
    populationVector = c()
    for(i in 1:10){
        populationVector = c(populationVector, as.numeric(tally(populationMatrix %>% filter(Age == i))))
    }
    populationCounts$`Death Proportion in Ontario (%)` = deathProbability*100
    populationCounts$`Eligible Population` = startingPop
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
    values$casesDifference = caseComparison$`Difference`[1]
    values$deathsDifference = caseComparison$`Difference`[2]
    values$populationCounts = populationCounts
    showTab("tabs", "1")
    showTab("tabs", "2")
    showTab("tabs", "3")
    showTab("tabs", "4")
    showTab("tabs", "6")
    if((pfizerVaccPerTime+modernaVaccPerTime) < 32258){
      shinyalert("Notice", "You must have a total of at least 32,258 vaccinations per week to achieve at least one dose per person by Sept. 30, 2021.")
    }
    })
    })
    
    #### TABLE OUTPUTS
    output$vaccMatrix <- renderTable(values$caseComparison, rownames = TRUE)
    output$populationTable <- renderTable(values$populationCounts)
    
    
    #### CASES PLOTS
    ## note on the first tab - it shows an error before showing up, just a byrpoduct of the hiding/showing of tabs
    ## give it a few seconds to show up
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
      
      colors <- c("Total Vaccinations" = "darkgreen", "Pfizer Vaccinations" = "red", "Moderna Vaccinations" = "blue")
      
      total_vaccinations = values$vaccPop$`Total Vaccinations`
      pfizer_vaccinations = values$vaccPop$`Pfizer Vaccinations`
      moderna_vaccinations = values$vaccPop$`Moderna Vaccinations`
      
      plot2 = values$vaccPop %>% ggplot(aes(x=week)) +
        geom_line(aes(y=total_vaccinations, color = "Total Vaccinations"))+
        geom_point(aes(y=total_vaccinations,color = "Total Vaccinations"))+
        geom_line(aes(y=pfizer_vaccinations, color = "Pfizer Vaccinations"))+
        geom_point(aes(y=pfizer_vaccinations,color = "Pfizer Vaccinations"))+
        geom_line(aes(y=moderna_vaccinations, color = "Moderna Vaccinations"))+
        geom_point(aes(y=moderna_vaccinations,color = "Moderna Vaccinations"))+
        labs(
          x = "Week",
          y = "Cumulative Vaccinations",
          color = "Legend")+
        scale_color_manual(values = colors)
      ggplotly(plot2) %>% config(displayModeBar = FALSE)
      

    })
    
    output$casesText <- renderUI({HTML(paste(h4(paste("Cases Prevented:", values$casesDifference)), " ", " "), sep = "<br/>" ) })
    output$deathsText <- renderUI({HTML(paste(h4(paste("Deaths Prevented:", values$deathsDifference)), " ", " "), sep = "<br/>" ) })
    
    
    output$introduction <- renderUI({HTML(paste(h3("Introduction"), "This dashboard intends to show the impact of different vaccination strategies running from March 1st to September 30th, 2021 (31 weeks), with the ultimate goal of reducing the number of COVID-19 cases across Ontario. This simulation has scaled Ontario’s population down to 1,000,000 while still maintaining the distribution of sociodemographic factors across the province.
                                                The default value of COVID-19 infection percentage is reflective of Ontario’s current situation."," ","As of February 26, 2021, Ontario has 298,569 cumulative cases, 6,944 deaths, and 15,029 cases reported in the last 14 days. Based on forecasts from the Government of Canada, Ontario is expected to administer 186,030 Pfizer vaccines (16,792 per 1M people) and 47,400 Moderna vaccines (4,279 per 1M people) for the week of February 22, 2021.",
                                                h3("Strategy"), 
                                                strong("High Population Density Vaccination Strategy:"), "Those who live in dense urban areas are first priority, suburban areas are second, and rural areas are third.", 
                                                strong("Oldest to Youngest/Youngest to Oldest Vaccination Strategy:"), "The entire population is grouped and sorted via decade and vaccinations are administered oldest to youngest or youngest to oldest.", 
                                                strong("Random Vaccination Strategy:"), "The vaccination order is determined by a full random sample.", " ", 
                                                h3("References and Data Sources"), 
                                                "CensusMapper. (2016). 2016 Canada Census [Data file]. Retrieved from https://censusmapper.ca/api.",
                                                " ","City of Toronto. (n.d.). Toronto at a glance. Retrieved February 12, 2021, from https://www.toronto.ca/city-government/data-research-maps/toronto-at-a-glance/",
                                                " ","Government of Canada. (2021, February 18). Vaccines and treatments for COVID-19: Vaccine rollout. Retrieved February 08, 2021, from https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/prevention-risks/covid-19-vaccine-treatment/vaccine-rollout.html#a4",
                                                " ","Nag, O. (2019, February 01). The 10 Biggest Cities In Ontario. Retrieved February 26, 2021, from https://www.worldatlas.com/articles/the-10-biggest-cities-in-ontario.html",
                                                " ","Ontario COVID-19 data tool. (2021). Retrieved February 26, 2021, from https://www.publichealthontario.ca/en/data-and-analysis/infectious-disease/covid-19-data-surveillance/covid-19-data-tool?tab=summary",
                                                " ","Ontario Data Catalogue. (2021). Effective Reproduction Number (Re) [Data file]. Retrieved from https://data.ontario.ca/dataset/effective-reproduction-number-re-for-covid-19-in-ontario/resource/1ffdf824-2712-4f64-b7fc-f8b2509f9204.",
                                                " ","Region of Peel. (n.d.). Population Change. Retrieved February 12, 2021, from https://www.peelregion.ca/finance/economic-indicators/population-change.asp",
                                                " ","Vella, E. (2018, January 10). Population density in Toronto significantly less compared to other major cities: Fraser Institute. Retrieved February 12, 2021, from https://globalnews.ca/news/3954609/population-density-in-toronto-fraser-institute/", " ", " ", 
                                                sep = "<br/>"))})
    
    output$manipulation <- renderUI({HTML(paste(h3("Calculation Methods"),
                                                strong("Defining Population Density"), 
                                                "Due to COVID-19 being transported via respiratory droplets, those with higher contact with others are at higher risk of becoming infected. This logic can be extended to population density, where those living in densely populated areas are at higher risk than 
                                                those in rural areas."," ",
                                                "To account for this, the population was assigned markers declaring them in high, medium, or low population density. High population density was defined by the scaled population of Toronto and Mississauga as these two cities are in the top
                                                25 densely populated cities in the world, reported by Global News. Medium density was defined using the scaled population of the eight other cities in Ontario’s top 10 highest populated cities (Toronto and Mississauga were omitted from this section). Low density 
                                                was defined as the remaining population. Once the proportion of the population in each category was determined, the population density was assigned randomly to an agent in the simulation population.",
                                                " ",
                                                strong("COVID-19 Spread"),
                                                "Ontario, via the Open Data Catalogue, publishes weekly COVID-19 effective reproduction values (Re), which is the number of people one sick individual can be expected to infect.", 
                                                " ",
                                                "To account for the higher chance of encountering other people in denser populated areas, a person’s Re value depends on their population density marker. Ontario’s weekly Re values were sorted in decreasing order and then split into three different vectors. 
                                                The median of these vectors was taken and then used to assign individual Re values. Those in urban areas (high population density) were assigned the highest median, those in suburban areas (medium population density) were assigned the middle median, and those 
                                                in rural areas (low population density) were assigned the lowest median.",
                                                " ",
                                                "A Poisson distribution is utilized to determine the number of exposures per week, with a rate of <i> 0.5*(Infected Individuals)*(Average Re of Infected Individuals) </i>.", 
                                                " ","Since current research suggests a COVID-19 infection lasts generally lasts 2 weeks, the Re values were divided by two to account of the weekly nature of the simulation. The average Re value was utilized here since the Re value is not constant throughout the 
                                                population and this provides equal weight to all infected individuals.",
                                                " ","Anyone who is currently well, including those vaccinated, has the potential to be exposed and who is exposed is calculated randomly via a sample. To determine if this exposure results in an infection, a binomial distribution is run with probability equal 
                                                to <i> 1 – administered vaccine efficacy </i>. If the individual has not been vaccinated, this value is 1. This current dashboard is built using Pfizer and Moderna vaccines which have a one dose efficacy of 52% and 80.2%, and a two-dose efficacy of 95% and 
                                                94.1% respectfully, according to the BBC.", 
                                                " ",
                                                strong("COVID-19 Deaths"), "Number of deaths per week is also calculated via a binomial distribution, where the probability of death is Ontario's actual deaths divided by the actual cumulative cases. Determining which individual dies is done via a sample of all active cases.", 
                                                
                                                h3("Limitations and Assumptions"), 
                                                "The Pfizer vaccine has been approved for ages 16 and up whereas the Moderna vaccine has only been approved for 18+. This causes some minor limitations as census population data is collected in 5-year sections, where late teens (15-19) are grouped together. To account
                                                for this, the late teens group was multiplied by 4/5 to eliminate the 15-year-olds in the eligible population. This reduced teenage population was randomly split in two via a sample and one group was marked as below 18 and ineligible for the Moderna vaccine.",
                                                " ",
                                                "There are also some limitations on the Re values used. Three medians of Ontario's total Re value, where utilized, so it is the same three options for every infected person. This means we cannot take other spread-increasing factors into account, for example, one’s 
                                                workplace industry, long-term care home residency, or unexpected spread within Public Health Units.", 
                                                " ",
                                                "In addition, the assumption is made with a 2-dose system that the weekly vaccination numbers are new vaccinations. This means second-dose vaccinations are not counted in weekly numbers.",
                                                " ", " ", " ", sep = "<br/>"))})
  
    
    
    
    
})
