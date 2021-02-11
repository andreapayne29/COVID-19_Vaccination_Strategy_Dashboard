library(cancensus)
library(tidyverse)
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

caseProbability = 279472/populationData$Population
deathProbability = 6538/279472

set.seed(29)
startingPop = 10000
populationMatrix = tibble("PersonID" = NA, "Vaccinated" = NA, "Status" = NA, "PopDensity" = NA, "Age" = NA, "LTC" = NA, "VaccineType" = NA, "Suseptibility" = NA, .rows = startingPop)
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

### Populating
populationMatrix$PersonID = c(1:nrow(populationMatrix))
populationMatrix$Vaccinated = 0
populationMatrix$Status = 0
populationMatrix$PopDensity = rbinom(startingPop, 1, 0.15)
populationMatrix$Age = round(runif(startingPop, min = 1, max = 10))
populationMatrix$LTC = 0
populationMatrix$VaccineType = 0
populationMatrix$Suseptibility = 1

vaccPop = tibble("newVaccinatedPop" = NA, "cumulativeVaccPop" = NA, "time"= NA) %>% remove_missing(na.rm = TRUE)
covidCases = tibble("newCases" = NA, "currentCases" = NA, "cumulativeCases" = NA, "time" = NA)%>% remove_missing(na.rm = TRUE)
covidDeaths = tibble("newDeaths" = NA, "cumulativeDeaths" = NA, "time" = NA)%>% remove_missing(na.rm = TRUE)

time = 5

priorityType = 3 #0 random, 1 pop density, 2 age decreasing, 3 age increasing
vaccPerTime = 15
doses = 2
startingCases = round(startingPop*caseProbability)


if(priorityType == 0){ #random
  possibleVaccinationPopulation = populationMatrix %>% filter(Vaccinated == 0)
  vaccinationOrder = sample(possibleVaccinationPopulation$PersonID, startingPop)
  populationMatrix = populationMatrix %>% mutate(VaccOrder = vaccinationOrder) %>% arrange(VaccOrder)
}

if(priorityType == 1){ #pop density
  populationMatrix = populationMatrix %>% filter(Vaccinated == 0) %>% arrange(desc(PopDensity))
  populationMatrix = populationMatrix %>% mutate(vaccOrder = c(1:startingPop))
}

if(priorityType == 2){ #oldest first
  populationMatrix = populationMatrix %>% filter(Vaccinated == 0) %>% arrange(desc(Age))
  populationMatrix = populationMatrix %>% mutate(vaccOrder = c(1:startingPop))
}

if(priorityType == 3){ #youngest first
  populationMatrix = populationMatrix %>% filter(Vaccinated == 0) %>% arrange(Age)
  populationMatrix = populationMatrix %>% mutate(vaccOrder = c(1:startingPop))
}  
  
  
for (currentTime in 1:time){
  
  if (currentTime == 1){
    susceptiblePopulation = populationMatrix %>% filter(Status == 0)
    newInfected = sample(susceptiblePopulation$PersonID, startingCases)
    populationMatrix = populationMatrix %>% mutate(Status = ifelse(test = PersonID %in% newInfected, yes = 1, no = Status))
    
    
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
    infectedThisWeek = rpois(1, lambda = 1.05*currentCases*(1/2)) #0.94 is ontario's current Re value
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

  }
  
  ### Vaccination Round - assumption 2nd dose does not impact weekly numbers
  # currently only built on Pfizer
  
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
  
  
  cumulativeVaccinations = as.numeric(count(populationMatrix %>% filter(VaccineType!=0)))
  vaccPop = vaccPop %>% add_row("newVaccinatedPop" = vaccPerTime, "cumulativeVaccPop" = cumulativeVaccinations, "time" = currentTime)
  
  newCasesINT = as.numeric(count(populationMatrix %>% filter(Status == 1)))
  activeCases = as.numeric(count(populationMatrix %>% filter(Status == 1 | Status == 2)))
  if(currentTime == 1){
    cumulativeCases = newCasesINT
  }  else{
    cumulativeCases = as.numeric(covidCases$cumulativeCases[currentTime-1])+newCasesINT
  }
  covidCases = covidCases %>% add_row("newCases" = newCasesINT, "currentCases" = activeCases, "cumulativeCases" = cumulativeCases, "time" = currentTime)
  
  
  if(currentTime == 1){
    newDeathsINT = 0
  }  else{
    newDeathsINT= length(newDeaths)
  }
  deaths = populationMatrix %>% filter(Status == 3)
  cumulativeDeathsINT = as.numeric(count(deaths))
  covidDeaths = covidDeaths %>% add_row("newDeaths" = newDeathsINT, "cumulativeDeaths" = cumulativeDeathsINT, "time" = currentTime)
  
  
}


## Vaccination Plot
ggplot(data=vaccPop, aes(x=time, y=cumulativeVaccPop)) +
  geom_line(col = "darkgreen")+
  geom_point(col = "darkgreen")+
  xlab("Week")+
  ylab("Cumulative Vaccination")+
  ggtitle("Cumulative Vaccinations Per Week")


#Cases Plots
ggplot(data=covidCases, aes(x=time, y=cumulativeCases)) +
  geom_line(col = "blue")+
  geom_point(col = "blue")+
  xlab("Week")+
  ylab("Cumulative Cases")+
  ggtitle("Cumulative Cases Per Week")

ggplot(data=covidCases, aes(x=time, y=newCases)) +
  geom_line(col = "blue")+
  geom_point(col = "blue")+
  xlab("Week")+
  ylab("New Cases")+
  ggtitle("New Cases Per Week")

ggplot(data=covidCases, aes(x=time, y=currentCases)) +
  geom_line(col = "blue")+
  geom_point(col = "blue")+
  xlab("Week")+
  ylab("Active Cases")+
  ggtitle("Active Cases Per Week")


#deaths plots
ggplot(data=covidDeaths, aes(x=time, y=newDeaths)) +
  geom_line(col = "red")+
  geom_point(col = "red")+
  xlab("Week")+
  ylab("New Deaths")+
  ggtitle("New Deaths Per Week")

ggplot(data=covidDeaths, aes(x=time, y=cumulativeDeaths)) +
  geom_line(col = "red")+
  geom_point(col = "red")+
  xlab("Week")+
  ylab("Cumulative Deaths")+
  ggtitle("Cumulative Deaths Per Week")

