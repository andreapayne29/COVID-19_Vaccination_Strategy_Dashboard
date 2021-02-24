## Population Density
#Toronto and Mississauga are the cities with the highest population density in Ontario
#Therefore they will be used to determine the percentage of Ontarians who live in densely populated
#areas

torontoPop = 2956024
mississaugaPop = 757787

ontariansLivingInHighPopDensity = torontoPop+mississaugaPop


## scaling COVID constants to a 100,000 person population
pfizerVaccinations = (512655/11078186)*100000  ## number of pfizer vaccines distributed on Feb 18th divided by Ontario's eligible population (found in server.R)
modernaVaccinations = (170600/11078186)*100000 ## number of Moderna vaccines distributed on Feb 18th div by Ontario's eligible population
