## Population Density
#Toronto and Mississauga are the cities with the highest population density in Ontario
#Therefore they will be used to determine the percentage of Ontarians who live in densely populated
#areas

torontoPop = 2956024
mississaugaPop = 757787

ontariansLivingInHighPopDensity = torontoPop+mississaugaPop

ottawaPop = 934243
bramptonPop = 593638
hamiltonPop = 536917
londonPop = 383822
markhamPop = 328966
vaughanPop = 306233
kitchenerPop = 233222
windsorPop = 217188

ontariansLivingInMediumPopDensity = ottawaPop +  bramptonPop + hamiltonPop + 
  londonPop + markhamPop + vaughanPop + kitchenerPop + windsorPop


## scaling COVID constants to a 100,000 person population
pfizerVaccinations = (512655/11078186)*100000  ## number of pfizer vaccines distributed on Feb 18th divided by Ontario's eligible population (found in server.R)
modernaVaccinations = (170600/11078186)*100000 ## number of Moderna vaccines distributed on Feb 18th div by Ontario's eligible population


## Re values for population density
re <- read_csv("App/Data/re_estimates_on.csv")$`Re` %>% sort()  ## pulled Feb 25
thirdsOfRe = round(length(re)/3)

medianRe = median(re)

lowestRe = median(re[(1:thirdsOfRe)])
middleRe = median(re[((thirdsOfRe+1):(2*thirdsOfRe))])
highestRe = median(re[((2*thirdsOfRe+1):length(re))])

