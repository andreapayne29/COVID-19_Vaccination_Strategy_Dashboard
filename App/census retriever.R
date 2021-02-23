### Accessing Ontario census data 

library(cancensus)
options(cancensus.api_key='') #API key removed for security reasons
options(cancensus.cache_path = "") #input personal directory for data

write.csv(get_census(dataset='CA16', regions=list(PR="35"), 
                     vectors=c("v_CA16_401","v_CA16_4", "v_CA16_64","v_CA16_82",
                               "v_CA16_100","v_CA16_118","v_CA16_136",
                               "v_CA16_154","v_CA16_172","v_CA16_190",
                               "v_CA16_208","v_CA16_226","v_CA16_247",
                               "v_CA16_265","v_CA16_283","v_CA16_301",
                               "v_CA16_322","v_CA16_340","v_CA16_358",
                               "v_CA16_376"), labels="detailed", 
                     geo_format=NA, level='Regions'), file = "Census Data/censusData.csv", row.names = FALSE)
