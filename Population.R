#population
p_w <- data.frame(html_table(read_html("https://www.worldometers.info/world-population/population-by-country/")))


#adjustment country name, absent countries
population_world <- p_w %>% dplyr::select (names(p_w)[2], names(p_w)[3], names(p_w)[7])

colnames(population_world)[1] <- 'Country_original'
colnames(population_world)[2] <- 'Population'
colnames(population_world)[3] <- 'Land_area'


#include absent countries
#https://countrymeters.info/en/Kosovo
#https://worldpopulationreview.com/countries/palestine-population/
population_world[nrow(population_world) + 1,] = c('Kosovo', '1,810,774', '10,887')
population_world[nrow(population_world) + 1,] = c('West Bank and Gaza', '5,101,414', '6,020')
# population_world[nrow(population_world) + 1,] = c('Diamond Princess', '', '')
# population_world[nrow(population_world) + 1,] = c('MS Zaandam', '', '')


#name conversion
population_world <- population_world %>% mutate(Country_name = Country_original)
population_world$Country_name <- gsub("United States","US", population_world$Country_name)
population_world$Country_name <- gsub("DR Congo","Kinshasa", population_world$Country_name)
population_world$Country_name <- gsub("Congo","Brazzaville", population_world$Country_name)
population_world$Country_name <- gsub("Kinshasa","Congo \\(Kinshasa\\)", population_world$Country_name)
population_world$Country_name <- gsub("Brazzaville","Congo \\(Brazzaville\\)", population_world$Country_name)
population_world$Country_name <- gsub("Myanmar","Burma", population_world$Country_name)
population_world$Country_name <- gsub("South Korea","Korea, South", population_world$Country_name)
population_world$Country_name <- gsub("Czech Republic \\(Czechia\\)","Czechia", population_world$Country_name, ignore.case = TRUE)
population_world$Country_name <- gsub("Côte d\\'Ivoire","Cote d\\'Ivoire", population_world$Country_name)
population_world$Country_name <- gsub("Sao Tome & Principe","Sao Tome and Principe", population_world$Country_name)
population_world$Country_name <- gsub("Saint Kitts & Nevis","Saint Kitts and Nevis", population_world$Country_name)
population_world$Country_name <- gsub("St\\. Vincent & Grenadines", "Saint Vincent and the Grenadines",population_world$Country_name)
population_world$Country_name <- gsub("Taiwan","Taiwan*", population_world$Country_name)

population_world$Country_name <- iconv(population_world$Country_name, from="UTF-8", to="latin2//TRANSLIT")


#casting values
population_world$Population <- as.numeric(gsub(",", "", population_world$Population))
population_world$Land_area <- as.numeric(gsub(",", "", population_world$Land_area))

#insert index
#https://stackoverflow.com/questions/23518605/add-an-index-numeric-id-column-to-large-data-frame
population_world$sk_population = seq.int(nrow(population_world))


#save file
write.csv(x = population_world, 
          file = "data/population_world.csv", 
          fileEncoding = "UTF-8",
          row.names = FALSE)

#delete dataframes
rm(population_world)
rm(p_w)
