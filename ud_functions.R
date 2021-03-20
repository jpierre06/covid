#user defined functions

#download from zio file
updateData <- function(forceUpdate = FALSE) {
  
  if(forceUpdate){
    
    downloadGithubData()

  } else {

    # Download data from Johns Hopkins (https://github.com/CSSEGISandData/COVID-19) if the data is older than 1h
    if (!dir_exists("data/download")) {
      dir.create('data/download')
      downloadGithubData()
    } else if ((!file.exists("data/download/covid19_data.zip")) || (as.double(Sys.time() - file_info("data/download/covid19_data.zip")$change_time, units = "hours") >= 1)) {
      downloadGithubData()
    }
    
  }
  
}

# download defined files
updateDataFile <- function(forceUpdate = FALSE) {
  
	confirmed_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

	deaths_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

	recovered_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

	
  if(forceUpdate){
    
    downloadGithubDataFile(confirmed_url, "data/download/time_series_covid19_confirmed_global.csv")

    downloadGithubDataFile(deaths_url, "data/download/time_series_covid19_deaths_global.csv")

    downloadGithubDataFile(recovered_url, "data/download/time_series_covid19_recovered_global.csv")


  } else {

    # Download data from Johns Hopkins (https://github.com/CSSEGISandData/COVID-19) if the data is older than 1h
    if (!dir_exists("data/download")) {
      dir.create('data/download')
    } 

	if ((!file.exists("data/download/time_series_covid19_confirmed_global.csv")) || (as.double(Sys.time() - file_info("data/download/time_series_covid19_confirmed_global.csv")$change_time, units = "hours") >= 1)) {
		downloadGithubDataFile(confirmed_url, "data/download/time_series_covid19_confirmed_global.csv")
    }
    
	if ((!file.exists("data/download/time_series_covid19_deaths_global.csv")) || (as.double(Sys.time() - file_info("data/download/time_series_covid19_deaths_global.csv")$change_time, units = "hours") >= 1)) {
		downloadGithubDataFile(deaths_url, "data/download/time_series_covid19_deaths_global.csv")
    }
    
	if ((!file.exists("data/download/time_series_covid19_recovered_global.csv")) || (as.double(Sys.time() - file_info("data/download/time_series_covid19_recovered_global.csv")$change_time, units = "hours") >= 1)) {
		downloadGithubDataFile(recovered_url, "data/download/time_series_covid19_recovered_global.csv")
    }
    
  }
  
}



downloadGithubDataFile <- function(source_url, dest_file) {

    download.file(
    url      = source_url,
    destfile = dest_file
  )  
}





downloadGithubData <- function() {

    download.file(
    url      = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "data/download/covid19_data.zip"
  )
  
	#unzip data from time series
	unzipTimeSeries()
  
	#unzip data from who region
	#unzipWhoRegion()
  
  
}



unzipTimeSeries <- function(){

  #unzip data from time series
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  
  unzip(
    zipfile   = "data/download/covid19_data.zip",
    files     = paste0(data_path, c("confirmed_global.csv", "deaths_global.csv", "recovered_global.csv")),
    exdir     = "data/download",
    junkpaths = T
  )

}



# unzipWhoRegion <- function(){
#   
#   #unzip data from who region
#   data_path <- "COVID-19-master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv"
# 
#   unzip(
#     zipfile   = "data/download/covid19_data.zip",
#     files     = data_path,
#     exdir     = "data/download",
#     junkpaths = T
#   )
# 
# }



unzipDateDefined <- function(dateDefined){

  #unzip data from date defined
  
  
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports/"

  unzip(
    zipfile   = "data/download/covid19_data.zip",
    files     = paste0(data_path, dateDefined, '.csv'),
    exdir     = "data/daily",
    junkpaths = T
  )

}



# updatePopulation <- function() {
#   
#   # Download data from Worldometers (https://www.worldometers.info/world-population/population-by-country/) 
#   if (!file.exists("data/population_world.csv")) {
#     downloadPopulation()
#   }
#   
# }



# downloadPopulation <- function(){
# 
#   #population
#   p_w <- data.frame(html_table(read_html("https://www.worldometers.info/world-population/population-by-country/")))
#   
#   #adjustment country name, absent countries
#   population_world <- p_w %>% select (names(p_w)[2], names(p_w)[3], names(p_w)[7])
#   rm(p_w)
#   
#   colnames(population_world)[1] <- 'Country'
#   colnames(population_world)[2] <- 'Population'
#   colnames(population_world)[3] <- 'Land_area'
#   
#   
#   #include absent countries
#   #https://countrymeters.info/en/Kosovo
#   #https://worldpopulationreview.com/countries/palestine-population/
#   population_world[nrow(population_world) + 1,] = c('Kosovo', '1,810,774', '10,887')
#   population_world[nrow(population_world) + 1,] = c('West Bank and Gaza', '5,101,414', '6,020')
#   # population_world[nrow(population_world) + 1,] = c('Diamond Princess', '', '')
#   # population_world[nrow(population_world) + 1,] = c('MS Zaandam', '', '')
#   
# 
#   #name conversion
#   population_world <- population_world %>% mutate(Country_adjust = Country)
#   population_world$Country_adjust <- gsub("United States","US", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("DR Congo","Kinshasa", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Congo","Brazzaville", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Kinshasa","Congo \\(Kinshasa\\)", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Brazzaville","Congo \\(Brazzaville\\)", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Myanmar","Burma", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("South Korea","Korea, South", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Czech Republic \\(Czechia\\)","Czechia", population_world$Country_adjust, ignore.case = TRUE)
#   population_world$Country_adjust <- gsub("C?te d'Ivoire","Cote d'Ivoire", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Sao Tome & Principe","Sao Tome and Principe", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Saint Kitts & Nevis","Saint Kitts and Nevis", population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("St\\. Vincent & Grenadines", "Saint Vincent and the Grenadines",population_world$Country_adjust)
#   population_world$Country_adjust <- gsub("Taiwan","Taiwan*", population_world$Country_adjust)
#   
#   
#   #casting values
#   population_world$Population <- as.numeric(gsub(",", "", population_world$Population))
#   population_world$Land_area <- as.numeric(gsub(",", "", population_world$Land_area))
#   
#   write.csv(x = population_world, file = "data/population_world.csv", fileEncoding = "UTF-8",row.names = FALSE)
#   
# }


