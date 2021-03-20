#local functions
source("load_library.R", local = T)
source("utils.R", local = T)
source("ud_functions.R", local = T)
#source("Population.R", local = T)



# Update with start of app
updateDataFile()

#########################begin cumulated based#########################

# TODO: Still throws a warning but works for now
data_confirmed = read_csv("data/download/time_series_covid19_confirmed_global.csv")
data_deceased = read_csv("data/download/time_series_covid19_deaths_global.csv")
data_recovered = read_csv("data/download/time_series_covid19_recovered_global.csv")


#delete information is not country
#https://www.bbc.com/portuguese/internacional-52223062
#https://www.nexojornal.com.br/expresso/2020/05/15/Quais-os-12-pa%C3%ADses-do-mundo-sem-registros-da-covid-19

data_confirmed = data_confirmed %>% 
  filter(., `Country/Region` != 'MS Zaandam' )

data_confirmed = data_confirmed %>% 
  filter(., `Country/Region` != 'Diamond Princess' )

data_deceased = data_deceased %>% 
  filter(., `Country/Region` != 'MS Zaandam' )

data_deceased = data_deceased %>% 
  filter(., `Country/Region` != 'Diamond Princess' )

data_recovered = data_recovered %>% 
  filter(., `Country/Region` != 'MS Zaandam' )

data_recovered = data_recovered %>% 
  filter(., `Country/Region` != 'Diamond Princess' )




#consolidate confirmed
num_col = ncol(data_confirmed)

data_confirmed_sub = data_confirmed %>%
  pivot_longer(names_to = "date", cols = 5:num_col) %>%
  group_by(`Country/Region`, date) %>%
  summarise("confirmed" = sum(value, na.rm = T))

colnames(data_confirmed_sub)[1] = 'country'
data_confirmed_sub$date = as.Date(data_confirmed_sub$date, "%m/%d/%y")


country_confirmed_d0 = data_confirmed_sub %>%
  filter(confirmed > 0) %>%
  group_by(country) %>%
  summarise("date_d0" = min(date, na.rm = T))


#consolidate recovered
num_col = ncol(data_recovered)

data_recovered_sub = data_recovered %>%
  pivot_longer(names_to = "date", cols = 5:num_col) %>%
  group_by(`Country/Region`, date) %>%
  summarise("recovered" = sum(value, na.rm = T))

colnames(data_recovered_sub)[1] = 'country'
data_recovered_sub$date = as.Date(data_recovered_sub$date, "%m/%d/%y")


#consolidate deceased
num_col = ncol(data_deceased)

data_deceased_sub = data_deceased %>%
  pivot_longer(names_to = "date", cols = 5:num_col) %>%
  group_by(`Country/Region`, date) %>%
  summarise("deceased" = sum(value, na.rm = T))

colnames(data_deceased_sub)[1] = 'country'
data_deceased_sub$date = as.Date(data_deceased_sub$date, "%m/%d/%y")

country_deceased_d0 = data_deceased_sub %>%
  filter(deceased > 0) %>%
  group_by(country) %>%
  summarise("date_d0" = min(date, na.rm = T))



#delete dataframe temp
rm(data_confirmed, data_recovered, data_deceased, num_col)


#dataframe consolidated

#create the list country
data_covid19 = unique(data.frame(data_confirmed_sub$country))
colnames(data_covid19)[1] = 'Country'

data_covid19$Country = sort(data_covid19$Country)
data_covid19_d0 = data_covid19

list_country = data_covid19

#list dates
data_covid19_date = unique(data.frame(data_confirmed_sub$date))
colnames(data_covid19_date)[1] = 'Date'

#combitabion list country and date
#https://stackoverflow.com/questions/18705153/generate-list-of-all-possible-combinations-of-elements-of-vector
data_covid19 = expand.grid(data_covid19$Country, data_covid19_date$Date)
colnames(data_covid19)[1] = 'Country'
colnames(data_covid19)[2] = 'Date'


#join list x confirmed, recovered and deceased
data_covid19 = left_join (
  x = data_covid19, 
  y = data_confirmed_sub, 
  by = c('Country' = 'country', 'Date' = 'date'))

data_covid19 = left_join (
  x = data_covid19, 
  y = data_recovered_sub, 
  by = c('Country' = 'country', 'Date' = 'date'))

data_covid19 = left_join (
  x = data_covid19, 
  y = data_deceased_sub, 
  by = c('Country' = 'country', 'Date' = 'date'))

#calculate active and no active cases
data_covid19$active = (data_covid19$confirmed - data_covid19$recovered - data_covid19$deceased)
data_covid19$no_active = (data_covid19$confirmed - data_covid19$active)


#calculate confirmed day and deceased days
data_covid19 = left_join (
  x = data_covid19, 
  y = country_confirmed_d0, 
  by = c('Country' = 'country'))

colnames(data_covid19)[8] = 'date_confirmed_d0'

data_covid19 = left_join (
  x = data_covid19, 
  y = country_deceased_d0, 
  by = c('Country' = 'country'))

colnames(data_covid19)[9] = 'date_deceased_d0'

data_covid19$confirmed_days = as.integer(data_covid19$Date - data_covid19$date_confirmed_d0)
data_covid19$deceased_days =  as.integer(data_covid19$Date - data_covid19$date_deceased_d0)
data_covid19$deceased_days = coalesce(data_covid19$deceased_days,0L)

data_covid19$date_confirmed_d0 <- NULL 
data_covid19$date_deceased_d0 <- NULL


#join list x d0 confirmed and deceased
data_covid19_d0 = left_join (
  x = data_covid19_d0, 
  y = country_confirmed_d0, 
  by = c('Country' = 'country'))

colnames(data_covid19_d0)[2] = 'date_confirmed_d0'

data_covid19_d0 = left_join (
  x = data_covid19_d0, 
  y = country_deceased_d0, 
  by = c('Country' = 'country'))

colnames(data_covid19_d0)[3] = 'date_deceased_d0'


#insert sk date_0
data_covid19_d0$sk_date0 <- seq.int(nrow(data_covid19_d0))


#########################end cumulated based#########################


#########################begin daily based cases#########################

#remove dataframe
if (exists("data_covid19_daily")){
  rm(data_covid19_daily)
}


#lastDate = format(Sys.Date()-1, "%m-%d-%Y")
initialDate = '01-23-2020'
initialDate = as.Date(initialDate, "%m-%d-%Y")
lastDate = max(data_covid19$Date)

for (workDate in lastDate:initialDate){

    workDate = as.Date(workDate, origin = "1970-01-01")
  
  #combitabion list country and date
  temp_date = expand.grid(list_country$Country, workDate)
  
  #create previous date
  temp_date$Var3 = temp_date$Var2 -1
  
  #join work date and previous date
  temp_date = left_join (
    x = temp_date, 
    y = data_covid19, 
    by = c('Var1' = 'Country', 'Var2' = 'Date'))
  
  temp_date = left_join (
    x = temp_date, 
    y = data_covid19, 
    by = c('Var1' = 'Country', 'Var3' = 'Date'))
  
  #calculate day
  temp_date$confirmed = temp_date$confirmed.x - temp_date$confirmed.y
  temp_date$recovered = temp_date$recovered.x - temp_date$recovered.y
  temp_date$deceased = temp_date$deceased.x - temp_date$deceased.y
  temp_date$active = temp_date$active.x - temp_date$active.y
  temp_date$no_active = temp_date$no_active.x - temp_date$no_active.y
  
  colnames(temp_date)[1] = "Country"
  colnames(temp_date)[2] = "Date"
  
  #remove columns
  #http://www.datasciencemadesimple.com/drop-variables-columns-r-using-dplyr/
  #https://stackoverflow.com/questions/6286313/remove-an-entire-column-from-a-data-frame-in-r
  temp_date = dplyr::select(temp_date, c(Country, Date, confirmed, recovered, deceased, active, no_active))
  
  if (exists("data_covid19_daily")){
    data_covid19_daily =  rbind(data_covid19_daily, temp_date)
    }else{
      data_covid19_daily = temp_date
  }

}

#consilidate first date
workDate = '01-22-2020'
workDate = as.Date(workDate, "%m-%d-%Y")

#combitabion list country and date
temp_date = expand.grid(list_country$Country, workDate)

#join work date 
temp_date = left_join (x = temp_date, y = data_covid19, by = c('Var1' = 'Country', 'Var2' = 'Date'))

colnames(temp_date)[1] = "Country"
colnames(temp_date)[2] = "Date"

temp_date = dplyr::select(temp_date, c(Country, Date, confirmed, recovered, deceased, active, no_active))

if (exists("data_covid19_daily")){
  data_covid19_daily =  rbind(data_covid19_daily, temp_date)
}else{
  data_covid19_daily = temp_date
}


#calculate confirmed day and deceased days
data_covid19_daily = left_join (
  x = data_covid19_daily, 
  y = country_confirmed_d0, 
  by = c('Country' = 'country'))

colnames(data_covid19_daily)[8] = 'date_confirmed_d0'

data_covid19_daily = left_join (
  x = data_covid19_daily, 
  y = country_deceased_d0, 
  by = c('Country' = 'country'))

colnames(data_covid19_daily)[9] = 'date_deceased_d0'

data_covid19_daily$confirmed_days = as.integer(data_covid19_daily$Date - data_covid19_daily$date_confirmed_d0)
data_covid19_daily$deceased_days =  as.integer(data_covid19_daily$Date - data_covid19_daily$date_deceased_d0)
data_covid19_daily$deceased_days = coalesce(data_covid19_daily$deceased_days,0L)

data_covid19_daily$date_confirmed_d0 <- NULL 
data_covid19_daily$date_deceased_d0 <- NULL




#########################end daily based cases#########################



#########################begin insert sk ids#########################


#load data population and date time
population = read_csv("data/population_world.csv")
date_csv = read_csv("data/date.csv")
date_csv$Data = as.Date(date_csv$Data, "%d/%m/%Y")

#insert artificial key sks from teh foreign key of the dataframe data_covid_0
data_covid19 = left_join(
  x = data_covid19, 
  y = (data_covid19_d0 %>% dplyr::select(Country, sk_date0)),
  by = c('Country' = 'Country')
) 

data_covid19_daily = left_join(
  x = data_covid19_daily, 
  y = (data_covid19_d0 %>% dplyr::select(Country, sk_date0)),
  by = c('Country' = 'Country')
) 



#insert artificial key sks from teh foreign key of the dataframe population
data_covid19 = left_join(
  x = data_covid19, 
  y = (population %>% dplyr::select(Country_name, sk_population)) ,
  by = c('Country' = 'Country_name')
) 

data_covid19_daily = left_join(
  x = data_covid19_daily, y = (population %>% dplyr::select(Country_name, sk_population)) ,
  by = c('Country' = 'Country_name')
) 


#insert artificial key sks from teh foreign key of the dataframe celandar
data_covid19 = left_join(
  x = data_covid19, 
  y = (date_csv %>% dplyr::select(Data, Id_data)),
  by = c('Date' = 'Data')
) 

data_covid19_daily = left_join(
  x = data_covid19_daily, 
  y = (date_csv %>% dplyr::select(Data, Id_data)),
  by = c('Date' = 'Data')
) 


#########################end insert sk ids#########################




#########################begin error transformation#########################

errorTemp = data_covid19 %>% 
  filter(.,  is.na(sk_population))

if (nrow(errorTemp)>0){
  errorTemp$Report_name = "data_covid19"  
}

if (exists("errorTransformation")){
  errorTransformation =  rbind(errorTransformation, errorTemp)
}else{
  errorTransformation = errorTemp
}


errorTemp = data_covid19 %>% 
  filter(.,  is.na(sk_date0))

if (nrow(errorTemp)>0){
  errorTemp$Report_name = "data_covid19"  
}

if (exists("errorTransformation")){
  errorTransformation =  rbind(errorTransformation, errorTemp)
}else{
  errorTransformation = errorTemp
}


errorTemp = data_covid19 %>% 
  filter(.,  is.na(Id_data))

if (nrow(errorTemp)>0){
  errorTemp$Report_name = "data_covid19"  
}

if (exists("errorTransformation")){
  errorTransformation =  rbind(errorTransformation, errorTemp)
}else{
  errorTransformation = errorTemp
}


errorTemp = data_covid19_daily %>% 
  filter(.,  is.na(sk_population))

if (nrow(errorTemp)>0){
  errorTemp$Report_name = "data_covid19_daily"  
}

if (exists("errorTransformation")){
  errorTransformation =  rbind(errorTransformation, errorTemp)
}else{
  errorTransformation = errorTemp
}


errorTemp = data_covid19_daily %>% 
  filter(.,  is.na(sk_date0))

if (nrow(errorTemp)>0){
  errorTemp$Report_name = "data_covid19_daily"  
}

if (exists("errorTransformation")){
  errorTransformation =  rbind(errorTransformation, errorTemp)
}else{
  errorTransformation = errorTemp
}


errorTemp = data_covid19_daily %>% 
  filter(.,  is.na(Id_data))

if (nrow(errorTemp)>0){
  errorTemp$Report_name = "data_covid19_daily"  
}

if (exists("errorTransformation")){
  errorTransformation =  rbind(errorTransformation, errorTemp)
}else{
  errorTransformation = errorTemp
}


try(errorTransformation$Date_generation = Sys.time())

#########################end error transformation#########################



#########################begin the final adjustment#########################

#Order data frame data covid
#https://rpubs.com/Kassio_Ferreira/tutorialdplyr
data_covid19 = data_covid19 %>% 
  arrange(., data_covid19$Id_data, data_covid19$sk_population)

data_covid19_daily = data_covid19_daily %>% 
  arrange(., data_covid19_daily$Id_data, data_covid19_daily$sk_population)


#delete primary key
data_covid19$Country <- NULL
data_covid19$Date <- NULL
data_covid19_daily$Country <- NULL
data_covid19_daily$Date <- NULL

#########################end the final adjustment#########################



#########################begin unification accumulated and daily database#########################

data_covid19_general = left_join(
  x = data_covid19, 
  y = data_covid19_daily,
  by = c('Id_data' = 'Id_data', 'sk_population' = 'sk_population', 'sk_date0' = 'sk_date0')
) 

colnames(data_covid19_general)[1] = "confirmed_acum"
colnames(data_covid19_general)[2] = "recovered_acum"
colnames(data_covid19_general)[3] = "deceased_acum"
colnames(data_covid19_general)[4] = "active_acum"
colnames(data_covid19_general)[5] = "no_active_acum"

colnames(data_covid19_general)[6] = "confirmed_days"
colnames(data_covid19_general)[7] = "deceased_days"

colnames(data_covid19_general)[11] = "confirmed_day"
colnames(data_covid19_general)[12] = "recovered_day"
colnames(data_covid19_general)[13] = "deceased_day"
colnames(data_covid19_general)[14] = "active_day"
colnames(data_covid19_general)[15] = "no_active_day"


data_covid19_general$confirmed_days.y <- NULL
data_covid19_general$deceased_days.y <- NULL

#########################end unification accumulated and daily database#########################



#########################begin save databases#########################

#create data covid19 comulated and data covid_d0 csv
write.csv(x = data_covid19, 
          file = "data/data_covid19_cumulated.csv", 
          fileEncoding = "UTF-8", 
          row.names = FALSE, 
          quote = TRUE)

write.csv(x = data_covid19_d0, 
          file = "data/data_covid19_d0.csv", 
          fileEncoding = "UTF-8", 
          row.names = FALSE, 
          quote = TRUE)


#create data covid19 daily
write.csv(x = data_covid19_daily, 
          file = "data/data_covid19_daily.csv", 
          fileEncoding = "UTF-8", 
          row.names = FALSE, 
          quote = TRUE)


#create data covid19 general
write.csv(x = data_covid19_general, 
          file = "data/data_covid19.csv", 
          fileEncoding = "UTF-8", 
          row.names = FALSE, 
          quote = TRUE)


#create error tranformation
write.csv(x = errorTransformation, 
          file = "data/errorTransformation.csv", 
          fileEncoding = "UTF-8", 
          row.names = FALSE, 
          quote = TRUE)


#########################end save databases#########################




rm(data_covid19_date, data_confirmed_sub, data_recovered_sub, data_deceased_sub)
rm(country_confirmed_d0, country_deceased_d0)
rm(data_covid19_d0)
rm(downloadGithubData)
rm(downloadGithubDataFile)
rm(capFirst, sourceDirectory, unzipTimeSeries, unzipDateDefined)
rm(updateData)
rm(updateDataFile)
rm(data_covid19, data_covid19_daily, data_covid19_general, list_country)
rm(population, date_csv)
rm(temp_date)
rm(initialDate, lastDate, workDate)
rm(errorTemp, errorTransformation)
