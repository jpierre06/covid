#country by WHO region
who_region = read_csv("data/who_covid_19_sit_rep_time_series.csv")

#select colums, rename columns, remove rows NA and remove duplicates
who_region = who_region %>% dplyr::select(`Country/Region`,`WHO region`)
colnames(who_region)[1] <- 'Country'
colnames(who_region)[2] <- 'WHO_Region'
who_region = who_region %>% drop_na()
who_region = who_region %>% distinct_all()

#name conversion
who_region <- who_region %>% mutate(Country_adjust = Country)
who_region$Country_adjust <- gsub("United States","US", who_region$Country_adjust)


#write csv file who region
write.csv(x = who_region, file = "data/who_region.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)


rm(who_region)
