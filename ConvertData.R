library(data.table)
library(lubridate)
library(dplyr)

setwd("C:/Users/Anna Barefield/Thesis/COVID-19 US state policy database (CUSP)")
closures <- read.csv('closures_original.csv')
school_closures <- closures %>%
                        dplyr::select(1:4) %>%
                        rename(Policy = X) %>%
                        rename(Date =Closed.K.12.public.schools)
nebusiness_closures <- closures %>%
                        dplyr::select(1,2,5,6) %>%
                        rename(Policy = X.1) %>%
                        rename(Date = Closed.other.non.essential.businesses)
restaurant_closures <- closures %>%
                        dplyr::select(1,2,7,8) %>%
                        rename(Policy = X.2) %>%
                        rename(Date = Closed.restaurants)
gym_closures <- closures %>%
                        dplyr::select(1,2,9,10) %>%
                        rename(Policy = X.3) %>%
                        rename(Date = Closed.gyms)
movietheater_closures <- closures %>%
                        dplyr::select(1,2,11,12) %>%
                        rename(Policy = X.4) %>%
                        rename(Date = Closed.movie.theaters)
bar_closures <- closures %>%
                        dplyr::select(1,2,13,14) %>%
                        rename(Policy = X.5) %>%
                        rename(Date = Closed.bars)
casino_closures <- closures %>%
                        dplyr::select(1,2,15,16) %>%
                        rename(Policy = X.6) %>%
                        rename(Date = Closed.casinos)
closures <- rbindlist(list(school_closures, restaurant_closures,
                           nebusiness_closures, movietheater_closures,
                           gym_closures,casino_closures,bar_closures))
closures$Group <- "Lockdown"


reopenings <- read.csv('reopenings_original.csv')
business_reopenings <- reopenings %>%
  dplyr::select(1:4) %>%
  rename(Policy = X) %>%
  rename(Date = Began.to.reopen.businesses.statewide)
restaurant_reopenings <- reopenings %>%
  dplyr::select(1,2,5,6) %>%
  rename(Policy = X.1) %>%
  rename(Date = Reopened.restaurants)
gym_reopenings <- reopenings %>%
  dplyr::select(1,2,7,8) %>%
  rename(Policy = X.2) %>%
  rename(Date = Reopened.gyms)
movietheater_reopenings <- reopenings %>%
  dplyr::select(1,2,9,10) %>%
  rename(Policy = X.3) %>%
  rename(Date = Reopened.movie.theaters)
hairsalon_reopenings <- reopenings %>%
  dplyr::select(1,2,11,12) %>%
  rename(Policy = X.4) %>%
  rename(Date = Reopened.hair.salons.barber.shops)
otherretail_reopenings <- reopenings %>%
  dplyr::select(1,2,13,14) %>%
  rename(Policy = X.5) %>%
  rename(Date = Reopened.other.non.essential.retail)
bar_reopenings <- reopenings %>%
  dplyr::select(1,2,15,16) %>%
  rename(Policy = X.6) %>%
  rename(Date = Reopened.bars)
casino_reopenings <- reopenings %>%
  dplyr::select(1,2,17,18) %>%
  rename(Policy = X.7) %>%
  rename(Date = Reopened.casinos)
reopenings <- rbindlist(list(business_reopenings, restaurant_reopenings,
                             gym_reopenings, movietheater_reopenings,
                             hairsalon_reopenings,otherretail_reopenings,
                             bar_reopenings, casino_reopenings))
reopenings$Group <- "Reopening"

stay_at_home <- read.csv("stay_at_home.csv")
state_of_emergency <- read.csv("state_of_emergency.csv")
stay_at_home$Group <- "Lockdown"
state_of_emergency$Group <- "Lockdown"

policies <- rbindlist(list(closures, reopenings, stay_at_home, state_of_emergency))
policies[policies==0] <- NA
policies <- na.omit(policies)
policies$Date <- mdy(policies$Date)
policies <- dplyr::arrange(policies, Date)
policies <- dplyr::slice(policies, 1:(n() - 14)) 

write.csv(policies, "C:/Users/Anna Barefield/Thesis/COVID-19 US state policy database (CUSP)/policies.csv", row.names = FALSE)