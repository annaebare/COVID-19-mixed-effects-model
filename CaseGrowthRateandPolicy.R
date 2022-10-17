library(dplyr)
library(data.table)
library(tidyverse)
library(tibble)
library(zoo)


#Data processing
setwd("C:/Users/Anna Barefield/Thesis/COVID-19 US state policy database (CUSP)")
cases <- read.csv('casesbystate.csv')
names(cases)[3] <- "Z_t"
cases <- cases[1:3]
casesbystate <- split(cases, cases$state)
usstates <- read.csv('us-states.csv')
usstates <- usstates[1:3]
usstates$date <- as.Date(usstates$date)
usstates <- split(usstates, usstates$state)
usstates[c("American Samoa", "Northern Mariana Islands", 
               "Guam", "Virgin Islands", 
               "Puerto Rico")] = NULL

#First case date
firstcasevec <- c()
for(i in 1:length(usstates))
{
  firstcasevec <- c(as.Date(firstcasevec), as.Date(usstates[[i]]$date[1]))
}

#Calculate time from first case to emergency declaration for each state
policies <- read.csv('policies_updated.csv')
ed <- policies[policies$Policy=='State.of.emergency.issued',]
ed <- ed[order(ed$State),]
edvec <- as.Date(ed$Date)
days_to_ed <- as.numeric(edvec - firstcasevec)

states <- state.name
states <- append(states, 'District of Columbia', after=8)
states <- data.frame(State=states)

#School closures
sc <- policies[policies$Policy=='Closed.K.12.public.schools',]
sc <- sc[order(sc$State),]
#Rhode Island never closed schools
sc <- merge(sc, states, by="State", all=TRUE)
scvec <- as.Date(sc$Date)

#Stay at home
sh <- policies[policies$Policy=='Stay.at.home.shelter.in.place',]
sh <- sh[order(sh$State),]
sh <- merge(sh, states, by="State", all=TRUE)
shvec <- as.Date(sh$Date)

#Restaurant/Bars
rb <- policies[policies$Policy=='Closed.restaurants' | policies$Policy=='Closed.bars',]
rb <- rb[order(rb$State),]
rbsplit <- split(rb, rb$State)
rbvec <- c()
statevec <- c()
for(i in 1:length(rbsplit))
{
  statevec <- c(statevec, rbsplit[[i]]$State[1])
  rbvec <- c(rbvec, min(rbsplit[[i]]$Date))
}
rb <- data.frame(State=statevec, Date=rbvec)
rb <- merge(rb, states, by='State', all=TRUE)
rbvec <- as.Date(rb$Date)

#Other business closures
ob <- policies[policies$Policy=='Closed.casinos' | policies$Policy=='Closed.gyms' 
               | policies$Policy=='Closed.other.non.essential.businesses',]
ob <- ob[order(ob$State),]
obsplit <- split(ob, ob$State)
obvec <- c()
statevec1 <- c()
for(i in 1:length(obsplit))
{
  statevec1 <- c(statevec1, obsplit[[i]]$State[1])
  obvec <- c(obvec, min(obsplit[[i]]$Date))
}
ob <- data.frame(State=statevec1, Date=obvec)
ob <- merge(ob, states, by='State', all=TRUE)
obvec <- as.Date(ob$Date)

#Updates as of 10/10/2022
#added zed (z standardized days from first case to ed)
zed = (days_to_ed-mean(days_to_ed))/sd(days_to_ed)
#added ed2 (number of days from emergency declaration to 3/17, 
#first day of study period)
ed2 <- as.numeric(rep(as.Date('2020-03-17'))-edvec)
#End 10/10/2022 updates

for(i in 1:length(casesbystate))
{
   for(j in 2:length(casesbystate[[i]]$Z_t))
   {
     is.na(casesbystate[[i]]$'W_t'[1]) = TRUE
     if(casesbystate[[i]]$Z_t[j-1] == 0 || is.na(casesbystate[[i]]$Z_t[j-1])) {
       is.na(casesbystate[[i]]$'W_t'[j]) = TRUE
     }
     else {
       casesbystate[[i]]$'W_t'[j] <- ((casesbystate[[i]]$Z_t[j])-(casesbystate[[i]]$Z_t[j-1]))/(casesbystate[[i]]$Z_t[j-1])
     }
   }
   casesbystate[[i]] <- casesbystate[[i]] %>% filter(between(date, as.Date("2020-03-13"), as.Date("2020-04-22")))
   casesbystate[[i]]$M_t <- rollmean(casesbystate[[i]]$Z_t, 7, align = "center", fill = NA)
   casesbystate[[i]]$W_t_ma <- c(NA,diff(c(casesbystate[[i]]$M_t))/casesbystate[[i]]$M_t[-(length(casesbystate[[i]]$M_t))])
   casesbystate[[i]] <- casesbystate[[i]] %>% filter(between(date, as.Date("2020-03-17"), as.Date("2020-04-19")))
   casesbystate[[i]]$date <- as.Date(casesbystate[[i]]$date)
   casesbystate[[i]] <- add_column(casesbystate[[i]], time = 1:length(casesbystate[[i]]$date), .after = 'state')
   casesbystate[[i]]$ed <- days_to_ed[i]
   casesbystate[[i]]$zed <- zed[i]
   casesbystate[[i]]$ed2 <- ed2[i]
   casesbystate[[i]]$sc <- as.integer(casesbystate[[i]]$date >= scvec[i])
   casesbystate[[i]]$sc[is.na(casesbystate[[i]]$sc)] <- 0
   casesbystate[[i]]$sh <- as.integer(casesbystate[[i]]$date >= shvec[i])
   casesbystate[[i]]$sh[is.na(casesbystate[[i]]$sh)] <- 0
   casesbystate[[i]]$rb <- as.integer(casesbystate[[i]]$date >= rbvec[i])
   casesbystate[[i]]$rb[is.na(casesbystate[[i]]$rb)] <- 0
   casesbystate[[i]]$ob <- as.integer(casesbystate[[i]]$date >= obvec[i])
   casesbystate[[i]]$ob[is.na(casesbystate[[i]]$ob)] <- 0
   casesbystate[[i]]$old <- casesbystate[[i]]$sc + casesbystate[[i]]$sh +
                                casesbystate[[i]]$rb + casesbystate[[i]]$ob
   casesbystate[[i]]$meanold <- rep(mean(casesbystate[[i]]$old), length(casesbystate[[i]]$old))
   casesbystate[[i]]$cold <- casesbystate[[i]]$old - casesbystate[[i]]$meanold
   
}

race_data <- read.csv('race_raw.csv')[1:52]


for(i in 1:length(casesbystate))
{
  casesbystate[[i]]$white <- rep(readr::parse_number(race_data[[i+1]][2])/100, length(casesbystate[[i]]$date))
}

for(i in 1:length(casesbystate))
{
  casesbystate[[i]]$black <- rep(readr::parse_number(race_data[[i+1]][3])/100, length(casesbystate[[i]]$date))
}

for(i in 1:length(casesbystate))
{
  casesbystate[[i]]$asian <- rep(readr::parse_number(race_data[[i+1]][4])/100, length(casesbystate[[i]]$date))
}

for(i in 1:length(casesbystate))
{
  casesbystate[[i]]$hispanic <- rep(readr::parse_number(race_data[[i+1]][1])/100, length(casesbystate[[i]]$date))
}


casesbystatedf <- do.call("rbind", casesbystate)
#with_w_t <- casesbystatedf
#casesbystatedf = subset(casesbystatedf, select = -c(W_t))


#WRITE CSV
write.csv(casesbystatedf,'cases_policies_race.csv',row.names=FALSE)














#REGULAR VERSION
library(zoo)
par(mfrow=c(6,9), mar = c(2, 2, 1.5, 1.5))
for(i in 1:51) {
  plot(x=casesbystate[[i]]$date, y=casesbystate[[i]]$W_t, xlab='',
       ylab='', main=casesbystate[[i]]$state[1],type='o')
  lines(x=casesbystate[[i]]$date,y=rollmean(casesbystate[[i]]$W_t,7,fill=NA),col='blue')
}


#TS VERSION
par(mfrow=c(6,9), mar = c(2, 2, 1.5, 1.5))
for(i in 1:51) {
  plot(zoo(casesbystate[[i]]$W_t,seq(as.Date("2020-03-18"), as.Date("2020-04-19"), by = "days")),
       ylab='', main=casesbystate[[i]]$state[1],type='o')
  lines(x=casesbystate[[i]]$date,y=rollmean(casesbystate[[i]]$W_t,7,fill=NA),col='blue')
}



# library(lme4)
# #No fixed effect, random intercept ML
# no_fixed_ml_lme4 <- lmer(W_t ~ 1 + (1|state), data=casesbystatedf, REML=F)
# #No fixed effect, random intercept REML
# no_fixed_reml_lme4 <- lmer(W_t ~ 1 + (1|state), data=casesbystatedf, REML=T)
# 
# #Fixed effect, random intercept ML
# random_int_ml_lme4 <- lmer(W_t ~ time + (1|state), data=casesbystatedf, REML=F)
# #Fixed effect, random intercept REML
# random_int_reml_lme4 <- lmer(W_t ~ time + (1|state), data=casesbystatedf, REML=T)
# 
# #Random slope
# rand_slope <- lmer(W_t ~ time + (0+time|state), data=casesbystatedf, REML=T)
# 
# 
# #Random intercept random slope ML
# random_int_slope_ml_lme4 <- lmer(W_t ~ time + (1+time|state), data=casesbystatedf, REML=F)
# #Random intercept random slope REML
# random_int_slope_reml_lme4 <- lmer(W_t ~ time + (1+time|state), data=casesbystatedf, REML=T)
# 
# #> null.model1<-lmer(W_t~1+(1|state), data=casesbystatedf ,REML=F)
# #> anova(null.model1, random_int_ml_lme4)
# 
# 
# library(nlme)
# #No fixed effect, random intercept ML
# no_fixed_ml_nlme <-lme(W_t ~ 1, random = ~ 1 | state, data=casesbystatedf, method='ML')
# #No fixed effect, random intercept REML
# no_fixed_reml_nlme <-lme(W_t ~ 1, random = ~ 1 | state, data=casesbystatedf)
# 
# 
# #Fixed effect, random intercept ML
# random_int_ml_nlme <- lme(W_t ~ time, random = ~ 1 | state, data=casesbystatedf, method='ML')
# #Fixed effect, random intercept REML
# random_int_reml_nlme <- lme(W_t ~ time, random = ~ 1 |state, data=casesbystatedf)
# 
# 
# #Random intercept random slope ML
# random_int_slope_ml_nlme <- lme(W_t ~ time, random = ~ 1+time|state, data=casesbystatedf, method='ML')
# #Random intercept random slope REML
# random_int_slope_reml_nlme <- lme(W_t ~ time, random = ~ 1+time|state, data=casesbystatedf)


#write.csv(casesbystatedf,'cases_policies_race.csv',row.names=FALSE)







#CONVERT TO WIDE FORM
# library(tidyr)
# policies <- read.csv('policies_updated.csv')
# all_policies <- read.csv('policies_updated.csv')
# #policies_updated <- read.csv('policies_updated.csv')
# policies <- subset(policies, policies$Group != "Reopening")
# wide_table <- pivot_wider(policies,
#             names_from = "Policy", 
#             values_from = 'Policy', 
#             values_fill = 0,
#             values_fn = function(x) 1)
# wide_table <- wide_table[order(wide_table$State, wide_table$Date),]
# 
# casesbystate <- do.call("rbind", casesbystate)
# merge <- merge(casesbystate, wide_table, by.x=c('state', 'date'), by.y=c('State', 'Date'), all=TRUE)
# library(tibble)
# merge <- add_column(merge, state.abbr = state.abb[match(merge$state, state.name)], .after = 1)
# merge[c('State.Abbreviation', 'Group')] = NULL
# 
# 
# #Fill NAs in column 8 though 16 with 0
# merge[8:length(merge)][is.na(merge[8:length(merge)])] <- 0
# bystate <- split(merge, merge$state)
# Y_i <- c()
# for(i in 1:length(bystate))
# {
#   subset <- subset(bystate[[i]], (bystate[[i]]$date > as.Date('2020-03-17')) &
#                                                  (bystate[[i]]$date < as.Date('2020-04-20')))
#   Y_i <- c(Y_i, mean(subset$W_t))
#   
# }
# 
# bystatedf <- do.call("rbind", bystate)
# 
# firstcase <- c()
# statecase <- c()
# for(i in 1:length(bystate)) {
#   firstcase <- c(as.Date(firstcase),bystate[[i]]$date[which(!is.na(bystate[[i]]$W_t))[1]])
#   statecase <- c(statecase, bystate[[i]]$state[1])
# }
# latestfirstcase <- as.Date(max(firstcase))
# statecase <- statecase[which(firstcase == latestfirstcase)]
# 
# 
# march15 <- casesbystate[casesbystate$date == as.Date('2020-03-15'), ]
# summary(march15$W_t)
# hist(march15$W_t)
# 
# april19 <- casesbystate[casesbystate$date == as.Date('2020-04-19'), ]
# summary(april19$W_t)
# hist(april19$W_t)

# #ADD LOCKDOWN 1
# merge <- add_column(merge, Lockdown1 = apply(merge[which(colnames(merge)=='State.of.emergency.issued'):length(merge)], 1, max), .after = 7)
# #ADD LOCKDOWN 3
# merge <- add_column(merge, Lockdown3 = apply(merge[which(colnames(merge)=='State.of.emergency.issued'):length(merge)], 1, sum), .after = 'Lockdown1')
# #SPLIT BY STATE
# bystate <- split(merge, merge$state)
# #ADD LOCKDOWN 2 and 4
# for(i in 1:length(bystate))
# {
#   # newcol2 <- bystate[[i]] %>% select(Lockdown1) %>% mutate(Lockdown2 = cumsum(Lockdown1))
#   # newcol2 <- newcol2[2] 
#   bystate[[i]] <- add_column(bystate[[i]], Lockdown2 = cumsum(bystate[[i]]$Lockdown1), .after = 'Lockdown1')
#   # newcol4 <- bystate[[i]] %>% select(Lockdown3) %>% mutate(Lockdown4 = cumsum(Lockdown3))
#   # newcol4 <- newcol4[2] 
#   bystate[[i]] <- add_column(bystate[[i]], Lockdown4 = cumsum(bystate[[i]]$Lockdown3), .after = 'Lockdown3')
# }

# for(i in 1:length(bystate))
# {
#   bystate[[i]] <- subset(bystate[[i]], 
#                          (bystate[[i]]$date > as.Date('2020-03-17')) & 
#                            (bystate[[i]]$date < as.Date('2020-04-21')))
# }

#combine into 1 df
#bystatedf <- do.call("rbind", bystate)
#n = 34
# split <- split(policies, policies$State)
# X_i1 <- c()
# for(i in 1:length(split))
# {
#   sub1 <- subset(split[[i]], split[[i]]$Policy=='State.of.emergency.issued')
#   if(as.Date(sub1$Date) > as.Date('2020-03-18')) {
#     X_i1 <- c(X_i1, as.Date('2020-04-20')-as.Date(sub1$Date))
#   }
#   else
#     X_i1 <- c(X_i1, 33)
# }
# X_ij <- data.frame(State = names(split), Y_i = Y_i)
# X_ij$X_i1 <- X_i1
# 
# X_i2 <- c()
# for(i in 1:length(split))
# {
#   vec <- split[[i]]$Policy=='Closed.K.12.public.schools'
#   if(TRUE %in% vec)
#   {
#     sub2 <- subset(split[[i]], split[[i]]$Policy=='Closed.K.12.public.schools')
#     if(as.Date(sub2$Date) > as.Date('2020-03-18')) {
#       X_i2 <- c(X_i2, as.Date('2020-04-20')-as.Date(sub2$Date))
#     }
#     else
#       X_i2 <- c(X_i2, 33)
#   }
#   else
#     X_i2 <- c(X_i2, 0)
# }
# X_ij$X_i2 <- X_i2
# 
# 
# X_i3 <- c()
# for(i in 1:length(split))
# {
#   vec <- split[[i]]$Policy=='Stay.at.home.shelter.in.place'
#   if(TRUE %in% vec)
#   {
#     sub3 <- subset(split[[i]], split[[i]]$Policy=='Stay.at.home.shelter.in.place')
#     if(as.Date(sub3$Date) > as.Date('2020-03-18')) {
#       X_i3 <- c(X_i3, as.Date('2020-04-20')-as.Date(sub3$Date))
#     }
#     else
#       X_i3 <- c(X_i3, 33)
#   }
#   else
#     X_i3 <- c(X_i3, 0)
# }
# X_ij$X_i3 <- X_i3
# 
# 
# X_i4 <- c()
# for(i in 1:length(split))
# {
#   vec <- split[[i]]$Policy=='Closed.restaurants' | split[[i]]$Policy=='Closed.bars'
#   if(TRUE %in% vec)
#   {
#     sub4 <- subset(split[[i]], split[[i]]$Policy=='Closed.restaurants' | split[[i]]$Policy=='Closed.bars')
#     if(min(as.Date(sub4$Date)) > as.Date('2020-03-18')) {
#       X_i4 <- c(X_i4, as.Date('2020-04-20')-min(as.Date(sub4$Date)))
#     }
#     else
#       X_i4 <- c(X_i4, 33)
#   }
#   else
#     X_i4 <- c(X_i4, 0)
# }
# X_ij$X_i4 <- X_i4
# 
# 
# 
# X_i5 <- c()
# for(i in 1:length(split))
# {
#   vec <- split[[i]]$Policy=='Closed.other.non.essential.businesses' | split[[i]]$Policy=='Closed.gyms' |
#     split[[i]]$Policy=='Closed.movie.theaters' | split[[i]]$Policy=='Closed.casinos'
#   if(TRUE %in% vec)
#   {
#     sub5 <- subset(split[[i]], split[[i]]$Policy=='Closed.other.non.essential.businesses' | split[[i]]$Policy=='Closed.gyms' |
#                      split[[i]]$Policy=='Closed.movie.theaters' | split[[i]]$Policy=='Closed.casinos')
#     if(min(as.Date(sub5$Date)) > as.Date('2020-03-18')) {
#       X_i5 <- c(X_i5, as.Date('2020-04-20')-min(as.Date(sub5$Date)))
#     }
#     else
#       X_i5 <- c(X_i5, 33)
#   }
#   else
#     X_i5 <- c(X_i5, 0)
# }
# X_ij$X_i5 <- X_i5
# 
# 
# 
# corr1 <- cor.test(x=X_i1, y=Y_i,  method = "pearson")
# plot(x=X_i1, y=Y_i)
# corr2 <- cor.test(x=X_i2, y=Y_i,  method = "pearson")
# plot(x=X_i2, y=Y_i)
# corr3 <- cor.test(x=X_i3, y=Y_i,  method = "pearson")
# plot(x=X_i3, y=Y_i)
# corr4 <- cor.test(x=X_i4, y=Y_i,  method = "pearson")
# plot(x=X_i4, y=Y_i)
# corr5 <- cor.test(x=X_i5, y=Y_i,  method = "pearson")
# plot(x=X_i5, y=Y_i)
# #p value 0.7
# corr25 <- cor.test(x=X_i2, y=X_i5,  method = "pearson")
# plot(x=X_i2, y=X_i5)
# #p value 0.01
# corr35 <- cor.test(x=X_i3, y=X_i5,  method = "pearson")
# plot(x=X_i3, y=X_i5)
# #p value very small
# corr45 <- cor.test(x=X_i4, y=X_i5,  method = "pearson")
# plot(x=X_i4, y=X_i5)
# #p value 0.7
# corr24 <- cor.test(x=X_i2, y=X_i4,  method = "pearson")
# plot(x=X_i2, y=X_i4)
# #p value 0.4
# corr23 <- cor.test(x=X_i2, y=X_i3,  method = "pearson")
# plot(x=X_i2, y=X_i3)
# #p value 0.004
# corr34 <- cor.test(x=X_i3, y=X_i4,  method = "pearson")
# plot(x=X_i3, y=X_i4)