library(lubridate)
library(dplyr)
policies <- read.csv("C:/Users/Anna Barefield/Thesis/COVID-19 US state policy database (CUSP)/policies.csv")
split <- split(policies, policies$Group)

df <- data.frame(table(split$Lockdown$Date))
df$Freq = cumsum(df$Freq)
cumlockdowns <- df
colnames(cumlockdowns) = c('Date', 'Number of Lockdown Policies')

df <- data.frame(table(split$Reopening$Date))
df$Freq = cumsum(df$Freq)
cumreopenings <- df
colnames(cumreopenings) = c('Date', 'Total Number of Reopening Policies')

merge <- merge(cumlockdowns,cumreopenings, by='Date', all=TRUE)
merge$Date <- as.Date(merge$Date)
merge <- dplyr::arrange(merge, Date)
merge$`Number of Lockdown Policies`[36:41] <- 413
merge$`Number of Lockdown Policies`[43:92] <- 414
merge$`Total Number of Reopening Policies`[1:35] <- 0
merge$'Total Number of Lockdown Policies' <- merge$`Number of Lockdown Policies`- merge$`Total Number of Reopening Policies`
merge$`Number of Lockdown Policies` <- NULL


plot(merge$Date, merge$`Total Number of Lockdown Policies`, type = "l", 
     col = 2, xlab = 'Date', ylab = 'Number of Policies in Place', 
     main='Number of Lockdown and Reopening Policies\n From 2/29/2020 to 10/09/2020')
lines(merge$Date, merge$`Total Number of Reopening Policies`, type = "l", 
      col = 4)
legend('topright', legend=c("Lockdown Policies", "Reopening Policies"),
       col=c("red", "blue"), lty=1, cex=0.8)
