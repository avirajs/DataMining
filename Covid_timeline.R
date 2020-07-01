library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("reshape2")
library("plotly")
library("hrbrthemes")
library(directlabels)
# Source: https://www.google.com/covid19/mobility/index.html

cases_US <- read_csv("COVID-19_Cases_Timeline_US.csv")
cases_US <- cases_US %>% mutate_if(is.character, factor)

cases_TX <- cases_US %>% filter(state == "TX")
cases_TX <- cases_TX %>% mutate_if(is.character, factor)



ggplot(cases_TX, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()
ggplot(cases_US, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()

#Confirmed cases per day
cases_confirmed_by_county_cum <- cases_TX %>% select(confirmed_cases,county_name, date) %>%
  drop_na() %>% group_by(county_name, date) %>% summarize(cases_sum = sum(confirmed_cases) )

cases_confirmed_by_state_cum <- cases_US %>% select(confirmed_cases,state, date) %>%
  drop_na() %>% group_by(state, date) %>% summarize(cases_sum = sum(confirmed_cases) )

#deaths per day
deaths_by_county_cum <- cases_TX %>% select(deaths,county_name, date) %>%
  drop_na() %>% group_by(county_name, date) %>% summarize(deaths_sum = sum(deaths) )

deaths_by_state_cum <- cases_US %>% select(deaths,state, date) %>%
  drop_na() %>% group_by(state, date) %>% summarize(deaths_sum = sum(deaths) )




#first convert from long to wide representation, apply diff on each column then melts back to long form, 
#TODO must get dates labels back

cases_confirmed_by_county_per_day <- cases_confirmed_by_county_cum %>% select(county_name, date, cases_sum) %>%
  group_by(county_name, date) %>% dcast(date ~ county_name,value.var="cases_sum")  %>% select(-date)%>%sapply( diff, lag=1) %>%
  melt(id.vars = c("date", "county_name"), measure.vars = c("cases_per_day")) %>%   drop_na()

cases_confirmed_by_state_per_day <- cases_confirmed_by_state_cum %>% select(state, date,cases_sum)%>%
  group_by(state, date) %>% dcast(date ~ state,value.var="cases_sum")  %>% select(-date)%>%sapply( diff, lag=1) %>%
  melt(id.vars = c("date", "state"), measure.vars = c("cases_per_day")) %>%   drop_na()

deaths_by_state_per_day <- deaths_by_state_cum %>% select(state, date,deaths_sum)%>%
  group_by(state, date) %>% dcast(date ~ state,value.var="deaths_sum")  %>% select(-date)%>%sapply( diff, lag=1) %>%
  melt(id.vars = c("date", "state"), measure.vars = c("deaths_per_day")) %>%   drop_na()

deaths_by_county_per_day <- deaths_by_county_cum %>% select(county_name, date,deaths_sum)%>%
  group_by(county_name, date) %>% dcast(date ~ county_name,value.var="deaths_sum")  %>% select(-date)%>%sapply( diff, lag=1) %>%
  melt(id.vars = c("date", "county_name"), measure.vars = c("deaths_per_day")) %>%   drop_na()


#Created a sequence of dates from the start of the data
cases_dates <- seq(as.Date("2020/01/23"), as.Date("2020/06/23"), "day" )


#Created a new column called Dates and repeat the sequence for each county
cases_confirmed_by_county_per_day <- cases_confirmed_by_county_per_day %>% mutate(
  Dates = rep(cases_dates,times = 255)
)

cases_confirmed_by_state_per_day <- cases_confirmed_by_state_per_day %>% mutate(
  Dates = rep(cases_dates,times = 51)
)

deaths_by_state_per_day <- deaths_by_state_per_day %>% mutate(
  Dates = rep(cases_dates,times = 51)
)
deaths_by_county_per_day <- deaths_by_county_per_day %>% mutate(
  Dates = rep(cases_dates,times = 255)
)

#cumulative
ggplot(cases_confirmed_by_county_cum, aes(x = date, y = cases_sum)) +
  geom_line(aes(color = county_name, linetype = county_name), show.legend = FALSE  ) +
  #geom_smooth(aes(color = county_name, linetype = county_name), show.legend = FALSE, se=FALSE)+
  #geom_dl(aes(label = county_name, color = county_name), method = list(dl.combine("first.points", "last.points"),cex = 0.5))  + 
  ggtitle("Number of cases overall by county: Texas") + coord_cartesian(ylim=c(-5,400))  + lims(x= c( Sys.Date() - 120, Sys.Date() -10))

ggplot(cases_confirmed_by_state_cum, aes(x = date, y = cases_sum)) +
  #geom_line(aes(color = state, linetype = state), show.legend = FALSE  ) +
  geom_smooth(aes(color = state, linetype = state), show.legend = FALSE, se=FALSE)+
  geom_dl(aes(label = state, color = state), method = list('last.qp', cex = .5))  + ggtitle("Number of cases overall by state: US")+ coord_cartesian(ylim=c(-5,70000))  + lims(x= c( Sys.Date() - 120, Sys.Date() -10))

ggplot(deaths_by_county_cum, aes(x = date, y = deaths_sum)) +
  geom_line(aes(color = county_name, linetype = county_name), show.legend = FALSE  ) +
  #geom_smooth(aes(color = county_name, linetype = county_name), show.legend = FALSE, se=FALSE)+
  #geom_dl(aes(label = county_name, color = county_name), method = list(dl.combine("first.points", "last.points"),cex = 0.5))  + 
  ggtitle("Number of deaths overall by county: Texas") + coord_cartesian(ylim=c(-1,200)) + lims(x= c( Sys.Date() -120, Sys.Date() -10))

ggplot(deaths_by_state_cum, aes(x = date, y = deaths_sum)) +
  geom_line(aes(color = state, linetype = state), show.legend = FALSE  ) +
  geom_smooth(aes(color = state, linetype = state), show.legend = FALSE, se=FALSE)+
  geom_dl(aes(label = state, color = state), method = list('last.qp', cex = .5))  + ggtitle("Number of deaths overall by state: US") + coord_cartesian(ylim=c(-5,5000)) + lims(x= c( Sys.Date() - 120, Sys.Date() -10))




#per day
ggplot(cases_confirmed_by_county_per_day, aes(x = Dates, y = value)) +
  #geom_line(aes(color = Var2, linetype = Var2), show.legend = FALSE  ) +
  geom_smooth(aes(color = Var2, linetype = Var2), show.legend = FALSE, se=FALSE) +
  geom_dl(aes(label = Var2, color = Var2), method = list(dl.combine("first.points", "last.points"),cex = 0.5))  + 
  ggtitle("Number of cases per day by county: Texas") + coord_cartesian(ylim=c(-1,40)) + lims(x= c( Sys.Date() - 120, Sys.Date() -10))

ggplot(cases_confirmed_by_state_per_day, aes(x = Dates, y = value)) +
  geom_smooth(aes(color = Var2, linetype = Var2), show.legend = FALSE, se =FALSE  )+
  # geom_line(aes(color = Var2, linetype = Var2), show.legend = FALSE  ) + 
  geom_dl(aes(label = Var2, color = Var2), method = list('last.qp', cex = .5)) +  ggtitle("Number of cases per day by state: US")  + coord_cartesian(ylim=c(-1,3500)) + lims(x= c( Sys.Date() - 120, Sys.Date() -10))

dim(deaths_by_county_per_day)
ggplot(deaths_by_county_per_day, aes(x = Dates, y = value)) +
  #geom_smooth(aes(color = Var2, linetype = Var2), show.legend = FALSE, se =FALSE  )+
  geom_line(aes(color = Var2, linetype = Var2), show.legend = FALSE  ) + 
  #geom_dl(aes(label = Var2, color = Var2), method = list(dl.combine("first.points", "last.points"),cex = 0.5)) +  
  ggtitle("Number of deaths per day by county: Texas")  + coord_cartesian(ylim=c(-1,5)) + lims(x= c( Sys.Date() - 120, Sys.Date() -10))

ggplot(deaths_by_state_per_day, aes(x = Dates, y = value)) +
  geom_smooth(aes(color = Var2, linetype = Var2), show.legend = FALSE, se =FALSE  )+
  #geom_line(aes(color = Var2, linetype = Var2), show.legend = FALSE  ) + 
  geom_dl(aes(label = Var2, color = Var2), method = list('last.qp', cex = .5)) +  ggtitle("Number of deaths per day by state: US")  + coord_cartesian(ylim=c(-1,100)) + lims(x= c( Sys.Date() - 120, Sys.Date() -10))

