library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("reshape2")

cases_TX <- read_csv("COVID-19_cases_TX.csv")
cases_TX <- cases_TX %>% mutate_if(is.character, factor)
                                   
cases_US <- read_csv("Covid_Cases_US_Timeline.csv")
cases_US <- cases_US %>% mutate_if(is.character, factor)

ggplot(cases_TX, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()
ggplot(cases_US, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()


cases_confirmed_by_county <- cases_TX %>% select(confirmed_cases,county_name, date) %>%
  drop_na() %>% group_by(county_name, date) %>% summarize(cases_per_day = c(confirmed_cases[1],diff(confirmed_cases)) )


#cumulative
ggplot(cases_confirmed_by_county, aes(x = date, y = cases_per_day)) +
 geom_line(aes(color = county_name, linetype = county_name), show.legend = FALSE  ) +
 geom_smooth(aes(color = county_name, linetype = county_name), show.legend = FALSE, se=FALSE)+
 ylim(0,100)

#first convert from long to wide representation, apply diff on each column then melts back to long form, 
#TODO must get dates labels back

cases_confirmed_by_county <- cases_TX %>% select(county_name, date, confirmed_cases) %>%
 group_by(county_name, date) %>% dcast(date ~ county_name,value.var="confirmed_cases")  %>% select(-date)%>%sapply( diff, lag=1) %>%
  melt(id.vars = c("date", "county_name"), measure.vars = c("cases_per_day")) %>%   drop_na()


ggplot(cases_confirmed_by_county, aes(x = Var1, y = value)) +
 # geom_line(aes(color = Var2, linetype = Var2), show.legend = FALSE  ) +
 geom_smooth(aes(color = Var2, linetype = Var2), show.legend = FALSE, se=FALSE) + ylim(0,20)
