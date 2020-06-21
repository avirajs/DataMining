library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")

cases_TX <- read_csv("COVID-19_cases_TX.csv")
cases_TX <- cases_TX %>% mutate_if(is.character, factor)
                                   
cases_US <- read_csv("Covid_Cases_US_Timeline.csv")
cases_US <- cases_US %>% mutate_if(is.character, factor)

ggplot(cases_TX, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()
ggplot(cases_US, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()


cases_confirmed_by_county <- cases_TX %>% select(confirmed_cases,county_name, date) %>%
  drop_na() %>% group_by(county_name, date) %>% summarize(cases_per_day = c(confirmed_cases[1],diff(confirmed_cases)) )

ggplot(cases_confirmed_by_county, aes(x = date, y = cases_per_day)) + geom_line(aes(color = county_name, linetype = county_name),  show.legend = FALSE)
