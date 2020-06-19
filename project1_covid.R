library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")



cases <- read.csv("COVID-19_cases_plus_census.csv")
cases <- cases %>% mutate_if(is.character, factor)
head(cases)
dim(cases)


#subset data
cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)

#check data quality
which(duplicated(cases_TX))



#select most important features
str(cases, list.len=ncol(cases))
cases_TX_select <- cases_TX %>% select(county_name, confirmed_cases, deaths, total_pop, median_income, gini_index, black_pop, white_pop, asian_pop)



#summarize data mean median mode
summary(cases_TX_select)


#scale cases by populations
cases_TX_scaled <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop,
  deaths_per_1000 = deaths/total_pop,
  death_per_case = deaths/confirmed_cases,
  percent_black = black_pop/total_pop,
  percent_white = white_pop/total_pop,
  percent_asian = asian_pop/total_pop
)

# install.packages("fBasics")
library(fBasics)
cases_TX_scaled %>% keep(is.numeric) %>% basicStats()

#remove the nans created by new features
cases_TX_scaled[is.na(cases_TX_scaled)] <- 0


#plot all the new scaled as histograms
cases_TX_scaled %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram() + geom_density(alpha=.2, fill="#FF6666")


#plot new features in correlations
#remove non-ratio data
cor_TX <- cases_TX_scaled[,-1]%>% select(-contains("pop")) %>%
  keep(is.numeric) %>% cor

ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_scaled[,-1]), insig = "blank", hc.order = TRUE, lab = TRUE)







#create data sumns of
cases_TX %>% select(contains("pop"))



summary(cases_TX[,1:10])



ggplot(cases_TX, mapping = aes(confirmed_cases)) + geom_histogram(binwidth = 100)


ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()

ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) +
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") +
  geom_text_repel(data = subset(cases_TX, deaths >= 50))


cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>%
  arrange(desc(confirmed_cases)) %>%
  select(county_name, confirmed_cases, deaths, total_pop, median_income)

cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)


datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)


ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) +
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") +
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .75)))


ggplot(cases_TX_select, mapping = aes(x= total_pop, y = deaths_per_1000, label = county_name)) +
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") +
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .75)))

ggplot(cases_TX_select, mapping = aes(x = median_income, y = death_per_case, label = county_name)) +
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") +
  geom_text_repel(data = subset(cases_TX_select, death_per_case > quantile(death_per_case, .75)))


cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)


counties <- as_tibble(map_data("county"))

counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>%
    str_replace('\\s+county\\s*$', ''))


counties_TX <- counties_TX %>% left_join(cases_TX %>%
    select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

## Joining, by = "county"

ggplot(counties_TX, aes(long, lat, label = county)) +
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>%
    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() +
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", subtitle = "Only counties reporting 100+ cases")


# Source: https://console.cloud.google.com/marketplace/details/usafacts-public-data/covid19-us-cases?filter=solution-type:dataset&filter=category:covid19&id=3eaff9c5-fbaf-47bb-a441-89db1e1395ab

# SELECT *
# FROM `bigquery-public-data.covid19_usafacts.summary` covid19 WHERE state = "TX"

cases_TX <- read_csv("COVID-19_cases_TX.csv")


cases_TX


cases_Dallas <- cases_TX %>% filter(county_name == "Dallas County" & state == "TX")
dim(cases_Dallas)



num_cases <- diff(cases_Dallas$confirmed_cases)
append(num_cases, 0:0, after = 1)
num_deaths <- diff(cases_Dallas$deaths)



cases_confirmed_per_day <- data.frame(date = cases_Dallas$date[2:135], cases_per_day = num_cases)

deaths_per_day <- data.frame(date = cases_Dallas$date[2:135], deaths = num_deaths)

cases_Dallas <- cases_Dallas %>% mutate(confirmed_cases_per_day = num_cases)

# Graphing cumulative cases
ggplot(cases_Dallas, aes(x = date, y = confirmed_cases)) + geom_line() + geom_smooth()

ggplot(cases_Dallas, aes(x = date, y = deaths)) + geom_line() + geom_smooth()

ggplot(cases_TX, aes(x = date, y = confirmed_cases, label = county_name, group = county_name)) + geom_line() 

# Graphing cases per day
ggplot(cases_confirmed_per_day, aes(x = date, y = case_numbers)) + geom_step() + ggtitle("Number of cases per day: Dallas County")

ggplot(deaths_per_day, aes(x = date, y = deaths)) + geom_step()

x <- 1:5


# Source: https://www.google.com/covid19/mobility/index.html

mobility <- read_csv("Global_Mobility_Report.csv")


mobility <- read_csv("Global_Mobility_Report.csv", col_types =  cols(sub_region_2 = col_character()))

mobility <- mobility %>% mutate_if(is.character, factor)
dim(mobility)

head(mobility)

summary(mobility)

mobility_Dallas <- mobility %>% filter(sub_region_1 == "Texas" & sub_region_2 == "Dallas County")
dim(mobility_Dallas)


mobility_Dallas


ggplot(mobility_Dallas, mapping = aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) + geom_line() + geom_smooth()
