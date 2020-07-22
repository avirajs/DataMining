library("tidyverse")
library("ggplot2")
library("DT")

cases <- read_csv("COVID-19_cases_plus_census.csv")
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)

cases <- cases %>% filter(confirmed_cases > 10) %>%
  arrange(desc(confirmed_cases)) #%>%
  #select(county_name, state, confirmed_cases, deaths, total_pop, median_income, median_age)
cases <- cases %>% mutate(
  cases_per_10000 = confirmed_cases/total_pop*10000,
  deaths_per_10000 = deaths/total_pop*10000,
  death_per_case = deaths/confirmed_cases)



cases_sel <- cases %>% select(county_name, state, total_pop,
  nonfamily_households, median_year_structure_built,
  female_pop, median_age, white_pop,
  black_pop, asian_pop, hispanic_pop, amerindian_pop,
  commuters_by_public_transportation,
  households, median_income, housing_units,
  vacant_housing_units,
  percent_income_spent_on_rent,
  employed_pop, unemployed_pop,
  in_school, in_undergrad_college,
  cases_per_10000, deaths_per_10000, death_per_case)

# normalize by population
cases_sel <- cases_sel %>% mutate(
  nonfamily_households = nonfamily_households / total_pop,
  female_pop = female_pop / total_pop,
  white_pop = white_pop / total_pop,
  black_pop = black_pop / total_pop,
  asian_pop = asian_pop / total_pop,
  hispanic_pop = hispanic_pop / total_pop,
  amerindian_pop = amerindian_pop / total_pop,
  commuters_by_public_transportation = commuters_by_public_transportation/ total_pop,
  households = households / total_pop,
  housing_units = housing_units / total_pop,
  vacant_housing_units = vacant_housing_units / total_pop,
  employed_pop = employed_pop / total_pop,
  unemployed_pop = unemployed_pop / total_pop,
  in_school = in_school / total_pop,
  in_undergrad_college = in_undergrad_college / total_pop
  )

cases_sel %>% summary()
str(cases_sel)
table(complete.cases(cases_sel))

cm <- cor(cases_sel %>% select_if(is.numeric))
library(seriation)

hmap(cm, margins = c(10,10))


cases_sel <- cases_sel %>% mutate(bad = as.factor(deaths_per_10000 > 1))
cases_sel %>% pull(bad) %>% table()

library(FSelector)
cases_sel %>%  chi.squared(bad ~ ., data = .) %>%
  arrange(desc(attr_importance)) %>% head()

# we have to remove the variable that was used to create the class variable
cases_sel <- cases_sel %>% select(-deaths_per_10000)
cases_sel %>%  chi.squared(bad ~ ., data = .) %>%
  arrange(desc(attr_importance)) %>% head()

# remove more variables
cases_sel <- cases_sel %>% select(-death_per_case, -cases_per_10000)

cases_sel %>%  chi.squared(bad ~ ., data = .) %>%
  arrange(desc(attr_importance)) %>% head()

library(caret)

fit <- cases_sel %>%
  train(bad ~ . - county_name,
    data = . ,
    method = "rpart",
    trControl = trainControl(method = "cv", number = 10)
    )
fit

library(rpart.plot)

rpart.plot(fit$finalModel, extra = 2)

varImp(fit)
