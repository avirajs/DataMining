library("tidyverse")
library("ggplot2")
library("DT")
library("plyr")
library(rpart)
library("rjava")


cases_current <- read.csv("COVID-19_cases_plus_census.csv")
cases_current <- cases_current %>% mutate_if(is.character, factor)
cases_current$date %>% unique
dim(cases_current)


# SELECT *
# FROM `bigquery-public-data.covid19_usafacts.summary` covid19
# JOIN `bigquery-public-data.census_bureau_acs.county_2017_5yr` acs
# ON covid19.county_fips_code = acs.geo_id
# WHERE date = DATE_SUB(CURRENT_DATE(), INTERVAL 3 MONTH)


cases_old <- read.csv("COVID-19_cases_plus_census_enddate.csv")
cases_old <- cases_old %>% mutate_if(is.character, factor)
cases_old$date %>% unique

#make predicive variables death and cases across time
delta_columns <- cases_current[6:7] - cases_old[6:7]
delta_columns
delta_columns <- delta_columns %>% dplyr::rename(cases_delta = confirmed_cases, deaths_delta = deaths )
cases <- rbind.fill(cases_current, delta_columns)


# joining data sets for population density and health care
pop_dense_county <- read_csv("Population-Density_By_County.csv")  %>% mutate_if(is.character, factor)
cases <- cases %>% left_join(pop_dense_county,by = c("county_fips_code"= "GCT_STUB.target-geo-id2" ))
dim(cases)

healthcare_county <- read_csv("healthcare_services_per_county.csv")  %>% mutate_if(is.character, factor)
cases <- cases %>% left_join(healthcare_county,by = c("county_fips_code"= "FIPS" ))
dim(cases)

cases %>% colnames










cases_pop_scaled <- cases %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  cases_delta_per_1000 = confirmed_cases/total_pop*1000,
  deaths_delta_per_1000 = deaths/total_pop*1000,
  death_per_case = deaths/confirmed_cases,
  percent_black = black_pop/total_pop,
  percent_male = male_pop/total_pop,
  unemployment_rate = unemployed_pop/total_pop,
  population_density = `Density per square mile of land area`,
  percent_commuters = `commuters_16_over`/total_pop,
  icu_bed_pop = `ICU Beds`/total_pop,
  primary_care_1000 = `Active Primary Care Physicians per 100,000 Population, 2018 (AAMC)`,
  commuters_by_public_transportation = commuters_by_public_transportation/ total_pop,
  households = households / total_pop,
  housing_units = housing_units / total_pop,
  vacant_housing_units = vacant_housing_units / total_pop,
  in_school = in_school / total_pop,
  in_undergrad_college = in_undergrad_college / total_pop
)

cases_select <- cases_pop_scaled %>% filter(confirmed_cases > 10) %>%
  arrange(desc(confirmed_cases)) %>%
  select(county_name,cases_per_1000,deaths_per_1000,cases_delta_per_1000,deaths_delta_per_1000,
    death_per_case,percent_black,percent_male,unemployment_rate,population_density,
    percent_commuters,icu_bed_pop,primary_care_1000,commuters_by_public_transportation,
    households,housing_units,vacant_housing_units,
    in_school,in_undergrad_college,in_undergrad_college,
    median_income,median_age)


#clean na
which(is.na(cases_select), arr.ind=TRUE)
cases_select[1487,]
cases_select <- na.omit(cases_select)







cases_select <- cases_select %>% mutate(
  cases_per_1000_levels =  as.factor(cut_number(cases_select$cases_delta_per_1000,3)),
  deaths_per_1000_levels = as.factor(cut_number(cases_select$deaths_delta_per_1000,3))
)


#all variable scaled
cases_select_scaled <- cases_select %>% mutate_if(is.numeric,scale)



cases_select_scaled %>% group_by(deaths_per_1000_levels) %>% tally()
cases_select_scaled %>% group_by(cases_per_1000_levels) %>% tally()






#select valuable features only

library(FSelector)

death_weights <- cases_select_scaled %>%  chi.squared(deaths_per_1000_levels ~ ., data = .) %>%
 arrange(desc(attr_importance))

case_weights <- cases_select_scaled %>%  chi.squared(cases_per_1000_levels ~ ., data = .) %>%
 arrange(desc(attr_importance) )%>% head()


ggplot(as_tibble(death_weights, rownames = "feature"),
       aes(x = attr_importance, y = reorder(feature, attr_importance))) +
  geom_bar(stat = "identity") +
  xlab("Importance score") + ylab("Feature")



#tree model
cases_select_tree <- cases_select


cases_select_tree <- cases_select_tree%>% select(-deaths_per_1000, -cases_per_1000, -death_per_case)
cases_select_tree <- cases_select_tree%>% select(-deaths_delta_per_1000,-cases_delta_per_1000)



cases_select_tree %>%  chi.squared(deaths_per_1000_levels ~ ., data = .) %>%
  arrange(desc(attr_importance) )%>% head()

cases_select_tree %>%  chi.squared(cases_per_1000_levels ~ ., data = .) %>%
  arrange(desc(attr_importance) )%>% head()



library(caret)


library(doParallel)
registerDoParallel()
getDoParWorkers()



fit <- cases_select_tree %>%
  train(deaths_per_1000_levels ~ . - county_name,
        data = . ,
        method = "rpart",
        control = rpart.control(minsplit = 2),
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 5
  )
fit

library(rpart.plot)

rpart.plot(fit$finalModel, extra = 2)

varImp(fit)

library(sampling)
id <- strata(cases_select_tree, stratanames = "high_risk", size = c(50, 50), method = "srswr")
cases_select_tree_balanced <- cases_select_tree%>% slice(id$ID_unit)

fit_balanced <- cases_select_tree_balanced %>%
  train(high_risk ~ . - county_name,
        data = . ,
        method = "rpart",
        control = rpart.control(minsplit = 2),
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 5
  )
fit_balanced


rpart.plot(fit_balanced$finalModel, extra = 2)

varImp(fit_balanced)




library(keras)
model <- keras_model_sequential()

model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% summary


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)
