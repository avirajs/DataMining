library("tidyverse")
library("ggplot2")
library("DT")
library("plyr")




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
  cases_per_1000 = confirmed_cases/total_pop,
  deaths_per_1000 = deaths/total_pop,
  cases_delta_per_1000 = confirmed_cases/total_pop,
  deaths_delta_per_1000 = deaths/total_pop,
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
  select(county_name,cases_delta_per_1000,deaths_delta_per_1000,cases_delta_per_1000,deaths_delta_per_1000,
    death_per_case,percent_black,percent_male,unemployment_rate,population_density,
    percent_commuters,icu_bed_pop,primary_care_1000,commuters_by_public_transportation,
    households,housing_units,vacant_housing_units,
    in_school,in_undergrad_college,in_undergrad_college,
    median_income,median_age)


#clean na
which(is.na(cases_select), arr.ind=TRUE)
cases_select[1487,]
cases_select <- na.omit(cases_select)







#discretize to create classes
xs = quantile(cases_select$cases_delta_per_1000,c(0,1/3,2/3,1))
xs[1]=xs[1]-.00005

xs1 = quantile(cases_select$deaths_delta_per_1000,c(0,1/3,2/3,1))
xs1[1]=xs1[1]-.00005



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

cases_select_scaled %>%  chi.squared(deaths_per_1000_levels ~ ., data = .) %>%
 arrange(desc(attr_importance))

cases_select_scaled %>%  chi.squared(cases_per_1000_levels ~ ., data = .) %>%
 arrange(desc(attr_importance))







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
