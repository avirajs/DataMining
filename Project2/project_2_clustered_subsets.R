library("tidyverse")
library("ggplot2")
library("DT")


cases <- read.csv("COVID-19_cases_plus_census.csv")
cases <- cases %>% mutate_if(is.character, factor)

dim(cases)


# joining data sets for population density and health care
pop_dense_county <- read_csv("Population-Density_By_County.csv")  %>% mutate_if(is.character, factor)
cases <- cases %>% left_join(pop_dense_county,by = c("county_fips_code"= "GCT_STUB.target-geo-id2" ))
dim(cases)

healthcare_county <- read_csv("healthcare_services_per_county.csv")  %>% mutate_if(is.character, factor)
cases <- cases %>% left_join(healthcare_county,by = c("county_fips_code"= "FIPS" ))
dim(cases)





cases_pop_scaled <- cases %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop,
  deaths_per_1000 = deaths/total_pop,
  death_per_case = deaths/confirmed_cases,
  percent_black = black_pop/total_pop,
  percent_male = male_pop/total_pop,
  unemployment_rate = unemployed_pop/total_pop,
  population_density = `Density per square mile of land area`,
  percent_commuters = `commuters_16_over`/total_pop,
  icu_bed_pop = `ICU Beds`/total_pop,
  primary_care_1000 = `Active Primary Care Physicians per 100,000 Population, 2018 (AAMC)`,

)


cases_select <- cases_pop_scaled %>% filter(confirmed_cases > 10) %>%
  arrange(desc(confirmed_cases)) %>%
  select(county_name,cases_per_1000,deaths_per_1000,death_per_case,percent_black,percent_male,unemployment_rate,population_density,
    percent_commuters,icu_bed_pop,primary_care_1000,median_income,median_age)


#clean na
which(is.na(cases_select), arr.ind=TRUE)
cases_select[1487,]
cases_select <- na.omit(cases_select)


#scale
cases_select_scaled <- cases_select %>% keep(is.numeric) %>% scale()
cor(cases_select_scaled)



#all variable clustering



#kmeans
km <- kmeans(cases_select_scaled, centers = 3)

ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"),
  cols = colnames(km$centers)),
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

counties <- as_tibble(map_data("county"))

cases_select <- cases_select %>% mutate(county = county_name %>%
  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))




# all usa
us_counties <- counties %>%
  rename(c(county = subregion))

us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(km$cluster)))

ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")





d <- dist(cases_select_scaled)
hc <- hclust(d)
# plot(hc)

cl <- cutree(hc, k = 8)

us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(cl)))

ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")






# all texas counties
# counties_TX <- counties %>% dplyr::filter(region == "texas") %>%
#   rename(c(county = subregion))








#impact of health on death and deaths per cases
selection_health <- cases_select_scaled %>% as.tibble %>% select(deaths_per_1000,death_per_case,icu_bed_pop,primary_care_1000,median_age)



km <- kmeans(selection_health, centers = 3)


ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"),
  cols = colnames(km$centers)),
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))
  us_counties <- counties %>%
    rename(c(county = subregion))

us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(km$cluster)))

ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")



counties <- as_tibble(map_data("county"))

cases_select <- cases_select %>% mutate(county = county_name %>%
str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

#hierarch clustering health
d <- dist(selection_health)
hc <- hclust(d)
# plot(hc)

cl <- cutree(hc, k = 8)

us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(cl)))

ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")












#impact of demographics on death and deaths per cases
selection_demographics <- cases_select_scaled %>% as.tibble %>% select(cases_per_1000,deaths_per_1000,percent_black,unemployment_rate,population_density,median_income)



km <- kmeans(selection_demographics, centers = 3)


ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"),
  cols = colnames(km$centers)),
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))
  us_counties <- counties %>%
    rename(c(county = subregion))

us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(km$cluster)))

ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")



counties <- as_tibble(map_data("county"))

cases_select <- cases_select %>% mutate(county = county_name %>%
str_to_lower() %>% str_replace('\\s+county\\s*$', ''))





#hierarch clustering demo
d <- dist(selection_demographics)
hc <- hclust(d)
# plot(hc)

cl <- cutree(hc, k = 8)

us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(cl)))

ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")
