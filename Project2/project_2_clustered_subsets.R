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
library(dbscan)
library(cluster)
# library(fpc)

#select a subset and run the code below

#all variable clustering
cases_select_scaled <- cases_select %>% keep(is.numeric) %>% scale()

#impact of health on death and deaths per cases
cases_select_scaled <- cases_select %>% keep(is.numeric) %>% scale() %>% as.tibble %>%
  select(deaths_per_1000,death_per_case,icu_bed_pop,primary_care_1000,median_age)

#impact of demographics on death and deaths per cases
selection_demographics <- cases_select %>% keep(is.numeric) %>% scale() %>%
 as.tibble %>% select(cases_per_1000,deaths_per_1000,percent_black,unemployment_rate,population_density,median_income)








ks <- 2:15



#selecting cluster number

#silouete
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(cases_select_scaled, centers=k, nstart=5)$cluster)$avg.silwidth
  })
#choosing optimal number of clusters
best_k <- ks[which.max(ASW)]
ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = 2, color = "red", linetype = 2)


#dun index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(cases_select_scaled, centers=k, nstart=5)$cluster)$dunn
})
best_k <- ks[which.max(DI)]
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)


#gap statistic
k <- clusGap(cases_select_scaled, FUN = kmeans,  nstart = 10, K.max = 10)
plot(k)





#kmeans
km <- kmeans(cases_select_scaled, centers = 2)
#internel cluster validation
fpc::cluster.stats(d, km$cluster)
#visualize cluster charactersitics
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"),
  cols = colnames(km$centers)),
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))
#sillouete for kmeans
d <- dist(cases_select_scaled)
plot(silhouette(km$cluster, d))
# all usa
counties <- as_tibble(map_data("county"))
cases_select <- cases_select %>% mutate(county = county_name %>%
  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
us_counties <- counties %>%
  rename(c(county = subregion))
#visualize map
us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(km$cluster)))
ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")

#see texas zoomed
counties_TX <- counties %>% dplyr::filter(region == "texas") %>%
  rename(c(county = subregion))

us_counties_clust <- counties_TX %>% left_join(cases_select %>%
    add_column(cluster = factor(km$cluster)))
ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")



#hierarcal clustering
d <- dist(cases_select_scaled)
hc <- hclust(d)
cl <- cutree(hc, k = 12)
#internal cluster validation
fpc::cluster.stats(d, hc$cluster)
#visualize map
us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(cl)))
ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")
#texas
us_counties_clust <- counties_TX %>% left_join(cases_select %>%
    add_column(cluster = factor(cl)))
ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")




#dbscan clustering
kNNdistplot(cases_select_scaled, k = 10)
abline(h=3, col="red")

db <- dbscan(cases_select_scaled, eps=3)
db

#internel cluster validation
fpc::cluster.stats(d, db$cluster)

#visualize map
us_counties_clust <- us_counties %>% left_join(cases_select %>%
    add_column(cluster = factor(db$cluster)))
ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")

us_counties_clust <- counties_TX %>% left_join(cases_select %>%
    add_column(cluster = factor(db$cluster)))
ggplot(us_counties_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")


library(seriation)

pimage(d)
dissplot(d, labels = km$cluster, options=list(main="k-means with k=2"),col = bluered(100,bias = 0.1))
dissplot(d, labels = cl, options=list(main="Hierarchal with 12"),col = greenred(100,bias = 0.1))

pimage(d, order=order(db$cluster), col = bluered(100,bias = 0.1))

library(factoextra)

pca_res <- prcomp(cases_select_scaled, scale. = TRUE)

library(ggfortify)
autoplot(pca_res)
