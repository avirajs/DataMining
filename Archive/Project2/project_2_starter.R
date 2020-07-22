install.packages("tidyverse")

install.packages("ggplot2")
install.packages("DT")

cases <- read_csv("COVID-19_cases_plus_census.csv")



cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)

summary(cases_TX[,1:10])

cases_TX <- cases_TX %>% filter(confirmed_cases > 10) %>%
  arrange(desc(confirmed_cases)) %>%
  select(county_name, confirmed_cases, deaths, total_pop, median_income, median_age)
cases_TX <- cases_TX %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  death_per_case = deaths/confirmed_cases)

cases_TX

datatable(cases_TX) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

cases_TX_scaled <- cases_TX %>%
  select(median_income, median_age, cases_per_1000, deaths_per_1000) %>%
  scale()

cor(cases_TX_scaled)

km <- kmeans(cases_TX_scaled, centers = 3)
km

ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"),
  cols = colnames(km$centers)),
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>%
  rename(c(county = subregion))

cases_TX <- cases_TX %>% mutate(county = county_name %>%
    str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_TX_clust <- counties_TX %>% left_join(cases_TX %>%
    add_column(cluster = factor(km$cluster)))

ggplot(counties_TX_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")

d <- dist(cases_TX_scaled)

hc <- hclust(d)
plot(hc)

cl <- cutree(hc, k = 4)

counties_TX_clust <- counties_TX %>% left_join(cases_TX %>%
    add_column(cluster = factor(cl)))

ggplot(counties_TX_clust, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() +
  labs(title = "Clusters", subtitle = "Only counties reporting 10+ cases")
