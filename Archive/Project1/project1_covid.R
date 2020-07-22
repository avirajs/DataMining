library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")


# https://github.com/JieYingWu/COVID-19_US_County-level_Summaries/blob/master/raw_data/national/Density/housing_area_density_national_2010_census.csv
cases <- read.csv("COVID-19_cases_plus_census.csv")
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)

pop_dense_county <- read_csv("Population-Density_By_County.csv")  %>% mutate_if(is.character, factor)
cases <- cases %>% left_join(pop_dense_county,by = c("county_fips_code"= "GCT_STUB.target-geo-id2" ))
dim(cases)

healthcare_county <- read_csv("healthcare_services_per_county.csv")  %>% mutate_if(is.character, factor)
cases <- cases %>% left_join(healthcare_county,by = c("county_fips_code"= "FIPS" ))
dim(cases)

# install.packages('devtools')











#check data quality
which(duplicated(cases))



#select most important features
str(cases, list.len=ncol(cases))



cases_select <- cases %>% select(county_name,
 confirmed_cases, deaths, total_pop,male_pop, unemployed_pop,median_income,
   gini_index, black_pop, white_pop,
 !!as.symbol("Density per square mile of land area"),
 !!as.symbol("commuters_16_over"),
 !!as.symbol("ICU Beds"),
 !!as.symbol("Active Primary Care Physicians per 100,000 Population, 2018 (AAMC)"),

)


cases_select
#summarize data mean median mode
summary(cases_select)


#scale cases by populations
cases_scaled <- cases_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop,
  deaths_per_1000 = deaths/total_pop,
  death_per_case = deaths/confirmed_cases,
  percent_black = black_pop/total_pop,
  percent_white = white_pop/total_pop,
  percent_male = male_pop/total_pop,
  unemployment_rate = unemployed_pop/total_pop,
  population_density = `Density per square mile of land area`,
  percent_commuters = `commuters_16_over`/total_pop,
  icu_bed_pop = `ICU Beds`/total_pop,
  primary_care_1000 = `Active Primary Care Physicians per 100,000 Population, 2018 (AAMC)`,

)

cases_scaled <- cases_scaled %>% select(-`Density per square mile of land area`, -`commuters_16_over`, -`Active Primary Care Physicians per 100,000 Population, 2018 (AAMC)`)









# install.packages("fBasics")
library(fBasics)
library(kableExtra)
# install.packages("magick")
stats <- cases_scaled %>% keep(is.numeric) %>% basicStats()
stats


write.csv(stats, "variable_stats.csv")



which(is.na(cases_scaled), arr.ind=TRUE) %>% as.data.frame %>% select(col) %>% unique

cases_scaled[60,]

#remove the nans created by new features
cases_scaled[is.na(cases_scaled)] <- 0





xs = quantile(cases_scaled$cases_per_1000,c(0,1/3,2/3,1))
xs[1]=xs[1]-.00005

xs1 = quantile(cases_scaled$deaths_per_1000,c(0,1/3,2/3,1))
xs1[1]=xs1[1]-.00005

xs2 = quantile(cases_scaled$death_per_case,c(0,1/3,2/3,1))
xs2[1]=xs2[1]-.00005

cases_scaled <- cases_scaled %>% mutate(
  cases_per_1000_levels = cut(cases_per_1000,3, breaks=xs,
                        labels = c("low", "medium", "high"), ordered = TRUE),
  deaths_per_1000_levels = cut(deaths_per_1000,3, breaks=xs,
                        labels = c("low", "medium", "high"), ordered = TRUE),
  death_per_case_levels = cut(death_per_case,3, breaks=xs,
                        labels = c("low", "medium", "high"), ordered = TRUE),
)





cases_scaled
library(GGally)

options(repr.plot.width=13, repr.plot.height=5)

library(seriation)
library(viridis)

ser1 <- cases_scaled[-1] %>% select(population_density, primary_care_1000, total_pop,
gini_index, median_income, percent_commuters,unemployment_rate, percent_black, percent_white)

o <- seriate(as.dist(1-cor(ser1)), method="Spectral")
get_order(o)

cases_scaled[-1]%>% select(population_density, primary_care_1000, total_pop,
gini_index, median_income, percent_commuters,unemployment_rate, percent_black, percent_white,
    cases_per_1000_levels) %>%
    ggparcoord(  columns = get_order(o), groupColumn = "cases_per_1000_levels",
    title = "Predicting cases per 1000 (std scaled)",  scale="std",   alphaLines = 0.3)  +
  scale_color_viridis(discrete=TRUE) + ylim(-5,10)





ser1 <- cases_scaled[-1] %>% select(population_density, primary_care_1000, total_pop, icu_bed_pop,
gini_index, median_income, percent_commuters, percent_male,unemployment_rate, percent_black, percent_white)

o <- seriate(as.dist(1-cor(ser1)), method="BBURCG")
get_order(o)

cases_scaled[-1]%>% select(population_density, primary_care_1000, total_pop, icu_bed_pop,
gini_index, median_income, percent_commuters, percent_male,unemployment_rate, percent_black, percent_white,
    death_per_case_levels) %>%
    ggparcoord(  columns = get_order(o), groupColumn = "death_per_case_levels",
    title = "Predicting Deaths per Cases (std scale)",  scale="std",   alphaLines = 0.3) + ylim(-5,10)

options(repr.plot.width=8, repr.plot.height=2.5)



# %>% mutate_each(funs(scale
cases_scaled %>% select(deaths_per_1000, cases_per_1000, death_per_case) %>%
  keep(is.numeric)  %>%
  gather() %>%
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins=50) + scale_y_continuous(trans='log10') +
    xlab("number")  + ylab("Counts logscaled") + ggtitle("Variables for Prediction", subtitle = waiver())

options(repr.plot.width=5, repr.plot.height=4)

cases_scaled %>% select(population_density, primary_care_1000, total_pop, icu_bed_pop)%>%
  keep(is.numeric)  %>%
  gather() %>%
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins=40) +
    scale_y_continuous(trans='log10')+
    xlab("number")  + ylab("Counts logscaled") + ggtitle("Causal Variables", subtitle = waiver())

#plot all the new scaled as histograms
cases_scaled %>% select(gini_index, median_income, percent_commuters, percent_male,unemployment_rate, percent_black, percent_white) %>%
  keep(is.numeric)  %>%
  gather() %>%
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram() +
    xlab("number")  + ylab("Counts") + ggtitle("Unconfirmed Causal Variables", subtitle = waiver())


#plot new features in correlations
#remove non-ratio data
cases_scaled[-1]


cor_TX <- cases_scaled[,-1]%>% select(deaths_per_1000, death_per_case, cases_per_1000,deaths, confirmed_cases, population_density, primary_care_1000, total_pop, icu_bed_pop) %>%
  keep(is.numeric) %>% cor
options(repr.plot.width=6, repr.plot.height=6)


require(GGally)
ggparcoord(as_tibble(iris), columns = 1:4, groupColumn = 5)


ggcorrplot(cor_TX, p.mat = cor_pmat(cases_scaled[,-1]), insig = "blank", hc.order = TRUE, lab = TRUE) + ggtitle("Correlation matrix with causal variables")
cor_TX <- cases_scaled[,-1]%>% select(deaths_per_1000, death_per_case, deaths, confirmed_cases,gini_index, median_income, percent_commuters, percent_male,unemployment_rate, percent_black, percent_white) %>%
  keep(is.numeric) %>% cor
options(repr.plot.width=7, repr.plot.height=7)

ggcorrplot(cor_TX, p.mat = cor_pmat(cases_scaled[,-1]), insig = "blank", hc.order = TRUE, lab = TRUE) + ggtitle("Correlation matrix with experimental variables")


cases_scaled %>% select(death_per_case) %>% summarise(mean(death_per_case))


options(repr.plot.width=5, repr.plot.height=3)

ggplot(cases_scaled, mapping = aes(x = population_density, y = deaths, label = county_name)) +
  geom_smooth(method = loess) +
  geom_point(mapping = aes(size = deaths_per_1000),alpha=.1, color = "blue") +
  ggtitle("Total Populations to ", subtitle="and Deaths per 1000") + geom_density_2d()
  xlim(0, 2500000) + ylim(0,1000)


ggplot(cases_scaled, mapping = aes(x = total_pop, y = `ICU Beds`, label = county_name)) +
  geom_smooth(method = loess,  se=FALSE) +
  geom_point(mapping = aes(size = total_pop),alpha=.1 ,color = "blue")+
  ggtitle("ICU beds to Deaths per Case") + xlim(.000001,NA) + ylim(0.00001,NA)#+ xlim(0,1000) + ylim(0.0001, NA)
dim(cases_scaled)



ggplot(sample_n(cases_scaled,300), mapping = aes(x = icu_bed_pop, y = death_per_case, label = county_name)) +
  geom_smooth(method = loess,  se=FALSE) +
  geom_point(mapping = aes(size = total_pop),alpha=.1 ,color = "blue")+
  ggtitle("Totoal pop oto confirmed cases per 1000")

#

  +
  geom_point(mapping = aes(size = population_density), color = "grey") +
  geom_text_repel(data = subset(cases_scaled, population_density >= .05)) +
  ggtitle("Cases linked to Population Density") +  xlim(0, 2600000) + ylim(0,20000)


ggplot(cases_scaled, mapping = aes(x = percent_black, y = cases_per_1000, label = county_name)) +
  geom_smooth(method = loess) +
  geom_point(mapping = aes(size = total_pop), color = "grey") +
  ggtitle("Cases linked to Population Density", subtitle="and death per case")














# Source: https://www.google.com/covid19/mobility/index.html

mobility <- read_csv("Global_Mobility_Report.csv", col_types =  cols(sub_region_2 = col_character()))
mobility <- mobility %>% mutate_if(is.character, factor)
mobility %>% select(country_region) %>% unique

us_mobility <- mobility %>% dplyr::filter(country_region=="United States")
summary(us_mobility)
dim(us_mobility)

us_mobility
ggplot(us_mobility, mapping = aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) + geom_line() + geom_smooth()


us_mobility_by_state <- us_mobility %>% select(sub_region_1, retail_and_recreation_percent_change_from_baseline, date) %>%
            drop_na() %>% group_by(sub_region_1, date) %>% summarize(mean_mobility = mean(retail_and_recreation_percent_change_from_baseline))


library(directlabels)
# install.packages("directlabels")


options(repr.plot.width=8, repr.plot.height=3)

ggplot(us_mobility_by_state, aes(x = date, y = mean_mobility)) +
  geom_line(aes(color = sub_region_1, linetype = sub_region_1),  show.legend = FALSE) +
  geom_dl(aes(label = sub_region_1, color = sub_region_1), method = list('last.qp', cex = .5))+
  ggtitle("State Mobility Over Time") + lims(x= c( Sys.Date() - 120, Sys.Date() -10))





texas_mobility <- us_mobility %>% dplyr::filter(sub_region_1=="Texas") %>% select(sub_region_2, retail_and_recreation_percent_change_from_baseline, date) %>% drop_na() %>%
    group_by(sub_region_2, date) %>% summarize(mean_mobility = mean(retail_and_recreation_percent_change_from_baseline))

options(repr.plot.width=3, repr.plot.height=3)
dim(texas_mobility)
ggplot(texas_mobility, aes(x = date, y = mean_mobility)) +
  geom_line(aes(color = sub_region_2, linetype = sub_region_2),  show.legend = FALSE, se=FALSE) +
  geom_dl(aes(label = sub_region_2, color = sub_region_2), method = list(dl.combine("first.points", "last.points"),cex = 0.5))+
  ggtitle("Texas County Mobility Over Time") +
   ylim(-60,60) + lims(x= c( Sys.Date() - 120, Sys.Date() -10))


  # geom_label_repel(aes(label = sub_region_2),
  # nudge_x = 1,
  # na.rm = TRUE)


require(reshape2)

us_mobility %>% colnames()
us_mobility_type_agg <- us_mobility %>% select(contains("baseline"), date) %>% melt(id.vars = "date")  %>% drop_na


options(repr.plot.width=6, repr.plot.height=3)
ggplot(us_mobility_type_agg, mapping = aes(x = date, y = value))  +
  geom_smooth(aes(color = variable, linetype = variable),  show.legend = FALSE) +
  scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = variable, color = variable), method = list('last.bumpup', cex = .5)) +
  ggtitle("US Mobility Types Over Time") + ylim(-50,50)  + lims(x= c( Sys.Date() - 120, Sys.Date() + 30))
