library(here)

gapminder <- readr::read_csv(here("data/gapminder/raw/gapminder_data.csv"))
mean(gapminder$gdpPercap[gapminder$continent=="Africa"])
mean(gapminder$gdpPercap[gapminder$continent=="Americas"])

## difficult to do each analysis individually, ergo use dplyr or tidyverse
library(tidyverse)
year_country_gdp <- select(gapminder, year, country, gdpPercap)
head(year_country_gdp)
year_country_gdp<- gapminder %>%  ## %>% "Pipes" the data down to the next line
  filter(continent == "Europe") %>%  
  select(year, country, gdpPercap)  ## restricts your dataset to those variables

LifeExp_country_year<- select(gapminder, country, year, lifeExp, continent)
LifeExp_country_year<-gapminder %>%
  filter(continent == "Africa")  ## 624 rows of data

gapminder %>%  
  group_by(continent) %>%
  summarize(mean_val = mean(gdpPercap))

lifeExp_bycountry <- gapminder %>%
  group_by(country) %>%
  summarize(mean_lifeExp = mean(lifeExp))
  filter(mean_lifeExp == min(mean_lifeExp == max(mean_lifeExp)))
 
gdp_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap), sd_gdpPercap = sd(gdpPercap))  
head(gdp_bycontinents_byyear)

gapminder %>%
  ggplot(data=gapminder, aes(x=year, y=lifeExp, color=continent)) +
  geom_line() +
  facet_wrap(~country)

gapminder %>%
  filter(continent =="Africa") %>%  ##restricting to just African countries
  ggplot( aes(x=year, y=lifeExp, color=continent)) +
  geom_line() +
  facet_wrap(~country)

