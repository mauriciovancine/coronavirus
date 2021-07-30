#' ---
#' title: covid19 municipios e estados do brasil
#' author: mauricio vancine
#' date: 2021-06-15
#' ---

# packages ----------------------------------------------------------------
# load packages
library(tidyverse)
library(lubridate)

# directory
setwd("/home/mude/data/github/coronavirus/unesp")

# data --------------------------------------------------------------------
# municipality information
info <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cities_info.csv")
dplyr::glimpse(info)

## state time
sta_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% 
  dplyr::rename(abbrev_state = state) %>% 
  dplyr::left_join(info %>% group_by(state) %>% summarise(pop2020 = sum(pop2020)/1e5), 
                   by = c("abbrev_state" = "state")) %>% 
  dplyr::mutate(newDeaths_per_100k_inhabitants = zoo::rollmean(newDeaths, k = 7, fill = NA)/pop2020,
                newCases_per_100k_inhabitants = zoo::rollmean(newCases, k = 7, fill = NA)/pop2020,
                totalCases_per_100k_inhabitants = zoo::rollmean(totalCases, k = 7, fill = NA)/pop2020,
                totalDeaths_per_100k_inhabitants = zoo::rollmean(deaths, k = 7, fill = NA)/pop2020)
dplyr::glimpse(sta_cases_time)

# graphics ----------------------------------------------------------------
## Scatter total deats vs cases per 100k
sta_cases_time %>% 
  dplyr::filter(abbrev_state == "SP") %>% 
  ggplot(aes(x = totalCases_per_100k_inhabitants, y = totalDeaths_per_100k_inhabitants)) +
  geom_point(color = "steelblue", size = 4, alpha = .7) +
  stat_smooth(method = "gam", size = 2, color = "black", lty = 1) +
  theme_classic() +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20)) +  
  labs(x = "Número total de casos (por 100 mil hab.)", 
       y = "Número total de mortes (por 100 mil hab.)")
ggsave("diagrama_total_casos_total_mortes.png", wi = 25, he = 20, un = "cm", dpi = 200)

## Scatter new deats vs cases per 100k
sta_cases_time %>% 
  dplyr::filter(abbrev_state == "SP") %>% 
  ggplot(aes(x = newCases_per_100k_inhabitants, 
             y = newDeaths_per_100k_inhabitants)) +
  geom_point(color = "steelblue", size = 4, alpha = .7) +
  stat_smooth(method = "gam", size = 2, color = "black", lty = 1) +
  theme_classic() +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20)) +  
  labs(x = "Novos casos (por 100 mil hab.)", 
       y = "Novos mortes (por 100 mil hab.)")
ggsave("diagrama_novos_casos_novas_mortes.png", wi = 25, he = 20, un = "cm", dpi = 200)

## Scatter new deats/cases per 100k per days
sta_cases_time %>% 
  dplyr::filter(abbrev_state == "SP") %>% 
  ggplot(aes(x = as_date(date), 
             y = newDeaths_per_100k_inhabitants/newCases_per_100k_inhabitants)) +
  geom_point(color = "steelblue", size = 4, alpha = .5) +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  stat_smooth(method = "gam", size = 2, color = "black", lty = 1) +
  theme_classic() +
  theme(axis.title = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(angle = 90, vjust = .5)) +  
  labs(x = "Data", 
       y = "Novos mortes/novos casos (por 100 mil hab.)")
ggsave("diagrama_novas_mortes_novos_casos_tempo.png", wi = 25, he = 20, un = "cm", dpi = 200)

## Histogram
sta_cases_time %>% 
  dplyr::filter(abbrev_state == "SP") %>% 
  ggplot(aes(x = newDeaths_per_100k_inhabitants/newCases_per_100k_inhabitants)) +
  geom_histogram(bins = 20, color = "white", fill = "steelblue", alpha = .7) +
  theme_classic() +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20)) +  
  labs(x = "Novos mortes/novos casos (por 100 mil hab.)", 
       y = "Frequência")
ggsave("histograma.png", wi = 25, he = 20, un = "cm", dpi = 200)


# end ---------------------------------------------------------------------
