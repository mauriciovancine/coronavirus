#' ---
#' title: covid19 municipios e estados do brasil
#' author: mauricio vancine
#' date: 2021-06-15
#' ---

# packages ----------------------------------------------------------------

# load packages
library(ggrepel)
library(geobr)
library(ggsci)
library(lubridate)
library(sf)
library(spatialEco)
library(tmap)
library(zoo)
library(tidyverse)
library(wesanderson)

# options
options(scipen = 1e5)

# import data -------------------------------------------------------------
# coronavirus no mundo
wd_cases <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  dplyr::mutate(date = date %>% lubridate::as_date() %>% lubridate::ymd(),
                country_name = stringr::str_replace_all(location, "_", " "),
                cases = total_cases,
                deaths = total_deaths,
                cases_pop = new_cases_per_million,
                deaths_pop = new_deaths_per_million,
                cases_rollmean_pop = zoo::rollmean(new_cases_per_million, k = 7, fill = NA),
                deaths_rollmean_pop = zoo::rollmean(new_deaths_per_million, k = 7, fill = NA),
                vaccinations = new_vaccinations,
                vaccinations_rollmean = zoo::rollmean(new_vaccinations, k = 7, fill = NA)) %>% 
  dplyr::select(date,
                country_name,
                cases,
                deaths,
                cases_pop,
                deaths_pop,
                cases_rollmean_pop,
                deaths_rollmean_pop,
                vaccinations,
                vaccinations_rollmean)
wd_cases

# municipality information
info <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cities_info.csv")
dplyr::glimpse(info)

# state
sta_cases <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv") %>% 
  dplyr::rename(abbrev_state = state)
dplyr::glimpse(sta_cases)

# state time
sta_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% 
  dplyr::rename(abbrev_state = state) %>% 
  dplyr::left_join(info %>% group_by(state) %>% summarise(pop2020 = sum(pop2020)/1e5), 
                   by = c("abbrev_state" = "state")) %>% 
  dplyr::mutate(newDeaths_per_100k_inhabitants = newDeaths/(pop2020),
                newCases_per_100k_inhabitants = newCases/(pop2020), 
                .after = totalCases_per_100k_inhabitants)
dplyr::glimpse(sta_cases_time)

# municipality
mun_cases <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv") %>%
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))
dplyr::glimpse(mun_cases)

# municipality time
mun_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time_changesOnly.csv.gz") %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>%
  dplyr::left_join(info %>% dplyr::select(ibge, pop2020), 
                   by = c("ibgeID" = "ibge")) %>% 
  dplyr::mutate(newDeaths_per_100k_inhabitants = newDeaths/(pop2020/1e5),
                newCases_per_100k_inhabitants = newCases/(pop2020/1e5), .after = totalCases_per_100k_inhabitants) %>% 
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))
dplyr::glimpse(mun_cases_time)

# mun_cases_time %>% 
# filter(name_muni == "Botucatu") %>% 
#   write_csv("botucatu.csv")

# percetage uti covid
# uti <- readr::read_csv2("ocup_leitos_covid19_20210617_171832.csv")
# uti
# dplyr::glimpse(uti)

# # percetage uti covid
# uti <- readr::read_csv(
#   paste0("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/Leitos/",
#          lubridate::today()-1, "/esus-vepi.LeitoOcupacao.csv"))
# uti
# dplyr::glimpse(uti)
  
# import geodata ----------------------------------------------------------
# state geodata
sta_geo <- geobr::read_state(code_state = "all", year = 2020) %>%
  sf::as_Spatial() %>%
  spatialEco::explode() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(area = as.numeric(sf::st_area(.)/1e6)) %>%
  dplyr::arrange(area) %>%
  dplyr::filter(area > 5e3)
sta_geo

# states centroids
sta_geo_cen <- sf::st_centroid(sta_geo)
sta_geo_cen

# municipality geodata
mun_geo <- geobr::read_municipality(code_muni = "all", year = 2020) %>%
  sf::st_crop(sta_geo)
mun_geo

# municipality centroids
mun_geo_cen <- geobr::read_municipal_seat(year = 2010)
mun_geo_cen

# join data ---------------------------------------------------------------
# state data
sta_cases_geo <- sta_geo %>%
  dplyr::mutate(abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(sta_cases, "abbrev_state")
dplyr::glimpse(sta_cases_geo)

# state data time
sta_cases_time_geo <- sta_geo_cen %>%
  dplyr::mutate(abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(sta_cases_time, "abbrev_state")
dplyr::glimpse(sta_cases_time_geo)

# municipality data
mun_cases_geo <- mun_geo %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni),
                abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(mun_cases, by = "name_muni")
dplyr::glimpse(mun_cases_geo)

# municipality data time
mun_cases_time_geo <- mun_geo_cen %>%
  dplyr::mutate(name_muni = as.character(name_muni)) %>%
  dplyr::left_join(mun_cases_time, by = "name_muni") %>%
  tidyr::drop_na(date)
dplyr::glimpse(mun_cases_time_geo)

# uti
# uti_geo <- sta_geo %>% 
#   dplyr::left_join(uti, by = c("name_state" = "uf"))
# uti_geo

# graphics ----------------------------------------------------------------
# world ----
# world total cases ----
cou_cases <- wd_cases %>%
  dplyr::group_by(country_name) %>%
  dplyr::summarise(cases_sum = sum(cases)) %>%
  dplyr::arrange(-cases_sum) %>%
  dplyr::filter(!country_name %in% c("World", "North America", "Europe", "South America", "European Union", "Asia")) %>% 
  dplyr::slice(1:5) %>%
  dplyr::select(country_name) %>% 
  dplyr::pull()
cou_cases

fig_world_cases <- wd_cases %>%
  dplyr::filter(country_name %in% cou_cases, 
                cases_pop > 0, 
                date > "2020-02-20") %>%
  ggplot() +
  geom_line(aes(x = date, y = cases_pop, color = country_name), size = .2) +
  geom_point(aes(x = date, y = cases_pop, color = country_name, fill = country_name), col = "white", size = 3, 
             shape = 21, stroke = 1) +
  geom_line(aes(x = date, y = cases_rollmean_pop, color = country_name), size = 1) +
  # geom_label_repel(data = wd_cases %>% 
  #                    tidyr::drop_na(cases_pop) %>% 
  #                    dplyr::filter(country_name %in% cou_cases) %>%
  #                    dplyr::filter(date == max(date)),
  #                  aes(x = date, y = cases_pop, label = country_name, color = country_name),
  #                  fill = "white", hjust = 1, alpha = .9) +
  labs(x = "Data",
       y = "Número de novos casos (por milhões de hab.)",
       title = "Número de novos casos no mundo",
       color = "Países",
       fill = "Países") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  scale_color_jco() +
  scale_fill_jco() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.15, .8))
fig_world_cases
ggsave(filename = "graficos/fig_world_cases.png", 
       plot = fig_world_cases, width = 30, height = 20, units = "cm", dpi = 200)

# world deaths ----
cou_deaths <- wd_cases %>%
  dplyr::group_by(country_name) %>%
  dplyr::summarise(deaths_sum = sum(deaths, na.rm = TRUE)) %>%
  dplyr::arrange(-deaths_sum) %>%
  dplyr::filter(!country_name %in% c("World", "North America", "Europe", "South America", "European Union", "Asia")) %>% 
  dplyr::slice(1:5) %>%
  dplyr::select(country_name) %>%
  dplyr::pull()
cou_deaths

fig_world_deaths <- wd_cases %>%
  dplyr::filter(country_name %in% cou_deaths, date > "2020-02-20",
                deaths_pop > 0) %>%
  ggplot() +
  geom_line(aes(x = date, y = deaths_pop, color = country_name), size = .2) +
  geom_point(aes(x = date, y = deaths_pop, color = country_name, fill = country_name), 
             col = "white", size = 3, shape = 21, stroke = 1) +
  geom_line(aes(x = date, y = deaths_rollmean_pop, color = country_name), size = 1) +
  # geom_label_repel(data = wd_cases %>% 
  #                    tidyr::drop_na(deaths_pop) %>% 
  #                    dplyr::filter(country_name %in% cou_deaths) %>%
  #                    dplyr::filter(date == max(date)),
  #                  aes(x = date, y = deaths_pop, label = country_name, color = country_name),
  #                  fill = "white", hjust = 1, alpha = .9) +
  labs(x = "Data",
       y = "Número de novos óbitos (por milhões de hab.)",
       title = "Número de novos óbitos no mundo",
       color = "Países",
       fill = "Países") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  scale_color_jco() +
  scale_fill_jco() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.15, .8))
fig_world_deaths
ggsave(filename = "graficos/fig_world_deaths.png", 
       plot = fig_world_deaths, width = 30, height = 20, units = "cm", dpi = 200)

# world vaccinations ----
# cou_vaccinations <- wd_cases %>%
#   dplyr::group_by(country_name) %>%
#   dplyr::summarise(vaccinations_sum = sum(vaccinations, na.rm = TRUE)) %>%
#   dplyr::arrange(-vaccinations_sum) %>%
#   dplyr::slice(1:5) %>%
#   dplyr::select(country_name) %>%
#   dplyr::pull()
cou_vaccinations <- c("China", "United States", "India", "United Kingdom", "Brazil", "France", "Russia")
cou_vaccinations
 
fig_world_vaccinations <- wd_cases %>%
  dplyr::filter(country_name %in% cou_vaccinations, 
                date > "2020-12-01") %>%
  ggplot() +
  geom_line(aes(x = date, y = vaccinations,
                color = country_name), size = .2) +
  geom_point(aes(x = date, y = vaccinations, 
                 color = country_name, fill = country_name),
             col = "white", size = 3, shape = 21, stroke = 1) +
  geom_line(aes(x = date, y = vaccinations_rollmean, 
                color = country_name), size = 1) +
  labs(x = "Data",
       y = "Número de vacinados (por milhões de hab.)",
       color = "Countries",
       fill = "Countries",
       title = "Número de vacinados no mundo") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  scale_color_jco() +
  scale_fill_jco() +
  #scale_y_log10() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(angle = 90, hjust = .5),
        legend.position = c(.15, .8))
fig_world_vaccinations
ggsave(filename = "graficos/fig_world_vaccinations.png",
       plot = fig_world_vaccinations, width = 30, height = 20, units = "cm", dpi = 200)



# brazil ----
# brazil total cases  ----
fig_brazil_cases <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL")  %>% 
  dplyr::mutate(totalCases_rollmean = zoo::rollmean(totalCases, k = 7, fill = NA)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = totalCases), size = .2, 
            color = "steelblue", linetype = 2) +
  geom_point(aes(x = date, y = totalCases), size = 3, 
             color = "white", fill = "steelblue", shape = 21, 
             stroke = 1, alpha = .95) +
  geom_line(aes(x = date, y = totalCases_rollmean),
            size = 1.5, color = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de casos",
       title = "Número total de casos no Brasil") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_brazil_cases
ggsave(filename = "graficos/fig_brazil_cases.png", 
       plot = fig_brazil_cases, width = 30, height = 20, units = "cm", dpi = 200)

# brazil new cases ----
fig_brazil_new_cases <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  dplyr::mutate(newCases_rollmean = zoo::rollmean(newCases, 7, fill = NA),
                #newCases_rollsd = roll::roll_sd(newCases, width = 7),
                #newCases_roll_lo95 = newCases_rollmean - 2*newCases_rollsd, 
                #newCases_roll_hi95 = newCases_rollmean + 2*newCases_rollsd
  ) %>%
  ggplot() +
  geom_line(aes(x = date, y = newCases), 
            size = .2, color = "red", linetype = 2) +
  geom_point(aes(x = date, y = newCases), 
             size = 3, color = "white", fill = "red", 
             shape = 21, stroke = 1, alpha = .95) +
  geom_line(aes(x = date, y = newCases_rollmean), 
            size = 1.5, color = "gray30", alpha = .8) +
  # geom_ribbon(aes(x = date,
  #                 ymin = newCases_roll_lo95, 
  #                 ymax = newCases_roll_hi95), 
  #             color = "red", alpha = .4) +
  labs(x = "Data",
       y = "Número de novos casos",
       title = "Número de novos casos no Brasil") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_brazil_new_cases
ggsave(filename = "graficos/fig_brazil_new_cases.png", 
       plot = fig_brazil_new_cases, width = 30, height = 20, units = "cm", dpi = 200)

# brazil deaths ----
fig_brazil_deaths <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  dplyr::mutate(deaths_rollmean = zoo::rollmean(deaths, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = date, y = deaths), size = .2, 
            color = "gray40", linetype = 2) +
  geom_point(aes(x = date, y = deaths), size = 3, color = "white", 
             fill = "gray40", shape = 21, stroke = 1, alpha = .95) +
  geom_line(aes(x = date, y = deaths_rollmean), size = 1.5, 
            color = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de óbitos",
       title = "Número total de óbitos no Brasil") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_brazil_deaths
ggsave(filename = "graficos/fig_brazil_deaths.png", 
       plot = fig_brazil_deaths, width = 30, height = 20, units = "cm", dpi = 200)

# brazil new deaths ----
fig_brazil_new_deaths <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  dplyr::mutate(newDeaths_rollmean = zoo::rollmean(newDeaths, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = date, y = newDeaths), size = .2, 
            color = "gray50", linetype = 2) +
  geom_point(aes(x = date, y = newDeaths), size = 3, color = "white", 
             fill = "gray50", shape = 21, stroke = 1, alpha = .95) +
  geom_line(aes(x = date, y = newDeaths_rollmean), size = 1.5, 
            color = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos óbitos",
       title = "Número de novos óbitos no Brasil") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_brazil_new_deaths
ggsave(filename = "graficos/fig_brazil_new_deaths.png", 
       plot = fig_brazil_new_deaths, width = 30, height = 20, units = "cm", dpi = 200)

# brazil recovered ----
fig_brazil_recovered <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  dplyr::mutate(recovered_rollmean = zoo::rollmean(recovered, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = date, y = recovered), size = .2, 
            color = "forestgreen", linetype = 2) +
  geom_point(aes(x = date, y = recovered), size = 3, 
             color = "white", fill = "forestgreen", shape = 21, 
             stroke = 1, alpha = .95) +
  geom_line(aes(x = date, y = recovered), size = 1.5, 
            color = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Númerode de recuperados",
       title = "Número total de recuperados no Brasil") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_brazil_recovered
ggsave(filename = "graficos/fig_brazil_recovered.png", 
       plot = fig_brazil_recovered, width = 30, height = 20, units = "cm", dpi = 200)

# states ----
# state total cases ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_cases <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(totalCases_rollmean = zoo::rollmean(totalCases, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = totalCases), size = .2, 
                col = "steelblue4", linetype = 2) +
      geom_point(aes(x = date, y = totalCases), size = 3, 
                 col = "white", fill = "steelblue4", shape = 21, 
                 stroke = 1) +
      geom_line(aes(x = date, y = totalCases_rollmean), size = 1.5, 
                col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de casos",
           title = paste0("Número total de casos - ", i)) +
      scale_x_date(date_breaks = "10 day",
                   date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_cases
    ggsave(filename = paste0("graficos/fig_state_cases_", i, ".png"), 
           plot = fig_state_cases, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state new cases ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_new_cases <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(newCases_rollmean = zoo::rollmean(newCases, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = newCases), size = .2, 
                col = "red", linetype = 2) +
      geom_point(aes(x = date, y = newCases), size = 3, 
                 col = "white", fill = "red", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = newCases_rollmean), size = 1.5, 
                col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de novos casos",
           title = paste0("Número de novos casos - ", i)) +
      scale_x_date(date_breaks = "10 day",
                   date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_new_cases
    ggsave(filename = paste0("graficos/fig_state_new_cases_", i, ".png"), 
           plot = fig_state_new_cases, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state deaths ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_deaths <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(deaths_rollmean = zoo::rollmean(deaths, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = deaths), size = .2, 
                col = "gray40", linetype = 2) +
      geom_point(aes(x = date, y = deaths), size = 3, 
                 color = "white", fill = "gray40", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = deaths_rollmean), size = 1.5, 
                col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de óbitos",
           title = paste0("Número total de óbitos - ", i)) +
      scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_deaths
    ggsave(filename = paste0("graficos/fig_state_deaths_", i, ".png"), 
           plot = fig_state_deaths, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state new deaths ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_new_deaths <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(newDeaths_rollmean = zoo::rollmean(newDeaths, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = newDeaths), size = .2, 
                col = "gray60", linetype = 2) +
      geom_point(aes(x = date, y = newDeaths), size = 3, 
                 color = "white", fill = "gray60", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = newDeaths_rollmean), size = 1.5, 
                col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de novos óbitos",
           title = paste0("Número novos óbitos - ", i)) +
      scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_new_deaths
    ggsave(filename = paste0("graficos/fig_state_new_deaths_", i, ".png"), 
           plot = fig_state_new_deaths, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state recovered ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_recovered <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(recovered_rollmean = zoo::rollmean(recovered, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = recovered), size = .2, 
                col = "forestgreen", linetype = 2) +
      geom_point(aes(x = date, y = recovered), size = 3, 
                 col = "white", fill = "forestgreen", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = recovered_rollmean), size = 1.5, 
                col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de recuperados",                                                                               title = paste0("Número de recuperados - ", i)) +
      scale_x_date(date_breaks = "10 day",
                   date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_recovered
    ggsave(filename = paste0("graficos/fig_state_recovered_", i, ".png"), 
           plot = fig_state_recovered, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state suspects ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_suspects <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(suspects_rollmean = zoo::rollmean(suspects, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = suspects), size = .2, 
                col = "orange", linetype = 2) +
      geom_point(aes(x = date, y = suspects), size = 3, 
                 col = "white", fill = "orange", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = suspects_rollmean), size = 1.5, 
                col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de suspeitos",
           title = paste0("Número de suspeitos - ", i)) +
      scale_x_date(date_breaks = "10 day",
                   date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_suspects
    ggsave(filename = paste0("graficos/fig_state_suspects_", i, ".png"), 
           plot = fig_state_suspects, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state tests ----
# for(i in sta_cases_time$abbrev_state %>% unique){
#   
#   if(i != "TOTAL"){
#     
#   fig_state_tests <- sta_cases_time %>%
#     dplyr::filter(abbrev_state == i) %>% 
#     ggplot() +
#     aes(x = date, y = tests) +
#     geom_line(size = 1, col = "purple") +
#     geom_point(size = 3, col = "white", fill = "purple", shape = 21, stroke = 1) +
#     labs(x = "Data",
#          y = "Número de testes",
#          title = "") +
#     scale_x_date(date_breaks = "10 day",
#                  date_labels = "%d/%m") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 90, vjust = .5),
#           legend.position = "none")
#   fig_state_tests
#   ggsave(filename = paste0("graficos/fig_state_tests_", i, ".png"), 
#          plot = fig_state_tests, width = 25, height = 20, units = "cm", dpi = 200)
#   }
# }


# state total cases pop ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_cases_pop <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(totalCases_per_100k_inhabitants_rollmean = zoo::rollmean(totalCases_per_100k_inhabitants, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = totalCases_per_100k_inhabitants), 
                size = .2, col = "steelblue4", linetype = 2) +
      geom_point(aes(x = date, y = totalCases_per_100k_inhabitants),
                 size = 3, col = "white", fill = "steelblue4", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = totalCases_per_100k_inhabitants_rollmean), 
                size = 1.5, col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de casos (por 100 mil hab.)",
           title = paste0("Número total de casos (por 100 mil hab.) - ", i)) +
      scale_x_date(date_breaks = "10 day",
                   date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_cases_pop
    ggsave(filename = paste0("graficos/fig_state_cases_pop_", i, ".png"), 
           plot = fig_state_cases_pop, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state new cases pop ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_new_cases_pop <- sta_cases_time %>%
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(newCases_per_100k_inhabitants_rollmean = zoo::rollmean(newCases_per_100k_inhabitants, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = newCases_per_100k_inhabitants), 
                size = .2, col = "red", linetype = 2) +
      geom_point(aes(x = date, y = newCases_per_100k_inhabitants),
                 size = 3, col = "white", fill = "red", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = newCases_per_100k_inhabitants_rollmean), 
                size = 1.5, col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de novos casos (por 100 mil hab.)",
           title = paste0("Número de novos casos (por 100 mil hab.) - ", i)) +
      scale_x_date(date_breaks = "10 day",
                   date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_new_cases_pop
    ggsave(filename = paste0("graficos/fig_state_new_cases_pop_", i, ".png"), 
           plot = fig_state_new_cases_pop, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}


# state deaths pop ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_deaths_pop <- sta_cases_time %>% 
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(deaths_per_100k_inhabitants_rollmean = zoo::rollmean(deaths_per_100k_inhabitants, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = deaths_per_100k_inhabitants), 
                size = .2, col = "gray40", linetype = 2) +
      geom_point(aes(x = date, y = deaths_per_100k_inhabitants), 
                 size = 3, color = "white", fill = "gray40", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = deaths_per_100k_inhabitants_rollmean), 
                size = 1.5, col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de óbitos (por 100 mil hab.)",
           title = paste0("Número total de óbitos (por 100 mil hab.) - ", i)) +
      scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_deaths_pop
    ggsave(filename = paste0("graficos/fig_state_deaths_pop_", i, ".png"), 
           plot = fig_state_deaths_pop, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state new deaths pop ----
for(i in sta_cases_time$abbrev_state %>% unique){
  
  if(i != "TOTAL"){
    
    print(i)
    
    fig_state_new_deaths_pop <- sta_cases_time %>% 
      dplyr::filter(abbrev_state == i) %>% 
      dplyr::mutate(newdeaths_per_100k_inhabitants_rollmean = zoo::rollmean(newDeaths_per_100k_inhabitants, k = 7, fill = NA)) %>%
      ggplot() +
      geom_line(aes(x = date, y = newDeaths_per_100k_inhabitants), 
                size = .2, col = "gray60", linetype = 2) +
      geom_point(aes(x = date, y = newDeaths_per_100k_inhabitants), 
                 size = 3, color = "white", fill = "gray60", shape = 21, stroke = 1) +
      geom_line(aes(x = date, y = newdeaths_per_100k_inhabitants_rollmean), 
                size = 1.5, col = "gray30", alpha = .8) +
      labs(x = "Data",
           y = "Número de novos óbitos (por 100 mil hab.)",
           title = paste0("Número de novos óbitos (por 100 mil hab.) - ", i)) +
      scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            legend.position = "none")
    fig_state_new_deaths_pop
    ggsave(filename = paste0("graficos/fig_state_new_deaths_pop_", i, ".png"), 
           plot = fig_state_new_deaths_pop, width = 25, height = 20, units = "cm", dpi = 200)
    
  }
  
}

# state tests pop ----
# for(i in sta_cases_time$abbrev_state %>% unique){
#   
#   if(i != "TOTAL"){
#     
#   fig_state_tests_pop <- sta_cases_time %>% 
#     dplyr::filter(abbrev_state == i) %>% 
#     ggplot() +
#     aes(x = date, y = tests_per_100k_inhabitants) +
#     geom_line(size = 1, col = "purple") +
#     geom_point(size = 3, color = "white", fill = "purple", shape = 21, stroke = 1) +
#     labs(x = "Data",
#          y = "Número de testes (por 100 mil hab.)",
#          title = "") +
#     scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 90, vjust = .5),
#           legend.position = "none")
#   fig_state_tests_pop
#   ggsave(filename = paste0("graficos/fig_state_tests_pop_", i, ".png"), 
#          plot = fig_state_tests_pop, width = 25, height = 20, units = "cm", dpi = 200)
#   }
# }

# plano sao paulo - https://github.com/seade-R/dados-covid-sp ----
da_sp <- readr::read_csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv") %>% 
  dplyr::left_join(info %>% dplyr::select(ibge, pop2020), by = c("codigo_ibge" = "ibge")) %>% 
  dplyr::mutate(nome_drs_name = nome_drs %>% 
                  stringr::str_to_lower() %>% 
                  stringr::str_replace_all(" ", "_") %>% 
                  stringi::stri_trans_general(id = "Latin-ASCII"),
                casos_novos_pc = casos_novos/(pop2020/1e5),
                obitos_novos_pc = obitos_novos/(pop2020/1e5)) %>% 
  dplyr::arrange(nome_drs)
dplyr::glimpse(da_sp)

for(i in seq(na.omit(unique(da_sp$nome_drs)))){
  
  print(unique(da_sp$nome_drs)[i])
  
  fig_planosp_new_cases <- da_sp %>%
    dplyr::filter(nome_drs == unique(da_sp$nome_drs)[i]) %>% 
    dplyr::group_by(nome_drs, datahora) %>% 
    dplyr::summarise(casos_novos_sum_pc = sum(casos_novos)/(sum(pop2020)/1e5)) %>% 
    dplyr::mutate(casos_novos_sum_pc_rollmean = zoo::rollmean(casos_novos_sum_pc, k = 7, fill = NA)) %>%
    ggplot() +
    geom_line(aes(x = datahora, y = casos_novos_sum_pc),
              size = .2, col = "red", linetype = 2) +
    geom_point(aes(x = datahora, y = casos_novos_sum_pc),
               size = 3, col = "white", fill = "red", shape = 21, stroke = 1) +
    geom_line(aes(x = datahora, y = casos_novos_sum_pc_rollmean),
              size = 1.5, col = "gray30", alpha = .8) +
    labs(x = "Data",
         y = "Número de novos casos (por 100 mil hab.)",
         title = paste0("Número de novos casos - Plano São Paulo - Região de ", unique(da_sp$nome_drs)[i], " (por 100 mil hab.)")) +
    scale_x_date(date_breaks = "10 day",
                 date_labels = "%d/%m") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5),
          legend.position = "none")
  fig_planosp_new_cases
  ggsave(filename = paste0("graficos/fig_planosp_new_cases_", unique(da_sp$nome_drs_name)[i], ".png"), 
         plot = fig_planosp_new_cases, width = 25, height = 20, units = "cm", dpi = 200)
  
  fig_planosp_new_deaths <- da_sp %>%
    dplyr::filter(nome_drs == unique(da_sp$nome_drs)[i]) %>% 
    dplyr::group_by(nome_drs, datahora) %>% 
    dplyr::summarise(obitos_novos_sum_pc = sum(obitos_novos)/(sum(pop2020)/1e5)) %>% 
    dplyr::mutate(obitos_novos_sum_pc_rollmean = zoo::rollmean(obitos_novos_sum_pc, k = 7, fill = NA)) %>%
    ggplot() +
    geom_line(aes(x = datahora, y = obitos_novos_sum_pc), 
              size = .2, col = "gray40", linetype = 2) +
    geom_point(aes(x = datahora, y = obitos_novos_sum_pc), 
               size = 3, color = "white", fill = "gray40", shape = 21, stroke = 1) +
    geom_line(aes(x = datahora, y = obitos_novos_sum_pc_rollmean), 
              size = 1.5, col = "gray30", alpha = .8) +
    labs(x = "Data",
         y = "Número de novos óbitos (por 100 mil hab.)",
         title = paste0("Número de novos óbitos - Plano São Paulo - Região de ", unique(da_sp$nome_drs)[i], " (por 100 mil hab.)")) +
    scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5),
          legend.position = "none")
  fig_planosp_new_deaths
  ggsave(filename = paste0("graficos/fig_planosp_new_deaths_", unique(da_sp$nome_drs_name)[i], ".png"), 
         plot = fig_planosp_new_deaths, width = 25, height = 20, units = "cm", dpi = 200)
  
}

# summary ----
data_br <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  dplyr::mutate(newCases_per_100k_inhabitants = newCases/(sum(info$pop2020)/1e5),
                newCases_br = zoo::rollmean(newCases_per_100k_inhabitants, 7, fill = NA),
                newDeaths_per_100k_inhabitants = newDeaths/(sum(info$pop2020)/1e5),
                newDeaths_br = zoo::rollmean(newDeaths_per_100k_inhabitants, k = 7, fill = NA)) %>% 
  dplyr::select(date, newCases_br, newDeaths_br)
data_br

data_sp <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "SP") %>% 
  dplyr::mutate(newCases_sp = zoo::rollmean(newCases_per_100k_inhabitants, k = 7, fill = NA),
                newDeaths_sp = zoo::rollmean(newDeaths_per_100k_inhabitants, k = 7, fill = NA)) %>% 
  dplyr::select(date, newCases_sp, newDeaths_sp)
data_sp

data_pi <- da_sp %>%
  dplyr::filter(nome_drs == "Piracicaba") %>% 
  dplyr::group_by(datahora) %>% 
  dplyr::summarise(newCases_pc = sum(casos_novos)/(sum(pop2020)/1e5),
                   newDeaths_pc = sum(obitos_novos)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(newCases_pi = zoo::rollmean(newCases_pc, k = 7, fill = NA),
                newDeaths_pi = zoo::rollmean(newDeaths_pc, k = 7, fill = NA),
                date = datahora) %>% 
  dplyr::select(date, newCases_pi, newDeaths_pi)
data_pi

data_rc <- da_sp %>%
  dplyr::filter(nome_munic == "Rio Claro") %>% 
  dplyr::group_by(datahora) %>% 
  dplyr::summarise(newCases_pc = sum(casos_novos)/(sum(pop2020)/1e5),
                   newDeaths_pc = sum(obitos_novos)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(newCases_rc = zoo::rollmean(newCases_pc, k = 7, fill = NA),
                newDeaths_rc = zoo::rollmean(newDeaths_pc, k = 7, fill = NA),
                date = datahora) %>% 
  dplyr::select(date, newCases_rc, newDeaths_rc)
data_rc

data_bo <- da_sp %>%
  dplyr::filter(nome_munic == "Botucatu") %>% 
  dplyr::group_by(datahora) %>% 
  dplyr::summarise(newCases_pc = sum(casos_novos)/(sum(pop2020)/1e5),
                   newDeaths_pc = sum(obitos_novos)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(newCases_bo = zoo::rollmean(newCases_pc, k = 7, fill = NA),
                newDeaths_bo = zoo::rollmean(newDeaths_pc, k = 7, fill = NA),
                date = datahora) %>% 
  dplyr::select(date, newCases_bo, newDeaths_bo)
data_bo

data_se <- da_sp %>%
  dplyr::filter(nome_munic == "Serrana") %>% 
  dplyr::group_by(datahora) %>% 
  dplyr::summarise(newCases_pc = sum(casos_novos)/(sum(pop2020)/1e5),
                   newDeaths_pc = sum(obitos_novos)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(newCases_se = zoo::rollmean(newCases_pc, k = 7, fill = NA),
                newDeaths_se = zoo::rollmean(newDeaths_pc, k = 7, fill = NA),
                date = datahora) %>% 
  dplyr::select(date, newCases_se, newDeaths_se)
data_se


cases_summary_pop <- data_rc %>% 
  dplyr::left_join(data_bo) %>% 
  dplyr::left_join(data_se) %>% 
  dplyr::left_join(data_pi) %>% 
  dplyr::left_join(data_sp) %>% 
  dplyr::left_join(data_br) %>% 
  tidyr::pivot_longer(cols = -date, names_to = "var", values_to = "val") %>% 
  tidyr::drop_na() %>%
  dplyr::filter(stringr::str_detect(var, "newCases")) %>% 
  dplyr::mutate(var = recode(var, 
                             newCases_br = "Brasil",
                             newCases_sp = "São Paulo",
                             newCases_pi = "RE Piracicaba",
                             newCases_rc = "Rio Claro",
                             newCases_bo = "Botucatu",
                             newCases_se = "Serrana"))
cases_summary_pop

fig_cases_summary_pop <- ggplot(data = cases_summary_pop) +
  aes(x = date, y = val, color = as.factor(var)) +
  geom_line(size = 1.5, alpha = .8) +
  labs(x = "Data",
       y = "Número de novos casos (por 100 mil hab.)",
       color = "",
       title = "Número de novos casos (por 100 mil hab.)") +
  scale_color_manual(values = wesanderson::wes_palette("Zissou1", 6, type = c("continuous"))) +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.1, .8),
        legend.text = element_text(size = 15))
fig_cases_summary_pop
ggsave(filename = "graficos/fig_cases_summary_pop.png", 
       plot = fig_cases_summary_pop, width = 30, height = 20, units = "cm", dpi = 200)

fig_deaths_summary_pop <- data_rc %>% 
  dplyr::left_join(data_bo) %>% 
  dplyr::left_join(data_se) %>% 
  dplyr::left_join(data_pi) %>% 
  dplyr::left_join(data_sp) %>% 
  dplyr::left_join(data_br) %>%  
  tidyr::pivot_longer(cols = -date, names_to = "var", values_to = "val") %>% 
  tidyr::drop_na() %>%
  dplyr::filter(stringr::str_detect(var, "newDeaths")) %>% 
  dplyr::mutate(var = recode(var, 
                             newDeaths_br = "Brasil",
                             newDeaths_sp = "São Paulo",
                             newDeaths_pi = "RE Piracicaba",
                             newDeaths_rc = "Rio Claro",
                             newDeaths_bo = "Botucatu",
                             newDeaths_se = "Serrana")) %>% 
  ggplot() +
  aes(x = date, y = val, color = as.factor(var)) +
  geom_line(size = 1.5, alpha = .8) +
  labs(x = "Data",
       y = "Número de novos óbitos (por 100 mil hab.)",
       color = "",
       title = "Número de novos óbitos (por 100 mil hab.)") +
  scale_color_manual(values = wesanderson::wes_palette("Zissou1", 6, type = c("continuous"))) +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.1, .8),
        legend.text = element_text(size = 15))
fig_deaths_summary_pop
ggsave(filename = "graficos/fig_deaths_summary_pop.png", 
       plot = fig_deaths_summary_pop, width = 30, height = 20, units = "cm", dpi = 200)

fig_brazil_new_cases_pop <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  dplyr::mutate(newCases_per_100k_inhabitants = newCases/(sum(info$pop2020)/1e5),
                newCases_per_100k_inhabitants_rollmean = zoo::rollmean(newCases_per_100k_inhabitants, 7, fill = NA)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = newCases_per_100k_inhabitants), 
            size = .2, color = "red", linetype = 2) +
  geom_point(aes(x = date, y = newCases_per_100k_inhabitants), 
             size = 3, color = "white", fill = "red", shape = 21, stroke = 1, alpha = .95) +
  geom_line(aes(x = date, y = newCases_per_100k_inhabitants_rollmean), 
            size = 1.5, color = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos casos (por 100 mil hab.)",
       title = "Número de novos casos no Brasil (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_brazil_new_cases_pop
ggsave(filename = "graficos/fig_brazil_new_cases_pop.png", 
       plot = fig_brazil_new_cases_pop, width = 30, height = 20, units = "cm", dpi = 200)

fig_brazil_new_deaths_pop <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  dplyr::mutate(newDeaths_per_100k_inhabitants = newDeaths/(sum(info$pop2020)/1e5),
                newDeaths_per_100k_inhabitants_rollmean = zoo::rollmean(newDeaths_per_100k_inhabitants, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = date, y = newDeaths_per_100k_inhabitants), size = .2, 
            color = "gray50", linetype = 2) +
  geom_point(aes(x = date, y = newDeaths_per_100k_inhabitants), size = 3, color = "white", 
             fill = "gray50", shape = 21, stroke = 1, alpha = .95) +
  geom_line(aes(x = date, y = newDeaths_per_100k_inhabitants_rollmean), 
            size = 1.5, color = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos óbitos (por 100 mil hab.)",
       title = "Número de novos óbitos no Brasil (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_brazil_new_deaths_pop
ggsave(filename = "graficos/fig_brazil_new_deaths_pop.png", 
       plot = fig_brazil_new_deaths_pop, width = 30, height = 20, units = "cm", dpi = 200)

fig_state_sp_new_cases_pop <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "SP") %>% 
  dplyr::mutate(newCases_per_100k_inhabitants_rollmean = zoo::rollmean(newCases_per_100k_inhabitants, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = date, y = newCases_per_100k_inhabitants), size = .2, 
            col = "red", linetype = 2) +
  geom_point(aes(x = date, y = newCases_per_100k_inhabitants), size = 3, 
             col = "white", fill = "red", shape = 21, stroke = 1) +
  geom_line(aes(x = date, y = newCases_per_100k_inhabitants_rollmean), 
            size = 1.5, col = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos casos (por 100 mil hab.)",
       title = "Número de novos casos - São Paulo (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_state_sp_new_cases_pop
ggsave(filename = "graficos/fig_state_sp_new_cases_pop.png", 
       plot = fig_state_sp_new_cases_pop, width = 25, height = 20, units = "cm", dpi = 200)

fig_state_sp_new_deaths_pop <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "SP") %>% 
  dplyr::mutate(newDeaths_per_100k_inhabitants_rollmean = zoo::rollmean(newDeaths_per_100k_inhabitants, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = date, y = newDeaths_per_100k_inhabitants), size = .2, 
            col = "gray60", linetype = 2) +
  geom_point(aes(x = date, y = newDeaths_per_100k_inhabitants), size = 3, 
             color = "white", fill = "gray60", shape = 21, stroke = 1) +
  geom_line(aes(x = date, y = newDeaths_per_100k_inhabitants_rollmean), 
            size = 1.5, col = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos óbitos (por 100 mil hab.)",
       title = "Número novos óbitos - São Paulo (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_state_sp_new_deaths_pop
ggsave(filename = "graficos/fig_state_sp_new_deaths_pop.png", 
       plot = fig_state_sp_new_deaths_pop, width = 25, height = 20, units = "cm", dpi = 200)

fig_planosp_piracicaba_new_cases_pop <- da_sp %>%
  dplyr::filter(nome_drs == "Piracicaba") %>% 
  dplyr::group_by(datahora) %>% 
  dplyr::summarise(casos_novos_sum_pc = sum(casos_novos)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(casos_novos_sum_pc_rollmean = zoo::rollmean(casos_novos_sum_pc, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = datahora, y = casos_novos_sum_pc),
            size = .2, col = "red", linetype = 2) +
  geom_point(aes(x = datahora, y = casos_novos_sum_pc),
             size = 3, col = "white", fill = "red", shape = 21, stroke = 1) +
  geom_line(aes(x = datahora, y = casos_novos_sum_pc_rollmean),
            size = 1.5, col = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos casos (por 100 mil hab.)",
       title = "Número de novos casos - Plano São Paulo - Região de Piracicaba (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_planosp_piracicaba_new_cases_pop
ggsave(filename = "graficos/fig_planosp_piracicaba_new_cases_pop.png", 
       plot = fig_planosp_piracicaba_new_cases_pop, width = 25, height = 20, units = "cm", dpi = 200)

fig_planosp_piracicaba_new_deaths_pop <- da_sp %>%
  dplyr::filter(nome_drs == "Piracicaba") %>% 
  dplyr::group_by(nome_drs, datahora) %>% 
  dplyr::summarise(obitos_novos_sum_pc = sum(obitos_novos_pc)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(obitos_novos_sum_pc_rollmean = zoo::rollmean(obitos_novos_sum_pc, k = 7, fill = NA)) %>%
  ggplot() +
  geom_line(aes(x = datahora, y = obitos_novos_sum_pc), 
            size = .2, col = "gray40", linetype = 2) +
  geom_point(aes(x = datahora, y = obitos_novos_sum_pc), 
             size = 3, color = "white", fill = "gray40", shape = 21, stroke = 1) +
  geom_line(aes(x = datahora, y = obitos_novos_sum_pc_rollmean), 
            size = 1.5, col = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos óbitos (por 100 mil hab.)",
       title = "Número de novos óbitos - Plano São Paulo - Região de Piracicaba (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_planosp_piracicaba_new_deaths_pop
ggsave(filename = "graficos/fig_planosp_piracicaba_new_deaths_pop.png", 
       plot = fig_planosp_piracicaba_new_deaths_pop, width = 25, height = 20, units = "cm", dpi = 200)

fig_rio_claro_new_cases_pop <- da_sp %>%
  dplyr::filter(nome_munic == "Rio Claro") %>% 
  dplyr::group_by(datahora) %>% 
  dplyr::summarise(newCases_pc = sum(casos_novos)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(newCases_per_100k_inhabitants_rollmean = zoo::rollmean(newCases_pc, k = 7, fill = NA)) %>% 
  ggplot() +
  geom_line(aes(x = datahora, y = newCases_pc), 
            size = .2, col = "red", linetype = 2) +
  geom_point(aes(x = datahora, y = newCases_pc),
             size = 3, col = "white", fill = "red", shape = 21, stroke = 1) +
  geom_line(aes(x = datahora, y = newCases_per_100k_inhabitants_rollmean), 
            size = 1.5, col = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos casos (por 100 mil hab.)",
       title = "Número de novos casos - Plano São Paulo - Rio Claro/SP (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_rio_claro_new_cases_pop
ggsave(filename = "graficos/fig_rio_claro_new_cases_pop.png", 
       plot = fig_rio_claro_new_cases_pop, width = 25, height = 20, units = "cm", dpi = 200)

fig_rio_claro_new_deaths_pop <- da_sp %>%
  dplyr::filter(nome_munic == "Rio Claro") %>% 
  dplyr::group_by(datahora) %>% 
  dplyr::summarise(newDeaths_pc = sum(obitos_novos)/(sum(pop2020)/1e5)) %>% 
  dplyr::mutate(newdeaths_per_100k_inhabitants_rollmean = zoo::rollmean(newDeaths_pc, k = 7, fill = NA)) %>% 
  ggplot() +
  geom_line(aes(x = datahora, y = newDeaths_pc), 
            size = .2, col = "gray40", linetype = 2) +
  geom_point(aes(x = datahora, y = newDeaths_pc), 
             size = 3, color = "white", fill = "gray40", shape = 21, stroke = 1) +
  geom_line(aes(x = datahora, y = newdeaths_per_100k_inhabitants_rollmean), 
            size = 1.5, col = "gray30", alpha = .8) +
  labs(x = "Data",
       y = "Número de novos óbitos (por 100 mil hab.)",
       title = "Número de novos óbitos - Plano São Paulo - Rio Claro/SP (por 100 mil hab.)") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = "none")
fig_rio_claro_new_deaths_pop
ggsave(filename = "graficos/fig_rio_claro_new_deaths_pop.png", 
       plot = fig_rio_claro_new_deaths_pop, width = 25, height = 20, units = "cm", dpi = 200)

# maps --------------------------------------------------------------------

# map brazil states uti ----
# map_brazil_states_utis <- uti_geo %>%
#   mutate(
#     data_coleta_char = as.character(format(data_coleta, "%d/%m/%Y")),
#     data_coleta_char = fct_reorder(data_coleta_char, data_coleta)
#   ) %>%
#   ggplot() +
#   geom_sf(aes(fill = alerta), color = "white", lwd = .2) +
#   scale_fill_manual(values=c("Baixo" = "#55a95a", "Médio" = "#f4b132", "Crítico" = "#ca373c"), 
#                     breaks = c("Baixo", "Médio", "Crítico")) +
#   labs(title = "Taxa de ocupação (%) de leitos de UTI Covid-19 para adultos", 
#        fill = "Alerta", caption = "Observatório Covid-19 | Fiocruz") +
#   ylab("") + xlab("") +
#   theme_bw() +
#   theme(
#     legend.position="bottom",
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank()
#   ) +
#   facet_wrap(~ data_coleta_char) 
# ggsave(map_brazil_states_utis, filename = "mapas/map_brazil_states_uti.png", dpi = 200)

# map brazil states cases ----
map_brazil_states_cases <-  sta_cases_geo %>% 
  sf::st_transform(crs = 4326) %>% 
  tm_shape() +
  tm_polygons(border.col = "gray40", col = "totalCases", palette = "Reds", textNA = "Sem registros",
              title = "Casos confirmados (total)", n = 5, style = "jenks") +
  tm_text(text = "totalCases", shadow = TRUE) +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_brazil_states_cases
tmap::tmap_save(tm = map_brazil_states_cases, 
                filename = "mapas/map_brazil_states_cases.png", dpi = 200)

# map brazil states cases pop ----
map_brazil_states_cases_pop <- sta_cases_geo %>% 
  dplyr::mutate(totalCases_per_100k_inhabitants = round(totalCases_per_100k_inhabitants, 1))  %>% 
  sf::st_transform(crs = 4326) %>% 
  tm_shape() +
  tm_polygons(border.col = "gray40", col = "totalCases_per_100k_inhabitants", palette = "Reds", textNA = "Sem registros",
              title = "Casos confirmados (por 100 mil hab.)", n = 5, style = "pretty") +
  tm_text(text = "totalCases_per_100k_inhabitants", shadow = TRUE) +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_brazil_states_cases_pop
tmap::tmap_save(tm = map_brazil_states_cases_pop, 
                filename = "mapas/map_brazil_states_cases_pop.png", dpi = 200)

# map brazil deaths ----
map_brazil_deaths <- sta_cases_geo %>%
  sf::st_transform(crs = 4326) %>% 
  tm_shape() +
  tm_polygons(border.col = "gray40", col = "deaths", palette = "Greys",
              textNA = "Sem registros",
              title = "Óbitos confirmadas", n = 5, style = "jenks") +
  tm_text(text = "deaths", shadow = TRUE) +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_brazil_deaths
tmap::tmap_save(map_brazil_deaths, "mapas/map_brazil_states_deaths.png", dpi = 200)

# map brazil states deaths pop ----
map_brazil_states_deaths_pop <- sta_cases_geo %>%
  sf::st_transform(crs = 4326) %>% 
  dplyr::mutate(deaths_per_100k_inhabitants = round(deaths_per_100k_inhabitants, 1)) %>% 
  tm_shape() +
  tm_polygons(border.col = "gray40", col = "deaths_per_100k_inhabitants", palette = "Greys",
              textNA = "Sem registros",
              title = "Óbitos confirmadas (por 100 mil hab.)", n = 5, style = "pretty") +
  tm_text(text = "deaths_per_100k_inhabitants", shadow = TRUE) +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_brazil_states_deaths_pop
tmap::tmap_save(tm = map_brazil_states_deaths_pop, 
                filename = "mapas/map_brazil_states_deaths_pop.png", dpi = 200)

# brazil municipality cases ----
map_brazil_muni_cases <- mun_cases_geo %>% 
  sf::st_transform(crs = 4326) %>% 
  tm_shape(bbox = sf::st_bbox(mun_cases_geo)) +
  tm_fill(col = "totalCases", palette = "Reds",
          textNA = "Sem registros", colorNA = "gray70",
          title = "Casos confirmados", n = 5, style = "jenks") +
  tm_borders(col = "gray30", lwd = .1) +
  tm_shape(sta_geo) +
  tm_borders(lwd = .5, col = "gray20") +
  tm_graticules(lines = FALSE) + 
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
tmap::tmap_save(tm = map_brazil_muni_cases, 
                filename = "mapas/map_brazil_muni_cases.png", dpi = 200)

# brazil municipality cases pop ----
map_brazil_muni_cases_pop <- mun_cases_geo %>% 
  sf::st_transform(crs = 4326) %>% 
  dplyr::mutate(totalCases_per_100k_inhabitants = round(totalCases_per_100k_inhabitants, 1)) %>% 
  tm_shape(., bbox = sf::st_bbox(mun_cases_geo)) +
  tm_fill(col = "totalCases_per_100k_inhabitants", palette = "Reds",
          textNA = "Sem registros", colorNA = "gray70",
          title = "Casos confirmados (por 100 mil hab.)", n = 5, style = "pretty") +
  tm_borders(col = "gray30", lwd = .1) +
  tm_shape(sta_geo) +
  tm_borders(lwd = .5, col = "gray20") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
tmap::tmap_save(tm = map_brazil_muni_cases_pop, 
                filename = "mapas/map_brazil_muni_cases_pop.png", dpi = 200)

# brazil municipality deaths ----
map_brazil_muni_deaths <- mun_cases_geo %>% 
  sf::st_transform(crs = 4326) %>% 
  tm_shape(bbox = sf::st_bbox(mun_cases_geo)) +
  tm_fill(col = "deaths", palette = "Greys",
          textNA = "Sem registros", colorNA = "white",
          title = "Óbitos confirmadas", n = 5, style = "jenks") +
  tm_borders(col = "gray30", lwd = .1) +
  tm_shape(sta_geo) +
  tm_borders(lwd = .5, col = "gray20") +
  tm_graticules(lines = FALSE) + 
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
tmap::tmap_save(tm = map_brazil_muni_deaths, 
                filename = "mapas/map_brazil_muni_deaths.png", dpi = 200)

# brazil municipality deaths pop ----
map_brazil_muni_deaths_pop <- mun_cases_geo %>% 
  sf::st_transform(crs = 4326) %>% 
  dplyr::mutate(totalCases_per_100k_inhabitants = round(totalCases_per_100k_inhabitants, 1)) %>% 
  tm_shape(., bbox = sf::st_bbox(mun_cases_geo)) +
  tm_fill(col = "deaths_per_100k_inhabitants", palette = "Greys",
          textNA = "Sem registros", colorNA = "white",
          title = "Óbitos confirmadas (por 100 mil hab.)", n = 5, style = "pretty") +
  tm_borders(col = "gray30", lwd = .1) +
  tm_shape(sta_geo) +
  tm_borders(lwd = .5, col = "gray20") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .35),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
tmap::tmap_save(tm = map_brazil_muni_deaths_pop, 
                filename = "mapas/map_brazil_muni_deaths_pop.png", dpi = 200)

# map state cases----
da_state <- data.frame(
  abbrev_state = mun_geo$abbrev_state %>% unique %>% sort,
  legend_h = c("left", "left", "left", "left", "left", "right", "left", "left", "left", "right",
               "left", "left", "right", "right", "right", "left", "left", "right", "left", "left",
               "left", "left", "left", "left", "right", "left", "left"),
  legend_v = c("bottom", "bottom", "top", "bottom", "bottom", "bottom", "bottom", "top", "top", "bottom",
               "top", "bottom", "top", "bottom", "bottom", "bottom", "top", "top", "top", "top",
               "bottom", "bottom", "bottom", "bottom", "top", "bottom", "top"),
  compass_h = c("right", "right", "right", "right", "right", "right", "right", "right", "right", "right",
                "right", "right", "right", "right", "right", "right", "right", "right", "right", "right",
                "right", "right", "right", "left", "right", "right", "right"),
  compass_v = c("top", "bottom", "top", "top", "top", "top", "top", "bottom", "bottom", "top",
                "top", "top", "top", "top", "top", "top", "bottom", "bottom", "top", "top",
                "top", "top", "top", "bottom", "bottom", "top", "top"),
  scalebar_h = c("left", "right", "right", "right", "left", "left", "left", "right", "right", "left",
                 "left", "right", "left", "left", "left", "right", "right", "right", "right", "right",
                 "right", "right", "right", "left", "right", "right", "right"),
  scalebar_v = c("bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom",
                 "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom",
                 "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom"))
da_state <- as.matrix(da_state)
da_state

for(i in mun_geo$abbrev_state %>% unique %>% seq){
  
  # information
  print(da_state[i, 1])
  
  # filter data
  mun_cases_geo_st <- mun_cases_geo %>%
    dplyr::filter(abbrev_state.x == da_state[i, 1])
  
  # map
  map_st <- mun_cases_geo_st %>%
    sf::st_transform(crs = 4326) %>% 
    tm_shape() +
    tm_polygons(border.col = "gray40", col = "totalCases_per_100k_inhabitants", palette = "Reds", textNA = "Sem registros",
                title = "Casos confirmados (por 100 mil hab.)", n = 5, style = "pretty") +
    tm_graticules(lines = FALSE) +
    tm_compass(size = 2.5, position = c(da_state[i, 4], da_state[i, 5])) +
    tm_scale_bar(text.size = .8, position = c(da_state[i, 6], da_state[i, 7])) +
    tm_layout(title = paste0(da_state[i, 1]),
              legend.position = c(da_state[i, 2], da_state[i, 3])) +
    tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br")
  map_st
  tmap::tmap_save(map_st, paste0("mapas/map_brazil_cases_pop_", stringr::str_to_lower(da_state[i, 1]), ".png"), dpi = 200)
  
}

# map state deaths ----
for(i in mun_geo$abbrev_state %>% unique %>% seq){
  
  # information
  print(da_state[i, 1])
  
  # filter data
  mun_cases_geo_st <- mun_cases_geo %>%
    dplyr::filter(abbrev_state.x == da_state[i, 1])
  
  # map
  map_st <- mun_cases_geo_st %>%
    sf::st_transform(crs = 4326) %>% 
    tm_shape() +
    tm_polygons(border.col = "gray40", col = "deaths_per_100k_inhabitants", 
                palette = "Greys", textNA = "Sem registros",
                title = "Óbitos confirmadas (por 100 mil hab.)", n = 5, style = "pretty") +
    tm_graticules(lines = FALSE) +
    tm_compass(size = 2.5, position = c(da_state[i, 4], da_state[i, 5])) +
    tm_scale_bar(text.size = .8, position = c(da_state[i, 6], da_state[i, 7])) +
    tm_layout(title = paste0(da_state[i, 1]),
              legend.position = c(da_state[i, 2], da_state[i, 3])) +
    tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br")
  map_st
  tmap::tmap_save(map_st, paste0("mapas/map_brazil_deaths_pop_", stringr::str_to_lower(da_state[i, 1]), ".png"), dpi = 200)
  
}

# models ------------------------------------------------------------------
# model state ----
model_state_cases_deaths <- sta_cases %>%
  dplyr::filter(abbrev_state != "TOTAL") %>%
  ggplot() +
  aes(x = totalCases, y = deaths) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(size = 4, fill = "black", col = "gray", shape = 21, alpha = .7) +
  geom_text_repel(aes(label = abbrev_state)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  labs(x = "Total de casos (log10)", y = "Óbitos (log10)") +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
model_state_cases_deaths
ggsave(filename = "modelos/model_states_cases_deaths.png", 
       plot = model_state_cases_deaths, width = 30, height = 20, units = "cm", dpi = 200)

# model state pop ----
model_state_cases_deaths_pop <- sta_cases %>%
  dplyr::filter(abbrev_state != "TOTAL") %>%
  ggplot() +
  aes(x = totalCases_per_100k_inhabitants, y = deaths_per_100k_inhabitants) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(size = 4, fill = "black", col = "gray", shape = 21, alpha = .7) +
  geom_text_repel(aes(label = abbrev_state)) +
  theme_bw() +
  labs(x = "Total de casos (por 100 mil hab.)", y = "Óbitos (por 100 mil hab.)") +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
model_state_cases_deaths_pop
ggsave(filename = "modelos/model_states_cases_deaths_pop.png", 
       plot = model_state_cases_deaths_pop, width = 30, height = 20, units = "cm", dpi = 200)

# model municipality ----
model_muni_cases_deaths <- mun_cases %>%
  dplyr::filter(deaths > 0, name_muni != "Caso Sem Localização Definida") %>%
  ggplot() +
  aes(x = totalCases, y = deaths) +
  geom_point(size = 3, fill = "black", col = "gray", shape = 21, alpha = .7) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_text_repel(data = mun_cases %>% 
                    dplyr::filter(deaths > 500, name_muni != "Caso Sem Localização Definida"), 
                  aes(label = paste0(name_muni, " (", abbrev_state, ")"))) +
  labs(x = "Total de casos (log10)", y = "Óbitos (log10)", 
       title = "Relação do número de óbitos (log10) e de casos (log10) acima de 100 óbitos") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  labs(x = "Total de casos (log10)", y = "Óbitos (log10)") +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(angle = 90, hjust = .5))
model_muni_cases_deaths
ggsave(filename = "modelos/model_muni_cases_deaths.png", 
       plot = model_muni_cases_deaths, width = 30, height = 20, units = "cm", dpi = 200)

# model municipality pop ----
model_muni_cases_deaths_pop <- mun_cases %>%
  dplyr::filter(deaths > 0 | deaths_per_100k_inhabitants > 200 | totalCases_per_100k_inhabitants >= 6000, 
                name_muni != "Caso Sem Localização Definida") %>%
  ggplot() +
  aes(x = totalCases_per_100k_inhabitants, y = deaths_per_100k_inhabitants) +
  geom_point(size = 3, fill = "black", col = "gray", shape = 21, alpha = .7) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_text_repel(data = mun_cases %>% 
                    dplyr::filter(deaths_per_100k_inhabitants > 200 | 
                                    totalCases_per_100k_inhabitants >= 20000, name_muni != "Caso Sem Localização Definida"), 
                  aes(label = paste0(name_muni, " (", abbrev_state, ")"))) +
  labs(x = "Total de casos (por 100 mil hab.) (log10)", y = "Óbitos (por 100 mil hab.)",
       title = "Relação do número de óbitos (por 100 mil hab.) e de casos (por 100 mil hab.) acima de 50 óbitos por 100 mil hab.") +
  theme_bw() +
  labs(x = "Total de casos (por 100 mil hab.)", y = "Óbitos (por 100 mil hab.)") +
  theme(axis.title = element_text(size = 15),
        legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = .5))
model_muni_cases_deaths_pop
ggsave(filename = "modelos/model_muni_cases_deaths_pop.png", 
       plot = model_muni_cases_deaths_pop, width = 30, height = 20, units = "cm", dpi = 200)

## Scatter new deats/cases per 100k per days ---
model_state_cases_deaths_rate <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  ggplot() +
  aes(x = date, 
      y = deaths_by_totalCases) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(size = 4, fill = "black", col = "gray", shape = 21, alpha = .7) +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  stat_smooth(method = "gam") +
  theme_bw() +
  theme(axis.title = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(angle = 90, vjust = .5)) +  
  ylim(0, .07) + 
  labs(x = "Data", 
       y = "Novos mortes/novos casos")
model_state_cases_deaths_rate
ggsave(filename = "modelos/model_cases_deaths_rate.png", 
       plot = model_state_cases_deaths_rate, width = 30, height = 20, units = "cm", dpi = 200)

## sus data ----------------------------------------------------------------
# # sus
# sus <- readr::read_csv("https://raw.githubusercontent.com/JoaoCarabetta/SimulaCovid/master/data/raw/covid19_SUS_database.csv")
# dplyr::glimpse(sus)
# 
# sta_sus <- sus %>%
#   dplyr::select(-(1:4)) %>%
#   dplyr::rename(abbrev_state = uf) %>%
#   dplyr::group_by(abbrev_state) %>%
#   dplyr::summarise_all(~sum(., na.rm = TRUE))
# dplyr::glimpse(sta_sus)
# 
# mun_sus <- sus %>%
#   dplyr::rename(name_muni = municipio,
#                 abbrev_state = uf) %>%
#   dplyr::select(-(1:3)) %>%
#   dplyr::group_by(name_muni, abbrev_state) %>%
#   dplyr::summarise_all(~sum(., na.rm = TRUE))
# dplyr::glimpse(mun_sus)

# # state leitos
# map_sta_leitos <- sta_cases_sus_geo %>%
#   tm_shape() +
#   tm_polygons(border.col = "gray40", col = "quantidade_leitos", palette = "Blues",
#               textNA = "Sem registros",
#               title = "Quantidade de leitos", n = 5, style = "pretty") +
#   tm_text(text = "quantidade_leitos", shadow = TRUE) +
#   tm_graticules(lines = FALSE) +
#   tm_compass(position = c(.75, .08)) +
#   tm_scale_bar(text.size = .8, position = c(.65, .02)) +
#   tm_layout(title = paste0("Quantidade leitos no \n Brasil por Estado"),
#             title.position = c(.7, .9),
#             title.size = 1) +
#   tm_credits("Fonte: https://github.com/JoaoCarabetta/SimulaCovid", position = c(.57, 0))
# map_sta_leitos
# tmap::tmap_save(map_sta_dea, "mapas/leitos_brasil_estados.png", dpi = 200)
#
# # state ventiladores
# map_sta_vent <- sta_cases_sus_geo %>%
#   tm_shape() +
#   tm_polygons(border.col = "gray40", col = "ventiladores_existentes", palette = "Greens",
#               textNA = "Sem registros",
#               title = "Ventiladores existentes", n = 5, style = "pretty") +
#   tm_text(text = "ventiladores_existentes", shadow = TRUE) +
#   tm_graticules(lines = FALSE) +
#   tm_compass(position = c(.75, .08)) +
#   tm_scale_bar(text.size = .8, position = c(.65, .02)) +
#   tm_layout(title = paste0("Quantidade de ventinadores no \n Brasil por Estado"),
#             title.position = c(.7, .9),
#             title.size = 1) +
#   tm_credits("Fonte: https://github.com/JoaoCarabetta/SimulaCovid", position = c(.57, 0))
# map_sta_vent
# tmap::tmap_save(map_sta_vent, "mapas/vent_brasil_estados.png", dpi = 200)

# in time -----------------------------------------------------------------
# # state map time
# map_sta_total_time <- tm_shape(sta_geo, bbox = sf::st_bbox(sta_geo)) +
#   tm_polygons() +
#   tm_shape(sta_cases_sus_time_geo) +
#   tm_symbols(size = "totalCases_pop", scale = 2, title.size = "Casos por estado (por 100 mil hab.)",
#              alpha = .5, col = "red", border.col = "darkred") +
#   tm_facets(along = "date") +
#   tm_text(text = "totalCases_pop") +
#   tm_graticules(lines = FALSE) +
#   tm_compass(position = c(.73, .08)) +
#   tm_scale_bar(text.size = .6, position = c(.6, .02)) +
#   tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n ao longo dos dias",
#             title.position = c(.7, .9),
#             title.size = 1,
#             legend.position = c("left", "bottom")) +
#   tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
# tmap::tmap_animation(tm = map_sta_total_time, filename = "gifs/covid19_brasil_estados_evolucao.gif",
#                      wi = 2000, he = 2000, delay = 30)
# # magick::image_read("gifs/covid19_brasil_estados_evolucao.gif")
#
#
# # municipality map time
# map_mun_total_time <- tm_shape(sta_geo, bbox = sf::st_bbox(sta_geo)) +
#   tm_polygons() +
#   tm_shape(mun_cases_sus_time_geo) +
#   tm_symbols(size = "totalCases_pop", scale = 2, title.size = "Casos por municípios (por 100 mil hab.)",
#              alpha = .5, col = "red", border.col = "darkred") +
#   tm_facets(along = "date") +
#   tm_graticules(lines = FALSE) +
#   tm_compass(position = c(.73, .08)) +
#   tm_scale_bar(text.size = .6, position = c(.6, .02)) +
#   tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n ao longo dos dias",
#             title.position = c(.7, .9),
#             title.size = 1,
#             legend.position = c("left", "bottom")) +
#   tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
# tmap::tmap_animation(tm = map_mun_total_time, filename = "gifs/covid19_brasil_municipios_evolucao.gif",
#                      wi = 2000, he = 2000, delay = 50)
# # magick::image_read("gifs/covid19_brasil_municipios_evolucao.gif")

# end ---------------------------------------------------------------------