# packages
library(tidyverse)
library(readxl)
library(lubridate)

# directory
setwd("rio_claro/internacoes")

# import data
da <- readxl::read_excel("20210413 Internacoes.xlsx") %>% 
  dplyr::rename_with(tolower) %>%
  dplyr::rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("ç", "c", .x, fixed = TRUE)) %>%
  dplyr::rename_with(~ gsub("ã", "a", .x, fixed = TRUE)) %>% 
  dplyr::rename_with(~ gsub("ó", "o", .x, fixed = TRUE))
da

# filter data
da_na_data <- da %>% 
  tidyr::drop_na(data_entrada, data_da_situacao, situacao_cor, covid, nomehospital, enfuti) %>% 
  dplyr::filter(idade > 0) %>% 
  dplyr::select(-c(semana_epidemiologica, hosp))
da_na_data

# data
da_date <- tibble::tibble(data_entrada = lubridate::ymd("2020-03-09") + lubridate::days(0:400))
da_date

# total
da_na_data_table_total <- da_na_data %>% 
  dplyr::group_by(data_entrada, covid) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(da_date, .) %>% 
  dplyr::mutate(count = ifelse(is.na(count) == TRUE, 0, count)) %>% 
  dplyr::mutate(count_mean_7d = zoo::rollmean(count, k = 7, fill = NA),
                data_entrada = lubridate::as_date(data_entrada))
da_na_data_table_total

# internacao
da_na_data_table_enfuti <- da_na_data %>% 
  dplyr::group_by(data_entrada, covid, enfuti) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(da_date, .) %>% 
  dplyr::mutate(count = ifelse(is.na(count) == TRUE, 0, count)) %>% 
  dplyr::mutate(count_mean_7d = zoo::rollmean(count, k = 7, fill = NA),
                data_entrada = lubridate::as_date(data_entrada))
da_na_data_table_enfuti

# graficos ----------------------------------------------------------------

# total
da_na_data_table_total %>% 
  dplyr::filter(covid == "positivo") %>% 
  ggplot() +
  geom_point(aes(x = data_entrada, y = count), color = "steelblue", size = 4, alpha = .5) +
  geom_line(aes(x = data_entrada, y = count_mean_7d), color = "steelblue", size = 1.5) +
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(title = "Novas internações por COVID-19 em todos os hospitais de Rio Claro/SP",
       x = "Data de entrada", 
       y = "Número de novas internações") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
ggsave("fig_total.png", wi = 40, he = 20, un = "cm", dpi = 300)

# enfuti
da_na_data_table_enfuti %>% 
  tidyr::drop_na(enfuti) %>% 
  dplyr::filter(covid == "positivo",
                ) %>% 
  ggplot() +
 #  geom_point(aes(x = data_entrada, y = count), color = "red", size = 4, alpha = .5) +
  geom_line(aes(x = data_entrada, y = count_mean_7d), color = "red", size = 1.2) +
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  facet_wrap(~enfuti, nrow = 2) +
  labs(title = "Novas internações por dia por COVID-19 nos hospitais de Rio Claro/SP", 
       x = "Data de entrada", 
       y = "Número de novas internações") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 20),
        legend.position = "none")
ggsave("fig_enfuti.png", wi = 40, he = 20, un = "cm", dpi = 300)

# enfuti
da_na_data_table_enfuti %>% 
  tidyr::drop_na(enfuti) %>% 
  ggplot() +
  #  geom_point(aes(x = data_entrada, y = count), color = "red", size = 4, alpha = .5) +
  geom_line(aes(x = data_entrada, y = count_mean_7d), color = "red", size = 1.2) +
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  facet_wrap(~enfuti, nrow = 2) +
  labs(title = "Novas internações por dia por COVID-19 nos hospitais de Rio Claro/SP", 
       x = "Data de entrada", 
       y = "Número de novas internações") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 20),
        legend.position = "none")
ggsave("fig_enfuti.png", wi = 40, he = 20, un = "cm", dpi = 300)


# tabelas -----------------------------------------------------------------

# obitos
da_na_data %>% 
  dplyr::mutate(covid = str_to_title(covid),
                situacao_cor = str_to_title(situacao_cor)) %>% 
  dplyr::group_by(covid, enfuti, situacao_cor) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(p = round(n/sum(n)*100, 2)) %>% 
  readr::write_csv("obitos.csv")

# obitos por idade
da_na_data %>% 
  dplyr::mutate(faixa_etaria = sub("FE ", "", faixa_etaria),
                covid = str_to_title(covid),
                situacao_cor = str_to_title(situacao_cor)) %>% 
  dplyr::group_by(covid, enfuti, faixa_etaria, situacao_cor) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(p = round(n/sum(n)*100, 2)) %>% 
  readr::write_csv("obitos_idade.csv")

# end ---------------------------------------------------------------------
