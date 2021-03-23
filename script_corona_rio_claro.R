# packages
library(tidyverse)
library(lubridate)

# directory
setwd("rio_claro")

# data ----
da <- readr::read_csv("Relatório - Evolução COVID-19 em Rio Claro_SP_Tabela com dados diários_Tabela.csv") %>% 
  dplyr::mutate(Data = str_replace_all(Data, " ", "-"),
                Data = str_replace(Data, "jan.", "01"),
                Data = str_replace(Data, "fev.", "02"),
                Data = str_replace(Data, "mar.", "03"),
                Data = str_replace(Data, "abr.", "04"),
                Data = str_replace(Data, "mai.", "05"),
                Data = str_replace(Data, "jun.", "06"),
                Data = str_replace(Data, "jul.", "07"),
                Data = str_replace(Data, "ago.", "08"),
                Data = str_replace(Data, "set.", "09"),
                Data = str_replace(Data, "out.", "10"),
                Data = str_replace(Data, "nov.", "11"),
                Data = str_replace(Data, "dez.", "12"),
                Data = str_replace_all(Data, "-de-", "-"),
                Data = dmy(Data))
da

glimpse(da)

# rt ----
fig_rt <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(Rt)), col = "steelblue", size = 1) +
  geom_point(aes(x = Data, y = as.numeric(Rt)), size = 4, shape = 20, col = "steelblue", alpha = .5) +
  labs(x = "Data",
       y = "Rt",
       title = "Índice contágio (Rt)") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_rt
ggsave(filename = "fig_rt.png", 
       plot = fig_rt, width = 30, height = 20, units = "cm", dpi = 200)

# Casos 7 últimos dias ----
fig_casos <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(`Casos 7 ultimos dias`)), col = "steelblue", size = 1) +
  geom_point(aes(x = Data, y = as.numeric(`Casos 7 ultimos dias`)), size = 4, shape = 20, col = "steelblue", alpha = .5) +
  labs(x = "Data",
       y = "Casos 7 últimos dias",
       title = "Número de novos casos nos 7 últimos dias") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_casos
ggsave(filename = "fig_casos.png", 
       plot = fig_casos, width = 30, height = 20, units = "cm", dpi = 200)

# Internações 7 últimos ----
fig_inter <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(`Internações 7 últimos`)), col = "steelblue", size = 1) +
  geom_point(aes(x = Data, y = as.numeric(`Internações 7 últimos`)), size = 4, shape = 20, col = "steelblue", alpha = .5) +
  labs(x = "Data",
       y = "Internações 7 últimos dias",
       title = "Número de internações nos 7 últimos dias") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_inter
ggsave(filename = "fig_inter.png", 
       plot = fig_inter, width = 30, height = 20, units = "cm", dpi = 200)

# `UTI 7 últimos`  ----
fig_uti <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(`UTI 7 últimos`)), col = "steelblue", size = 1) +
  geom_point(aes(x = Data, y = as.numeric(`UTI 7 últimos`)), size = 4, shape = 20, col = "steelblue", alpha = .5) +
  labs(x = "Data",
       y = "UTI 7 últimos dias",
       title = "Número de internados na UTI nos 7 últimos dias") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_uti
ggsave(filename = "fig_uti.png", 
       plot = fig_uti, width = 30, height = 20, units = "cm", dpi = 200)

# Óbitos 7 últimos ----
fig_obitos <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(`Óbitos 7 últimos`)), col = "steelblue", size = 1) +
  geom_point(aes(x = Data, y = as.numeric(`Óbitos 7 últimos`)), size = 4, shape = 20, col = "steelblue", alpha = .5) +
  labs(x = "Data",
       y = "Óbitos 7 últimos dias",
       title = "Número de novos óbitos nos 7 últimos dias") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_obitos
ggsave(filename = "fig_obitos.png", 
       plot = fig_obitos, width = 30, height = 20, units = "cm", dpi = 200)

# end ---------------------------------------------------------------------