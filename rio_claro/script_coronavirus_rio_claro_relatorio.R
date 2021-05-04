# dados
# "https://datastudio.google.com/u/0/reporting/44b18def-5c65-4a1b-976b-2fcc55bfabe1/page/9xg1B"

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
  geom_line(aes(x = Data, y = as.numeric(Rt)), color = "red", size = 1) +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = .35, size = 5, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = .35, size = 5, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = .3, size = 5, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = .3, size = 5, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  geom_hline(yintercept = 2, color = "gray30", size = 1, linetype = 2) +
  geom_hline(yintercept = 1, color = "gray", size = 1, linetype = 2) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(x = "Data",
       y = "Rt",
       title = "Número de reprodução (Rt)") +
  ylim(0, 3) +
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
  geom_line(aes(x = Data, y = as.numeric(`Casos 7 ultimos dias`)), color = "steelblue", size = 1) +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = 17, size = 5, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = 17, size = 5, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = 14, size = 5, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = 14, size = 5, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(x = "Data",
       y = "Média de casos dos últimos 7 dias",
       title = "Número de novos casos") +
  ylim(0, 150) +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_casos
ggsave(filename = "fig_casos.png", 
       plot = fig_casos, width = 30, height = 20, units = "cm", dpi = 200)

# Casos ativos 7 últimos dias ----
fig_casos_ativos <- da %>%
  dplyr::mutate(casos_ativos = as.numeric(ifelse(`Casos ativos` == "null", NA, `Casos ativos`))) %>% 
  ggplot() +
  geom_line(aes(x = Data, y = zoo::rollmean(casos_ativos, k = 7, fill = NA)), color = "orange4", size = 1) +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = 135, size = 5, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = 135, size = 5, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = 125, size = 5, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = 125, size = 5, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(x = "Data",
       y = "Média de casos ativos dos últimos 7 dias",
       title = "Número de novos casos ativos") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_casos_ativos
ggsave(filename = "fig_casos_ativos.png", 
       plot = fig_casos_ativos, width = 30, height = 20, units = "cm", dpi = 200)

# Internações SARG 7 últimos ----
fig_inter <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(`Internações 7 últimos`)), 
            color = "steelblue", size = 1) +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = 23, size = 5, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = 23, size = 5, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = 20, size = 5, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = 20, size = 5, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  ylim(0, 200) +
  labs(x = "Data",
       y = "Média de internações dos últimos 7 dias",
       title = "Número de internações (SARG - Síndrome respiratória aguda grave)") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_inter
ggsave(filename = "fig_inter_sarg.png", 
       plot = fig_inter, width = 30, height = 20, units = "cm", dpi = 200)

# `UTI 7 últimos`  ----
fig_uti <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(`UTI 7 últimos`)), color = "red", size = 1) +
  geom_hline(yintercept = 71, color = "red", size = .7, linetype = 2) +
  geom_label(aes(x = da$Data[320], y = 75), fill = "red", size = 5, 
                 label = "Número total de vagas disponíveis nas UTIs", alpha = .7) +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = 10, size = 5, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = 10, size = 5, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = 9, size = 5, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = 9, size = 5, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  labs(x = "Data",
       y = "Média de internações dos últimos 7 dias",
       title = "Número de internados (UTI)") +
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_uti
ggsave(filename = "fig_inter_uti.png", 
       plot = fig_uti, width = 30, height = 20, units = "cm", dpi = 200)

# Óbitos 7 últimos ----
fig_obitos <- da %>%
  dplyr::filter(Óbitos != "null") %>% 
  dplyr::mutate(obitos7d = zoo::rollmean(as.numeric(Óbitos), k = 7, fill = NA)) %>% 
  ggplot() +
  geom_line(aes(x = Data, y = obitos7d), color = "gray30", size = 1) +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = .5, size = 5, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = .5, size = 5, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = .45, size = 5, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = .45, size = 5, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(x = "Data",
       y = "Média de óbitos dos 7 últimos dias",
       title = "Número de novos óbitos") +

  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_obitos
ggsave(filename = "fig_obitos.png", 
       plot = fig_obitos, width = 30, height = 20, units = "cm", dpi = 200)

# Óbitos acumulados 7 últimos ----
fig_obitos_total <- da %>%
  ggplot() +
  geom_line(aes(x = Data, y = as.numeric(`Óbitos 7 últimos`)), color = "gray30", size = 1) +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = 3.5, size = 5, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = 3.5, size = 5, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = 3, size = 5, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = 3, size = 5, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(x = "Data",
       y = "Total de óbitos dos 7 últimos dias",
       title = "Número acumulado de óbitos acumulados nos 7 últimos dias") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_obitos_total
ggsave(filename = "fig_obitos_total.png", 
       plot = fig_obitos_total, width = 30, height = 20, units = "cm", dpi = 200)

# internacoes -------------------------------------------------------------
# publica ----
da_pu <- readr::read_csv("Relatório - Evolução COVID-19 em Rio Claro_SP_Ocupação diária de leitos_Série temporal.csv") %>% 
  dplyr::mutate(date = str_replace_all(date, " ", "-"),
                date = str_replace(date, "jan.", "01"),
                date = str_replace(date, "fev.", "02"),
                date = str_replace(date, "mar.", "03"),
                date = str_replace(date, "abr.", "04"),
                date = str_replace(date, "mai.", "05"),
                date = str_replace(date, "jun.", "06"),
                date = str_replace(date, "jul.", "07"),
                date = str_replace(date, "ago.", "08"),
                date = str_replace(date, "set.", "09"),
                date = str_replace(date, "out.", "10"),
                date = str_replace(date, "nov.", "11"),
                date = str_replace(date, "dez.", "12"),
                date = str_replace_all(date, "-de-", "-"),
                date = dmy(date))
da_pu

glimpse(da_pu)

# internacao publica ----
fig_inter_pub <- da_pu %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(`Total de leitos públicos`)), color = "gray10", size = 1) +
  geom_line(aes(x = date, y = as.numeric(`Enfermaria Público`)), color = "steelblue", size = 1) +
  geom_line(aes(x = date, y = as.numeric(`UTI Publico` )), color = "red", size = 1) +
  
  geom_hline(yintercept = 67, color = "gray10", linetype = 3) +
  geom_hline(yintercept = 40, color = "red", linetype = 3) +
  geom_hline(yintercept = 27, color = "steelblue", linetype = 3) +
  
  geom_label(aes(x = da_pu$date[15], y = 70), size = 5, color = "gray10", label = "Total disponíveis") +
  geom_label(aes(x = da_pu$date[14], y = 43), size = 5, color = "red", label = "UTIs disponível") +
  geom_label(aes(x = da_pu$date[27], y = 30), size = 5, color = "steelblue", label = "Enfermaria disponíveis") +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = 7, size = 4, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = 7, size = 4, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = 7, size = 4, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = 7, size = 4, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(x = "Data",
       y = "Leitos ocupados",
       title = "Leitos públicos ocupados") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_inter_pub

ggsave(filename = "fig_inter_pub.png", 
       plot = fig_inter_pub, width = 30, height = 20, units = "cm", dpi = 200)

# privada ----
da_pr <- readr::read_csv("Relatório - Evolução COVID-19 em Rio Claro_SP_Ocupação diária de leitos_Série temporal(1).csv") %>% 
  dplyr::mutate(date = str_replace_all(date, " ", "-"),
                date = str_replace(date, "jan.", "01"),
                date = str_replace(date, "fev.", "02"),
                date = str_replace(date, "mar.", "03"),
                date = str_replace(date, "abr.", "04"),
                date = str_replace(date, "mai.", "05"),
                date = str_replace(date, "jun.", "06"),
                date = str_replace(date, "jul.", "07"),
                date = str_replace(date, "ago.", "08"),
                date = str_replace(date, "set.", "09"),
                date = str_replace(date, "out.", "10"),
                date = str_replace(date, "nov.", "11"),
                date = str_replace(date, "dez.", "12"),
                date = str_replace_all(date, "-de-", "-"),
                date = dmy(date))
da_pr

glimpse(da_pr)

# internacao publica ----
fig_inter_pri <- da_pr %>%
  ggplot() +
  geom_line(aes(x = date, y = as.numeric(`Total de leitos particulares`)), color = "gray10", size = 1) +
  geom_line(aes(x = date, y = as.numeric(`Enfermaria Particular`)), color = "steelblue", size = 1) +
  geom_line(aes(x = date, y = as.numeric(`UTI Particular` )), color = "red", size = 1) +
  
  geom_hline(yintercept = 88, color = "gray10", linetype = 3) +
  geom_hline(yintercept = 31, color = "red", linetype = 3) +
  geom_hline(yintercept = 57, color = "steelblue", linetype = 3) +
  
  geom_label(aes(x = da_pr$date[15], y = 91), size = 5, color = "gray10", label = "Total disponíveis") +
  geom_label(aes(x = da_pr$date[14], y = 34), size = 5, color = "red", label = "UTIs disponível") +
  geom_label(aes(x = da_pr$date[25], y = 60), size = 5, color = "steelblue", label = "Enfermaria disponíveis") +
  
  geom_vline(xintercept = as_date("2021-03-15"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-03-11"), y = 10, size = 4, color = "red",
           label = "Início Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-11"), color = "red", linetype = 3) +
  annotate("text", x = as_date("2021-04-16"), y = 10, size = 4, color = "red",
           label = "Final Fase Emer. SP", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-03-26"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-03-22"), y = 9, size = 4, color = "blue",
           label = "Início Restrição RC", alpha = .7, angle = 90) +
  
  geom_vline(xintercept = as_date("2021-04-05"), color = "blue", linetype = 3) +
  annotate("text", x = as_date("2021-04-01"), y = 9, size = 4, color = "blue",
           label = "Final Restrição RC", alpha = .7, angle = 90) +
  
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m") +
  labs(x = "Data",
       y = "Leitos ocupados",
       title = "Leitos particulares ocupados") +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90, vjust = .5),
        axis.text.y = element_text(size = 15),
        legend.position = "none")
fig_inter_pri

ggsave(filename = "fig_inter_pri.png", 
       plot = fig_inter_pri, width = 30, height = 20, units = "cm", dpi = 200)

# end ---------------------------------------------------------------------