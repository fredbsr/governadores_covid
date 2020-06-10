# leitura de dados
remotes::install_github("liibre/coronabr")
library(coronabr)
library(googlesheets4)
library(tidyverse)

covid <- get_corona_br(by_uf = TRUE)
covid_semana <- covid %>%
  mutate(Semana = lubridate::week(date),
         Estado = state) %>%
  group_by(Semana,Estado) %>%
  summarise(Casos=mean(confirmed,na.rm=T),
            Casos_100k = mean(confirmed_per_100k_inhabitants,na.rm=T),
            Mortes=mean(deaths,na.rm=T))
  
  
dados <- "https://docs.google.com/spreadsheets/d/1uZuSbqxywEwJiF4uBnc4yofIE9EH8oFG2maqhVfOrWk/edit#gid=1846588799"
df <- googlesheets4::read_sheet(dados,sheet = 3,range = "B2:AC985",
                                col_types = "cDcccccccccccccccccccccccccc")

df %>%
  mutate(Semana = lubridate::week(Data)) %>%
  group_by(Semana,Estado) %>%
  summarise(Normas=n()) %>%
  drop_na() %>%
  ggplot(aes(x=Semana,fill=Estado)) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA) +
 # geom_
  facet_wrap(~Estado) +
  theme_minimal() +
  theme(legend.position = "none")

covid_semana %>%
  ggplot(aes(x=Semana,color=Estado,y=Casos_100k)) +
  geom_line() +
  # geom_
  facet_wrap(~Estado) +
  theme_minimal() +
  theme(legend.position = "none")

left_join(df %>%
            mutate(Semana = lubridate::week(Data)) %>%
            dplyr::filter(!is.na(Estado)) %>%
            group_by(Semana,Estado) %>%
            summarise(Normas=n()),covid_semana) %>%
  ggplot(aes(x=Semana,fill=Estado,color=Estado)) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA,alpha=.3) +
  geom_line(aes(y=Casos_100k/50)) +
  scale_y_continuous(
    name = "Normas",
    sec.axis = sec_axis( trans=~.*50, name="Confirmados por 100k")
  ) +
  facet_wrap(~Estado) +
  theme_minimal() +
  theme(legend.position = "none")

  