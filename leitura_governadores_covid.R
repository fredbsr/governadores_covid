# leitura de dados
#remotes::install_github("liibre/coronabr")
devtools::install_github("tbrugz/ribge")
library(ribge)
library(coronabr)
library(googlesheets4)
library(tidyverse)
library(geobr)
library(rvest)
library(sf)

# Regiões e estadosbrasil
estados <- read_state()
# Codificação das regiões
cods_ibge <- estados %>% 
  dplyr::select("code_state",UF="abbrev_state",
                "code_region","name_region") 
st_geometry(cods_ibge) <- NULL

# Estado
pop <- ribge::populacao_municipios(2019) 

# Governadores
gov_url <- "https://pt.wikipedia.org/wiki/Lista_de_governadores_das_unidades_federativas_do_Brasil_(2019%E2%80%932023)"
govs <- gov_url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  dplyr::select(-bandeira) %>%
  rename(Estado=unidade_federativa,
         UF=abreviacao) %>%
# ideologia e orientacao 
   mutate(
    ideologia = case_when(
      partido %in% c("DEM","MDB","NOVO",
                     "PHS\nDEM","PP","PSC",
                     "PSD","PSDB","PSL") ~ "Direita",
      partido %in% c("PCdoB","PDT","PSB",
                     "PSB\nCidadania [2]",
                     "PT") ~ "Esquerda"
    ),
    federalismo = case_when(
      UF %in% c("PR","SC","RO","RR","MG",
                     "GO","MT","TO") ~ "Convergente",
      TRUE ~ "Divergente"
    ),
    federalismo2 = case_when(
      UF %in% c("AC","AM","DF","MG","PR","RO","RR") ~ "Convergente",
      TRUE ~ "Divergente"
    ),
    orientacao = paste(ideologia,federalismo)
  ) %>%
  left_join(cods_ibge) %>%
  left_join(pop %>%
              group_by(code_state=codigo_uf) %>%
              summarise(populacao = sum(populacao,na.rm=T))) %>% 
  dplyr::select(Estado,UF,#partido,
                ideologia,federalismo,orientacao,
                UF_codigo=code_state,Regiao=name_region,populacao )

rm(cods_ibge,estados,pop,gov_url)

# Carta 17.fev
# "GO","MT","PR","RO","RR","SC","TO"
# Leia mais em: https://www.gazetadopovo.com.br/parana/breves/ratinho-nao-assina-carta-governadores-criticas-bolsonaro/

# Carta 19.abr outros não assinaram
# "AC","AM","DF","MG","PR","RO","RR"
# https://www.em.com.br/app/noticia/politica/2020/04/19/interna_politica,1140171/zema-nao-assina-carta-de-governadores-contra-bolsonaro.shtml

# SRAG
esperados_localidade <- "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/valores_esperados_por_localidade_sem_filtro_febre.csv"
SRAG_esp <- read_csv2(esperados_localidade) %>% 
  janitor::clean_names() %>%
  rename(populacao = populacao_de_referencia_para_calculo_de_incidencia)

SRAG_esp_join <- SRAG_esp %>% 
  dplyr::filter(tipo=="Estado",
                dado=="srag",
                escala == "casos") %>%
  transmute(UF_codigo = uf,
            Semana=semana_epidemiologica,
            media_casos=corredor_mediano
  )
  

serie <- "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_febre.csv"
SRAG <- read_csv2(serie) %>% 
  janitor::clean_names() %>%
  rename(reportado=total_reportado_ate_a_ultima_atualizacao)

SRAG_join <- SRAG %>% 
  dplyr::filter(tipo=="Estado",
                dado=="srag",
                escala == "casos",
                ano_epidemiologico %in% c("2020"),
                semana_epidemiologica<22) %>% 
  mutate(code_region=as.numeric(str_sub(uf,1,1))) %>%
  left_join(regioes_cod) %>%
  transmute(Regiao=name_region,
            UF=unidade_da_federacao,
            UF_codigo = uf,
                Semana=semana_epidemiologica,
                casos=ifelse(is.na(casos_estimados)==F,casos_estimados,reportado),
              populacao_datasus=populacao,
              ) %>%
  arrange(UF,UF_codigo,Semana)  %>%
  left_join(SRAG_esp_join) %>%
  mutate(excedente = casos - media_casos) %>%
  arrange(UF,Semana) %>%
  group_by(UF) %>%
  mutate(acumulado = cumsum(casos),
         acumulado_ex = cumsum(excedente))


SRAG_join %>%
  ggplot(aes(x=Semana,
             y=acumulado_ex*100000/populacao_datasus,color=UF,
             fill=UF)) + 
  geom_line() +
    facet_wrap(vars(UF)) +
  theme(legend.position = "none")

# Dados de covid por estado ----
covid <- get_corona_br(by_uf = TRUE) %>% 
  rename(UF_codigo = city_ibge_code,
         UF = state)

# Dados por semana pidemiológica
covid_semana <- covid %>%
  mutate(Semana = lubridate::epiweek(date)) %>%
  group_by(UF,UF_codigo,Semana) %>%
  summarise(Casos=mean(confirmed,na.rm=T),
            Casos_100k = mean(confirmed_per_100k_inhabitants,na.rm=T),
            Mortes=mean(deaths,na.rm=T)) %>% # left_join(SRAG_join) %>%
            dplyr::filter(Semana<22)
# Gráfico
covid_semana %>% 
  left_join(govs) %>%
  ggplot(aes(x=Semana,group,color=UF,y=Casos_100k)) +
  geom_line() +
  # geom_
  facet_grid(Regiao~orientacao) +
  theme_minimal() +
  theme(legend.position = "none")


# Dados de testagem
testes_url <- "https://g1.globo.com/bemestar/coronavirus/noticia/2020/06/10/veja-taxa-de-ocupacao-nas-utis-testes-feitos-e-pacientes-recuperados-da-covid-19-em-cada-estado-do-brasil.ghtml"
path_url <- '/html/body/div[2]/main/div[4]/article/div[3]/div[7]/div/div/div/div[1]/table'
testes <- testes_url %>%
  read_html() %>%
  html_nodes('table') %>%
  html_node(xpath = path_url) 
testagem <- testes[[1]] %>%
  html_table(header=T) %>%
  janitor::clean_names() %>%
  rename(testes = nº_de_testes,
         Estado = estado) %>%
  mutate(testes=as.numeric(gsub("\\.","",testes))) %>% 
  dplyr::filter(Estado != "Total") 


testagem %>% left_join(govs) %>%
  mutate(tx_teste = testes*10^6/populacao) %>%
  ggplot(aes(fill=Regiao,x=reorder(UF,desc(-tx_teste)),y=tx_teste)) +
  geom_bar(stat = "identity") +
  labs(y="Testes por milhão de habitante",x="UF",fill="") +
  theme_minimal() +
  facet_grid(.~orientacao,space="free",scales = "free") +
  theme(legend.position = "bottom")

# Normas relacionadas à Saúde ----
normas_plan <- "https://docs.google.com/spreadsheets/d/1uZuSbqxywEwJiF4uBnc4yofIE9EH8oFG2maqhVfOrWk/edit#gid=1846588799"
# normas <- googlesheets4::read_sheet(normas_plan,sheet = 3,range = "B2:AC985",
#                                 col_types = "cDcccccccccccccccccccccccccc") 

normas_data <- googlesheets4::read_sheet(normas_plan,sheet = 3,range = "B2:C985",
                                    col_types = "cD") %>%
  rename(UF=Estado)

normas <- normas_data   %>% 
  left_join(govs) 

# Normas por estado gráfico
normas %>%
  mutate(Semana = lubridate::epiweek(Data)) %>%
  group_by(Semana,Estado,Regiao) %>%
  summarise(Normas=n()) %>%
  dplyr::filter(Semana<22) %>%
  drop_na() %>%
  ggplot(aes(x=Semana,fill=Estado)) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA) +
 # geom_
  facet_wrap(vars(Estado)) +
  theme_minimal() +
  theme(legend.position = "none")

full_join(normas %>% # se for usar todos os dados de covid
#left_join(normas %>%
            mutate(Semana = lubridate::epiweek(Data)) %>%
            dplyr::filter(!is.na(Estado)) %>%
            group_by(Semana,UF,Regiao) %>%
            summarise(Normas=n(),
                      populacao = mean(populacao,na.rm = T),
                      Dia = last(Data)),covid_semana) -> covid_normas

covid_normas %>%
  dplyr::filter(Semana<22) %>%
  ggplot(aes(x=Semana,fill=UF,color=UF)) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA,alpha=.5) +
  geom_line(aes(y=Casos_100k/50)) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~.*50, name="Confirmados por 100k (linhas)")
  ) +
  facet_wrap(~UF) +
  theme_minimal() +
  theme(legend.position = "none")

# Normas acumuladas por semana e por região
covid_normas %>%
  group_by(Semana,UF,Regiao) %>%
  summarise(Normas = sum(Normas,na.rm=T),
            pop = mean(populacao,na.rm=T),
            Casos = sum(Casos,na.rm=T),
            Mortes = sum(Mortes,na.rm=T),
            Dia = last(Dia)) %>%
  group_by(Semana,Regiao) %>%
  summarise(Normas = mean(Normas,na.rm=T),
            pop = sum(pop,na.rm=T),
            Casos = sum(Casos,na.rm=T),
            Mortes = sum(Mortes,na.rm=T),
            Dia = last(Dia)) %>%
  mutate(Casos_100k = Casos*100000/pop,
         Mortos_milhao = Mortes*10^6/pop) %>%
  group_by(Regiao) %>%
  mutate(Normas_acumuladas = cumsum(Normas)) %>%
  dplyr::filter(Semana<22) %>%
  ggplot(aes(x=Semana,color=Regiao,fill=Regiao,group=Regiao)) +
  geom_bar(aes(y=Normas_acumuladas),stat = "identity",color=NA,alpha=.5) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA,alpha=.5) +
  geom_line(aes(y=Casos_100k*15),size=1) +
  geom_hline(data=.%>% 
               group_by(Regiao) %>% 
               summarise(Normas = mean(Normas,na.rm=T)),
             aes(yintercept=Normas,color=Regiao,group=Regiao),
   size=2,alpha=.5
    ) +
  facet_wrap(vars(Regiao),nrow = 1) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~./15, name="Casos confirmados/100k hab (linhas)")
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Normas acumuladas por semana e por diálogo com Presidente
covid_normas <- normas %>%
            mutate(Semana = lubridate::epiweek(Data)) %>%
            dplyr::filter(!is.na(Estado)) %>%
            group_by(Semana,Estado,orientacao) %>%
            summarise(Normas=n(),
                      Dia = last(Data)) %>%
            ungroup() %>%
            left_join(covid_semana %>% 
                        ungroup() %>%
                        dplyr::select(-pop,-UF_codigo)) %>%
            left_join(covid_semana %>%
                        ungroup() %>%
              dplyr::select(Estado,pop) %>% distinct())

covid_normas %>%
  group_by(Semana,Estado,orientacao) %>%
  summarise(Normas = sum(Normas,na.rm=T),
            pop = sum(pop,na.rm=T),
            Casos = sum(Casos,na.rm=T),
            Mortes = sum(Mortes,na.rm=T),
            Dia = last(Dia)) %>%
  group_by(Semana,orientacao) %>%
  summarise(Normas = mean(Normas,na.rm=T),
            pop = sum(pop,na.rm=T),
            Casos = sum(Casos,na.rm=T),
            Mortes = sum(Mortes,na.rm=T),
            Dia = last(Dia)) %>%
  mutate(Casos_100k = Casos*100000/pop,
         Mortos_milhao = Mortes*10^6/pop) %>%
  group_by(orientacao) %>%
  mutate(Normas_acumuladas = cumsum(Normas)) %>% data.frame() -> covid_normas_fig
  dplyr::filter(Semana<22) %>%
  ggplot(aes(x=Semana,color=orientacao,fill=orientacao,group=orientacao)) +
  geom_bar(aes(y=Normas_acumuladas),stat = "identity",color=NA,alpha=.5) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA,alpha=.5,fill="grey50") +
  geom_line(aes(y=Casos_100k*15),size=1) +
  geom_hline(data=.%>% 
               group_by(orientacao) %>% 
               summarise(Normas = mean(Normas,na.rm=T)),
             aes(yintercept=Normas,color=orientacao,
                 group=orientacao),
             size=2,alpha=.5,color="grey50") +
  facet_wrap(vars(orientacao),nrow = 1) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~./15, name="Casos confirmados/100k hab (linhas)")
  ) +
  
  theme_minimal() +
  theme(legend.position = "none")



