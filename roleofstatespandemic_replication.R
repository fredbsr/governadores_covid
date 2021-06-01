# leitura de dados
#remotes::install_github("liibre/coronabr")
#devtools::install_github("tbrugz/ribge")
library(ribge)
library(coronabr)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(magrittr)
library(geobr)
library(rvest)
library(sf)
library(textclean)
library(data.table)
library(patchwork)
library(tsibble)
library(readr)

# Regions and states ----
estados <- read_state() 

# Regions codification
cods_ibge <- estados %>% 
  dplyr::select(codigo_uf="code_state",UF="abbrev_state",
                "code_region",Regiao="name_region",Estado.IBGE="name_state") %>%
  mutate(Estado.IBGE=mgsub(Estado.IBGE,c(" Da "," De "," Do "),c(" da "," de "," do ")))
cods_ibge -> maps_ibge # saving object with geomwty
st_geometry(cods_ibge) <- NULL

# States
cods_ibge %<>% left_join(ribge::populacao_municipios(2019) %>%
                           group_by(codigo_uf) %>%
                           summarise(populacao = sum(populacao,na.rm=T)))


# Governors ----
# Wikipedia url
gov_url <- "https://pt.wikipedia.org/wiki/Lista_de_governadores_das_unidades_federativas_do_Brasil_(2019%E2%80%932023)"

govs <- gov_url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  dplyr::select(-bandeira,-unidade_federativa) %>%
  rename(UF=abreviacao) %>%
  # ideology and orientation 
  mutate(
    partido = mgsub(partido,c("PSB\nCidadania [2]","PHS\nDEM","Sem Partido","Cidadania [2]"),
                    c("Cidadania","DEM","PSL","Cidadania")),
    partido = gsub("PSL\nPSL","PSL",partido),
    ideologia = case_when(
      partido %in% c("DEM","MDB","NOVO",
                     "DEM","PP","PSC","PL",
                     "PSD","PSDB","PSL") ~ "Right-wing",
      partido %in% c("PCdoB","PDT","PSB",
                     "Cidadania",
                     "PT") ~ "Left-wing"
    ),

    partido_join = str_to_lower(partido),
    federalismo = case_when(
      UF %in% c("AC","AM","DF","MG","PR",
                "RO","RR","RJ","SC") ~ "Aligned",
      TRUE ~ "Non-aligned"
    ),
    federalismo1 = case_when(
      UF %in% c("PR","SC","RO","RR","MG",
                "GO","MT","TO") ~ "Aligned",
      TRUE ~ "Non-aligned"
    ),
    federalismo2 = case_when(
      UF %in% c("AC","AM","DF","MG","PR","RO","RR") ~ "Aligned",
      TRUE ~ "Non-aligned"
    ),
    federalismo3 = case_when(
      partido %in% c("NOVO","PSC","PSL","PL") ~ "Aligned",
      partido %in% c("DEM","MDB",
                     "DEM","PP",
                     "PSD","PSDB","PCdoB","PDT","PSB",
                     "Cidadania",
                     "PT") ~ "Non-aligned"
    ),
    
    # Carta 17.fev
    # "GO","MT","PR","RO","RR","SC","TO"
    # Leia mais em: https://www.gazetadopovo.com.br/parana/breves/ratinho-nao-assina-carta-governadores-criticas-bolsonaro/
    
    # Carta 19.abr outros não assinaram
    # "AC","AM","DF","MG","PR","RO","RR"
    # https://www.em.com.br/app/noticia/politica/2020/04/19/interna_politica,1140171/zema-nao-assina-carta-de-governadores-contra-bolsonaro.shtml
    
    orientacao = paste(ideologia,federalismo),
    orientacao1 = paste(ideologia,federalismo1),
    orientacao2 = paste(ideologia,federalismo2),
    orientacao3 = paste(ideologia,federalismo3)
  ) %>%
  dplyr::select(UF,partido,partido_join,ideologia,
                federalismo,federalismo1,federalismo2,federalismo3,
                orientacao,orientacao1,orientacao2,orientacao3) 

# Time Windows definition ----
# Norms and times windows file
normas_plan <- "https://docs.google.com/spreadsheets/d/1uZuSbqxywEwJiF4uBnc4yofIE9EH8oFG2maqhVfOrWk/edit#gid=1846588799"


janelas_analise <- googlesheets4::read_sheet(normas_plan,sheet = 6,range = "A1:E5") 

janelas_join <- janelas_analise %>%
  pivot_longer(start:end,names_to="Fase",values_to="Semana")

janelas_full <- data.frame(Semana=6:53) %>%
  left_join(janelas_join) %>%
  fill(Stage:t.)


# Norms ----
normas_data <- bind_cols(
  googlesheets4::read_sheet(normas_plan,sheet = 3,range = "A1:O3759",
                            col_types = "cDDcccccccccccc"),
  googlesheets4::read_sheet(normas_plan,sheet = 3,range = "P1:AD3759",
                            col_types = "ccccccccccccccc") 
) %>%
  janitor::clean_names() %>%
  mutate_at(vars(9:30),as.numeric) %>%
  mutate_if(is.numeric,replace_na,replace = 0) %>%
  rename(UF=estado) %>%

# Filtering year 2020
  dplyr::filter(lubridate::year(data)==2020)

# Expanding grid to account for every week
normas <- 
  expand_grid(UF=levels(factor(normas_data$UF)),
              Semana = 1:53) %>% full_join(
                normas_data %>%
                  mutate(Semana = lubridate::epiweek(data)) %>%
                  group_by(Semana,UF) %>%
                  summarise(Normas=n()) 
              ) %>%
  replace_na(list(Normas = 0)) %>%
  group_by(UF) %>%
  mutate(Normas_acum=cumsum(Normas))



## Norms by theme and week
normas_tema_semana <- normas_data %>%
  mutate(UF=UF,
         data=data,
         "Processos de serviços de saúde" = diretrizes_de_tratamento+
           reorientacao_da_provisao_de_servicos+
           outros_processos,
         
         "Recursos Humanos" = aumentar_o_numero_de_profissionais_de_saude+
           medidas_de_protecao_dos_profissionais_de_saude+
           treinamento_dos_profissionais_de_saude+
           outros_rh,
         
         "Ações de vigilância epidemiológica" = identificacao_de_casos+
           isolamento_de_casos_positivos+
           outros_vigilancia,
         
         "Infraestrutura" = aumento_da_capacidade_da_urgencia_e_emergencia+
           aumento_da_capacidade_de_internacao+
           aumento_da_capacidade_da_atencao_basica+
           outros_infraestrutura,
         
         "Relações intergovernamentais" = 
           estabelece_questoes_envolvendo_outros_entes,
         
         "Gastos" = aquisicao_de_insumo+
           realocacao_de_recurso+
           aumento_de_recurso+
           contratacao_de_pessoal+
           investimento_em_infraestrutura+
           outros_gastos
         
  ) %>%
  janitor::clean_names() %>%
  rename(UF=uf) %>%
  mutate(Semana = lubridate::epiweek(data))  %>% 
  drop_na(data) %>%
  
  # Grouping by week
  group_by(UF,Semana=as.factor(Semana)) %>%
  summarise_if(is.numeric,sum,na.rm=T) %>%
  ungroup() %>%
  arrange(UF,Semana)  %>%
  mutate(Semana=as.numeric(as.character(Semana))) %>%
  right_join(expand_grid(UF=levels(factor(normas_data$UF)),
                         Semana = 1:53) ) %>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) 



normas_tema_acum <- normas_tema_semana %>%
  group_by(UF) %>%
  dplyr::select(-tipo_de_normativa_ver_tabela) %>%
  mutate_at(vars(processos_de_servicos_de_saude,
                 recursos_humanos,acoes_de_vigilancia_epidemiologica,
                 infraestrutura,relacoes_intergovernamentais,
                 gastos,
                 
                 #"Processos de serviços de saúde" = 
                 diretrizes_de_tratamento,
                 reorientacao_da_provisao_de_servicos,
                 outros_processos,
                 
                 #"Recursos Humanos" = 
                 aumentar_o_numero_de_profissionais_de_saude,
                 medidas_de_protecao_dos_profissionais_de_saude,
                 treinamento_dos_profissionais_de_saude,
                 outros_rh,
                 
                 #"Ações de vigilância epidemiológica" = 
                 identificacao_de_casos,
                 isolamento_de_casos_positivos,
                 outros_vigilancia,
                 
                 #"Infraestrutura" = 
                 aumento_da_capacidade_da_urgencia_e_emergencia,
                 aumento_da_capacidade_de_internacao,
                 aumento_da_capacidade_da_atencao_basica,
                 outros_infraestrutura,
                 
                 #"Relações intergovernamentais" = 
                 estabelece_questoes_envolvendo_outros_entes,
                 
                 #"Gastos" = 
                 aquisicao_de_insumo,
                 realocacao_de_recurso,
                 aumento_de_recurso,
                 contratacao_de_pessoal,
                 investimento_em_infraestrutura,
                 outros_gastos
                 
                 
  ),cumsum) 


normas_tema_semana_long <- normas_tema_semana %>%
  dplyr::select(UF,Semana,
                processos_de_servicos_de_saude,
                recursos_humanos,acoes_de_vigilancia_epidemiologica,
                infraestrutura,relacoes_intergovernamentais,
                gastos) %>%
  pivot_longer(-c(UF,Semana)) %>%
  replace_na(list(value = 0)) %>%
  mutate(name=mgsub(name,
                    c('processos_de_servicos_de_saude',
                      'recursos_humanos',
                      'acoes_de_vigilancia_epidemiologica',
                      'infraestrutura',
                      'relacoes_intergovernamentais',
                      'gastos'),
                    c("Health Services Working Processes",
                      "Human Resources",
                      "Epidemiological Surveillance",
                      "Infrastructure",
                      "Intergovernmental relations",
                      "Spending")))


# Cumsum norms by theme
normas_tema_acum_long <- normas_tema_acum %>%
  dplyr::select(UF,Semana,
                processos_de_servicos_de_saude,
                recursos_humanos,acoes_de_vigilancia_epidemiologica,
                infraestrutura,relacoes_intergovernamentais,
                gastos) %>%
  pivot_longer(-c(UF,Semana)) %>%
  replace_na(list(value = 0)) %>%
  mutate(name=mgsub(name,
                    c('processos_de_servicos_de_saude',
                      'recursos_humanos',
                      'acoes_de_vigilancia_epidemiologica',
                      'infraestrutura',
                      'relacoes_intergovernamentais',
                      'gastos'),
                    c("Health Services Working Processes",
                      "Human Resources",
                      "Epidemiological Surveillance",
                      "Infrastructure",
                      "Intergovernmental relations",
                      "Spending"))) 

# All norms of 2020
norms_sum_2020 <- normas_tema_acum_long %>%
  #Selecting last week of times windows
  dplyr::filter(Semana==max(normas_tema_acum_long$Semana)) %>%
  # excluindo o valor de semana
  dplyr::select(-Semana) %>%
  group_by(UF) %>% 
  mutate(pct=prop.table(value),
         normas=sum(value,na.rm=T)) %>%
  ungroup()

# norms by time windows
norms_tw <- normas_tema_semana_long %>% 
  left_join(janelas_full) %>%
  dplyr::filter(Semana>5) %>%
  group_by(UF,name,Stage,tn) %>% 
  summarise(value=sum(value,na.rm=T)) %>%
  ungroup() %>%
  group_by(UF,Stage) %>%
  mutate(pct=prop.table(value),
         normas=sum(value,na.rm=T)) %>%
  # bringing back week
  left_join(janelas_join)


# COVID-19 Data ----

# COVID Brasil.io ----
br.io_raw <- fread("https://data.brasil.io/dataset/covid19/caso_full.csv.gz") %>%
  mutate(date=ymd(date))

br.io <- br.io_raw %>% 
  dplyr::filter(place_type=="state") %>% 
  mutate(ano=lubridate::epiyear(date),
         Semana=epiweek(date),
         ano_semana=tsibble::yearweek(paste(ano,Semana,sep = " W"))) %>%
  group_by(UF=state,ano,Semana,ano_semana) %>%
  summarise(casos=sum(new_confirmed,na.rm=T),
            obitos=sum(new_deaths,na.rm=T)) %>% 
  ungroup() %>%
  arrange(ano_semana) %>%
  group_by(UF) %>%
  mutate(casos=forecast::ma(casos,7),
         obitos=forecast::ma(obitos,7),
         casos_acum=cumsum(casos),
         obitos_acum=cumsum(obitos)) %>%
  ungroup() %>%
  left_join(cods_ibge) %>%
  mutate(casos_100k=casos*10^5/populacao,
         casos_acum_100k=casos_acum*10^5/populacao,
         obitos_milhao = obitos*10^6/populacao,
         obitos_acum_milhao = obitos_acum*10^6/populacao
  ) %>%
  arrange(codigo_uf,ano_semana) %>%
  dplyr::filter(ano==2020)


# retrieving data from 
wcota.data <- "https://github.com/wcota/covid19br/raw/master/cases-brazil-states.csv"
covid_states <- read_delim(wcota.data, delim=",",
                                col_types = cols(country = col_skip(), 
                                                 city = col_skip(), recovered = col_skip(), 
                                                 suspects = col_skip(), tests = col_double(), 
                                                 tests_per_100k_inhabitants = col_double(), 
                                                 vaccinated = col_double(),
                                                 vaccinated_per_100k_inhabitants = col_double(),
                                                 vaccinated_second = col_double(),
                                                 vaccinated_second_per_100k_inhabitants = col_double()), 
                                locale = locale(encoding = "ISO-8859-1")) %>%
  dplyr::filter(state !="TOTAL") %>%
  dplyr::rename(UF=state) %>%
  dplyr::filter(lubridate::year(date)==2020)

covid_states_week <- covid_states  %>%
  mutate(Semana = lubridate::epiweek(date)) %>%
  group_by(UF,Semana) %>%
  summarise(
       vars(
            deaths_per_100k_inhabitants,
            totalCases_per_100k_inhabitants,
            deaths_by_totalCases,
            tests_per_100k_inhabitants),mean,na.rm=T) %>%
  ungroup()


# SARS InfoGripe----
raw = "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/dados_semanais_faixa_etaria_sexo_virus_sem_filtro_sintomas.csv"

SRAG_raw <- 
  read_delim(raw,";",escape_double = FALSE, 
             col_types = cols(`Idade desconhecida` = col_skip(),
                              `< 2 anos` = col_skip(), `0-4 anos` = col_skip(), 
                              `10-19 anos` = col_skip(), `2-4 anos` = col_skip(), 
                              `20-29 anos` = col_skip(), `30-39 anos` = col_skip(), 
                              `40-49 anos` = col_skip(), `5-9 anos` = col_skip(), 
                              `50-59 anos` = col_skip(), `60+ anos` = col_skip(), 
                              `Testes positivos` = col_skip(), 
                              `Testes negativos` = col_skip(), 
                              `Casos aguardando resultado` = col_skip(), 
                              `Casos sem informação laboratorial` = col_skip(), 
                              `Casos sem teste laboratorial` = col_skip(), 
                              `Resultado inconclusivo` = col_skip(), 
                              `Influenza A` = col_skip(), `Influenza B` = col_skip(), 
                              `SARS-CoV-2` = col_skip(), `Vírus sincicial respiratório (VSR)` = col_skip(), 
                              `Parainfluenza 1` = col_skip(), `Parainfluenza 2` = col_skip(), 
                              `Parainfluenza 3` = col_skip(), `Parainfluenza 4` = col_skip(), 
                              Adenovirus = col_skip(), Rinovirus = col_skip(), 
                              Bocavirus = col_skip(), Metapneumovirus = col_skip(), 
                              `Outros virus` = col_skip()), 
             locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>%
  janitor::clean_names() 

# write_rds(SRAG_raw,"SRAG_raw.rds"); rm(SRAG_raw,raw)
# SRAG_raw <- read_rds("SRAG_raw.rds")

SRAG <- 
  bind_rows(
    SRAG_raw %>% dplyr::select(tipo,escala,sexo,
                               dado,uf,unidade_da_federacao,
                               semana_epidemiologica,ano_epidemiologico,
                               casos_semanais_reportados_ate_a_ultima_atualizacao) %>%
      dplyr::filter(tipo=="Estado",escala=="casos",sexo=="Total",#semana_epidemiologica<22,
                    dado %in% c("srag","sragcovid"),
                    ano_epidemiologico!=2021) %>%
      group_by(codigo_uf=uf,unidade_da_federacao,semana_epidemiologica,ano_epidemiologico,dado) %>%
      summarise(casos=sum(casos_semanais_reportados_ate_a_ultima_atualizacao,
                          #total_reportado_ate_a_ultima_atualizacao,## mudou o nome da variavel
                          na.rm = T)) %>%
      ungroup() %>%
      mutate(ano_2020 = (ano_epidemiologico==2020)) %>%
      group_by(codigo_uf,unidade_da_federacao,semana_epidemiologica,ano_2020,dado) %>%
      summarise(casos=median(casos,na.rm = T)) %>%
      ungroup() %>%
      unite("dado_ano",ano_2020:dado) %>%
      pivot_wider(names_from = dado_ano,
                  values_from = casos)  %>%
      transmute(codigo_uf=codigo_uf,
                Estado=unidade_da_federacao,
                Semana=semana_epidemiologica,
                srag_excesso = TRUE_srag - FALSE_srag,
                srag_tot_2020 = TRUE_srag,
                covid = TRUE_sragcovid,
                covid_sub = srag_excesso - covid) %>%
      group_by(codigo_uf) %>%
      mutate(covid_acum=cumsum(covid),
             covid_sub_acum=cumsum(covid_sub),
             srag_excesso_acum = cumsum(srag_excesso)) %>%
      ungroup() %>%
      left_join(cods_ibge) %>%
      mutate(covid_100k=covid*10^5/populacao,
             covid_sub_100k=covid_sub*10^5/populacao,
             covid_acum_100k=covid_acum*10^5/populacao,
             covid_sub_acum_100k=covid_sub_acum*10^5/populacao,
             srag_excesso_100k = srag_excesso*10^5/populacao,
             srag_excesso_acum_100k = srag_excesso_acum*10^5/populacao,
             perc_sub = 100*(srag_excesso - covid)/(srag_excesso)) %>%
      
      mutate(ano=2020),
    
    SRAG_raw %>% dplyr::select(tipo,escala,sexo,
                               dado,uf,unidade_da_federacao,
                               semana_epidemiologica,ano_epidemiologico,
                               casos_semanais_reportados_ate_a_ultima_atualizacao) %>%
      dplyr::filter(tipo=="Estado",escala=="casos",sexo=="Total",#semana_epidemiologica<22,
                    dado %in% c("srag","sragcovid"),
                    ano_epidemiologico!=2020) %>%
      group_by(codigo_uf=uf,unidade_da_federacao,semana_epidemiologica,ano_epidemiologico,dado) %>%
      summarise(casos=sum(casos_semanais_reportados_ate_a_ultima_atualizacao,
                          #total_reportado_ate_a_ultima_atualizacao,## mudou o nome da variavel
                          na.rm = T)) %>%
      ungroup() %>%
      mutate(ano_2020 = (ano_epidemiologico==2021)) %>%
      group_by(codigo_uf,unidade_da_federacao,semana_epidemiologica,ano_2020,dado) %>%
      summarise(casos=median(casos,na.rm = T)) %>%
      ungroup() %>%
      unite("dado_ano",ano_2020:dado) %>%
      pivot_wider(names_from = dado_ano,
                  values_from = casos) %>%
      transmute(codigo_uf=codigo_uf,
                Estado=unidade_da_federacao,
                Semana=semana_epidemiologica,
                srag_excesso = TRUE_srag - FALSE_srag,
                srag_tot_2020 = TRUE_srag,
                covid = TRUE_sragcovid,
                covid_sub = srag_excesso - covid) %>%
      group_by(codigo_uf) %>%
      mutate(covid_acum=cumsum(covid),
             covid_sub_acum=cumsum(covid_sub),
             srag_excesso_acum = cumsum(srag_excesso)) %>%
      ungroup() %>%
      left_join(cods_ibge) %>%
      mutate(covid_100k=covid*10^5/populacao,
             covid_sub_100k=covid_sub*10^5/populacao,
             covid_acum_100k=covid_acum*10^5/populacao,
             covid_sub_acum_100k=covid_sub_acum*10^5/populacao,
             srag_excesso_100k = srag_excesso*10^5/populacao,
             srag_excesso_acum_100k = srag_excesso_acum*10^5/populacao,
             perc_sub = 100*(srag_excesso - covid)/(srag_excesso)
      ) %>%
      
      mutate(ano=2021) %>% dplyr::filter(Semana<10) 
    # filtrando para semanas existentes em 2021
  ) %>% 
  mutate(
    ano_semana=tsibble::yearweek(paste(ano,Semana,sep = " W"))
  )



# Gathering all data ----

df.discrete <-
  covid_states_week %>% 
  dplyr::filter(Semana %in% janelas_analise$end) %>%
  left_join(cods_ibge) %>%
  left_join(govs) %>%
  left_join(janelas_full) %>%
  left_join(SRAG %>% dplyr::select(Semana,UF,perc_sub)) %>%
  left_join(normas_tema_acum_long %>%
              pivot_wider(names_from=name,values_from=value)) %>%
  janitor::clean_names()



df <-
  SRAG %>% 
  dplyr::filter(ano==2020) %>%
  left_join(br.io) %>%
  left_join(govs) %>%
  left_join(normas) %>%
  left_join(janelas_full)

# FIGURE 1 ----
bask1 <- cestas %>%
  
  mutate(UF = fct_reorder(UF,desc(UF))) %>%
  ggplot() +
  geom_bar(aes(fill=str_wrap(name,25),y=reorder(UF,normas),
               x=pct
  ),width = .6, alpha=.75,
  position = "stack",
  stat = "identity") +
  scale_x_continuous(labels=scales::percent) +
  labs(fill="",y="",x="") + 
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm"))


bask2 <- cestas %>%
  
  mutate(UF = fct_reorder(UF,desc(UF))) %>%
  ggplot() +
  geom_bar(fill="grey75",aes(y=reorder(UF,normas),
                             x=normas
  ),width = .6,
  position = "stack",
  stat = "identity") +
  labs(fill="",y="",x="") + 
  scale_x_reverse(expand = c(0,0)) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm"))


bask3 <- cestas %>% group_by(name) %>%
  summarise(n=sum(value,na.rm=T)) %>% ungroup() %>%
  mutate(perc=prop.table(n)
  ) %>%
  
  ggplot(aes(x=str_wrap(name,10),y=n,fill=name)) +
  geom_bar(width = .6,position = "dodge",stat = "identity", alpha=.75) +
  geom_text(aes(label=paste0(" ",n),y=0),angle=90,hjust=0,size=3.5,fontface="bold") +
  geom_text(aes(label=paste0(round(perc*100,0),"%")),vjust=-0.5,size=3.5,fontface="bold") +
  scale_y_continuous(limits = c(0,1800)) +
  labs(fill="",y="",x="") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm"))

bask2 + bask1 + bask3 + plot_layout(design = "
  333
  122
  122
  122
  122
  
")


# FIGURE 2 ----
# Time windows
SRAG %>%
  dplyr::filter(ano==2020) %>%
  group_by(Semana) %>%
  summarise(covid=sum(covid,na.rm=T),
            srag_excesso = sum(srag_excesso,na.rm=T),
            populacao=sum(populacao,na.rm=T)) %>%
  ungroup() %>%
  mutate(covid_100k=covid*10^5/populacao,
         srag_excesso_100k = srag_excesso*10^5/populacao
  ) %>%
  #  dplyr::filter(Semana<24) %>%
  ggplot(aes(x=Semana)) +
  geom_area(aes(y=srag_excesso,fill="Exceeding SARS"),alpha=.5) +
  geom_area(aes(y=covid,fill="COVID-19")) +
  
  annotate(geom="rect",fill=c("red","yellow","green","red"),
           xmin=janelas_analise$start-.5,
           xmax=janelas_analise$end+.5,
           ymin=0,
           ymax=Inf,alpha=.15) +
  annotate(geom="text",label=c("t1\n(w6 to w17)\nBeginning of first wave",
                               "t2\n(w6 to w17)\nPlateau",
                               "t3\n(w6 to w17)\nDecreasing",
                               "t4\n(w6 to w17)\nBeginning of second wave"),
           vjust=1,size=3,
           x=c(c(janelas_analise$start)+c(janelas_analise$end))/2,
           y=c(rep(40000,4))) +
  scale_x_continuous(breaks = c(seq(5,50,by=5))) +
  
  
  labs(fill="",y="New cases",x="",
       caption = "Source: InfoGripe,2020\n
                  (Exceeding SARS is the 2020 values 
                  excedding 2009-2019 median)") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.15, "lines"),
        panel.grid = element_line(size=.25,color = "grey75"),
        panel.grid.minor.x = element_blank(),
        plot.margin = ggplot2::margin(1, 1, 1, 1))  


# FIGURE 3 ----

df %>% mutate(orientacao=orientacao1) %>%
  dplyr::filter(Semana>6) %>%

   # group_by(Semana,orientacao) %>%
   # summarise(covid_sub_100k = mean(covid_sub_100k,na.rm=T),
   #           perc_sub = mean(perc_sub,na.rm=T)) %>%
  ggplot(aes(x=Semana,color=orientacao,fill=orientacao,y=perc_sub)) +
 
  annotate(geom="rect",fill=c("red","yellow","green","red"),
           xmin=janelas_analise$start-.5,
           xmax=janelas_analise$end+.5,
           ymin=0,
           ymax=Inf,alpha=.15) +
  annotate(geom="text",label=c("t1\n(w6 to w17)\nBeginning of first wave",
                               "t2\n(w6 to w17)\nPlateau",
                               "t3\n(w6 to w17)\nDecreasing",
                               "t4\n(w6 to w17)\nBeginning of second wave"),
           vjust=1,size=3,
           x=c(c(janelas_analise$start)+c(janelas_analise$end))/2,
           y=c(rep(120,4))) +
  
  geom_point(size=.35,alpha=.5) +
  geom_smooth(aes(group=orientacao),span=.25,
              level = 0.95) +
  
  labs(y = "% of underreporting",color="",fill="",x="Epidemiological week of 2020") +
  theme_minimal() +
  theme(legend.position = "top")

# FIGURE 4 ----
norms_tw %>%
  left_join(govs %>% dplyr::select(UF,orientacao1)) %>%
  group_by(orientacao1,tn,Stage,name) %>%
  summarise(n=mean(value,na.rm=T)) %>%
  mutate(perc=prop.table(n),
         sum=sum(n,na.rm=T)) %>%
  ggplot(aes(x=str_wrap(name,10),y=n,fill=name)) +
  geom_hline(aes(yintercept=sum,color=orientacao1)) +
  geom_bar(width = .6,position = "dodge",stat = "identity", alpha=.75) +
#  geom_text(aes(label=paste0(" ",round(n,1)),y=0),angle=90,hjust=0,size=3.5,fontface="bold") +
  geom_text(aes(label=paste0(round(perc*100,0),"%")),vjust=-0.5,size=3.5,fontface="bold") +
  #scale_y_continuous(limits = c(0,25)) +
  labs(fill="",y="",x="",color="") +
  facet_grid(cols = vars(orientacao1),
                         rows = vars(tn)#,switch = "y"
             ) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_blank(),
      #  axis.ticks.y = element_blank(),
        legend.position = "top",
        plot.margin = margin(0, 0, 0, 0, "cm"))


df.discrete %>%
  ggplot(aes(x=acoes_de_vigilancia_epidemiologica,y=deaths_by_total_cases)) +
  geom_point(aes(color=orientacao1)) + 
  geom_smooth(aes(group=orientacao1,color=orientacao1,fill=orientacao1),method="lm")
  
reg <- lm(data=df.discrete,deaths_by_total_cases ~ acoes_de_vigilancia_epidemiologica + gastos)  
summary(reg)

sjPlot::plot_residuals(reg)

norms_tw %>%
  arrange(desc(value)) %>%
  group_by(tn,UF) %>%
  slice(1:2) %>% mutate(imp=1:2) %>%
  ungroup() %>% 
  dplyr::select(UF,tn,name,imp) %>%
  pivot_wider(names_from = imp,values_from = name ) %>%
  
  left_join(govs %>% dplyr::select(UF,ideologia,federalismo1)) -> mca
names(mca)  

res.mca <- FactoMineR::MCA(mca %>% dplyr::select(-UF) ,graph = FALSE)

factoextra::fviz_mca(res.mca)

  ggplot() +
  geom_bar(aes(fill=str_wrap(name,25),x=UF,
               y=value
  ),width = .6, alpha=.75,
  position = "stack",
  stat = "identity") +
  facet_grid(cols=vars(orientacao1),
             rows = vars(paste(tn,Stage)),
             scales = "free_x")




normas_tema_acum_long %>% 
  left_join(govs) %>%
  mutate(orientacao=orientacao1) %>%
  dplyr::filter(Semana>6) %>% 
  ggplot(aes(x=Semana,color=orientacao,fill=orientacao,y=value)) +
  
  
  geom_point(size=.35,alpha=.5) +
  geom_smooth(aes(group=orientacao),span=.25,
              level = 0.95) +
  
  labs(y = "% of underreporting",color="",fill="",x="Epidemiological week of 2020") +
  facet_wrap(vars(name)) +
  theme_minimal() +
  theme(legend.position = "top")



df %>% mutate(orientacao=orientacao1) %>%
  dplyr::filter(Semana>6) %>% 
  ggplot(aes(x=Semana,color=orientacao,fill=orientacao,y=Normas_acum)) +
  
  annotate(geom="rect",fill=c("red","yellow","green","red"),
           xmin=janelas_analise$start-.5,
           xmax=janelas_analise$end+.5,
           ymin=0,
           ymax=Inf,alpha=.15) +
  annotate(geom="text",label=c("t1\n(w6 to w17)\nBeginning of first wave",
                               "t2\n(w6 to w17)\nPlateau",
                               "t3\n(w6 to w17)\nDecreasing",
                               "t4\n(w6 to w17)\nBeginning of second wave"),
           vjust=1,size=3,
           x=c(c(janelas_analise$start)+c(janelas_analise$end))/2,
           y=c(rep(25,4))) +
  
  geom_point(size=.35,alpha=.5) +
  geom_smooth(aes(group=orientacao),span=.25,
              level = 0.95) +
  
  labs(y = "% of underreporting",color="",fill="",x="Epidemiological week of 2020") +
  theme_minimal() +
  theme(legend.position = "top")


# Model ----
df %>%
  ggplot(aes(x=Semana,y=casos_100k,color=orientacao1)) +
    annotate(geom="rect",fill=c("red","yellow","green","red"),
             xmin=janelas_analise$start-.5,
             xmax=janelas_analise$end+.5,
             ymin=0,
             ymax=Inf,alpha=.15) +
    annotate(geom="text",label=c("t1\n(w6 to w17)\nBeginning of first wave",
                                 "t2\n(w6 to w17)\nPlateau",
                                 "t3\n(w6 to w17)\nDecreasing",
                                 "t4\n(w6 to w17)\nBeginning of second wave"),
             vjust=1,size=3,
             x=c(c(janelas_analise$start)+c(janelas_analise$end))/2,
             y=c(rep(700,4))) +
    
    geom_line(aes(group=UF),size=.2,alpha=.5) +
    geom_smooth(aes(group=orientacao1),span=.25,
                level = 0.95) +
  scale_x_continuous(breaks = c(seq(5,50,by=5))) +
  
  
  labs(fill="",y="Casos novos",x="",
       caption = "Source: InfoGripe,2020\n
                  (Exceeding SARS is the 2020 values 
                  excedding 2009-2019 median)") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.15, "lines"),
        panel.grid = element_line(size=.25,color = "grey75"),
        panel.grid.minor.x = element_blank(),
        plot.margin = ggplot2::margin(1, 1, 1, 1))  

df.discrete %>%
  ggplot(aes(x=lag(epidemiological_surveillance),y=total_cases_per_100k_inhabitants-lag(total_cases_per_100k_inhabitants),color=orientacao1)) +
  geom_text(aes(label=uf)) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(stage))

"health_services_working_processes" "human_resources"                   "epidemiological_surveillance"     
 "infrastructure"                    "intergovernmental_relations"       "spending"    

# Garbage ----
df %>% drop_na(perc_sub) %>%
  ggplot() +
  geom_bar(aes(x=paste(UF),y=perc_sub,fill=orientacao),
           stat = "identity") + 
  facet_wrap(vars(paste(epi_week,Stage),orientacao),scales = "free_x",nrow = 4)
