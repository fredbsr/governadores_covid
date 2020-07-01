# leitura de dados
#remotes::install_github("liibre/coronabr")
#devtools::install_github("tbrugz/ribge")
library(ribge)
library(coronabr)
library(googlesheets4)
library(tidyverse)
library(geobr)
library(rvest)
library(sf)
library(textclean)

# Regiões e estadosbrasil
estados <- read_state()
# Codificação das regiões
cods_ibge <- estados %>% 
  dplyr::select(codigo_uf="code_state",UF="abbrev_state",
                "code_region",Regiao="name_region") 
st_geometry(cods_ibge) <- NULL

# Estado
pop <- ribge::populacao_municipios(2019) 
pop %<>%
  group_by(codigo_uf) %>%
  summarise(populacao = sum(populacao,na.rm=T))

# SRAG ----
raw = "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/dados_semanais_faixa_etaria_sexo_virus_sem_filtro_sintomas.csv"

SRAG_raw <- read_csv2(raw) %>% 
  janitor::clean_names() 

# write_rds(SRAG_raw,"SRAG_raw.rds")

SRAG <- SRAG_raw %>% 
  dplyr::filter(tipo=="Estado",escala=="casos",sexo=="Total",#semana_epidemiologica<22,
                dado%in%c("srag","sragcovid")) %>%
  group_by(codigo_uf=uf,unidade_da_federacao,semana_epidemiologica,ano_epidemiologico,dado) %>%
  summarise(casos=sum(total_reportado_ate_a_ultima_atualizacao,na.rm = T)) %>%
  ungroup() %>%
  mutate(ano_2020 = (ano_epidemiologico>2019)) %>%
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
  left_join(pop) %>%
  mutate(covid_100k=covid*10^5/populacao,
         covid_sub_100k=covid_sub*10^5/populacao,
         covid_acum_100k=covid_acum*10^5/populacao,
         covid_sub_acum_100k=covid_sub_acum*10^5/populacao,
         srag_excesso_100k = srag_excesso*10^5/populacao,
         srag_excesso_acum_100k = srag_excesso_acum*10^5/populacao
         ) %>%
  left_join(cods_ibge)

rm(cods_ibge,pop,estados,SRAG_raw,raw)
# write_excel_csv2(SRAG,"dados_srag_2020_uf.csv")

# Por UF
SRAG %>%
  ggplot(aes(x=Semana)) +
  geom_area(aes(y=srag_excesso_100k,fill="SRAG excedente"),alpha=.5) +
  geom_area(aes(y=covid_100k,fill="COVID-19")) +
  facet_wrap(vars(UF)) +
  labs(fill="",y="Casos por 100k hab") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))

# Brasil
SRAG %>%
  group_by(Semana) %>%
  summarise(covid=sum(covid,na.rm=T),
            srag_excesso = sum(srag_excesso,na.rm=T),
            populacao=sum(populacao,na.rm=T)) %>%
  ungroup() %>%
  mutate(covid_100k=covid*10^5/populacao,
         srag_excesso_100k = srag_excesso*10^5/populacao
  ) %>%
  dplyr::filter(Semana<24) %>%
  ggplot(aes(x=Semana)) +
  geom_area(aes(y=srag_excesso,fill="SRAG excedente"),alpha=.5) +
  geom_area(aes(y=covid,fill="COVID-19")) +
  scale_x_continuous(breaks = c(3,7.5,11.5,15.5,19.5,23.5),
                     labels = c("Jan","Fev","Mar","Abr","Mai","Jun")) +
  labs(fill="",y="Casos novos",x="") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))  
  

# Governadores ----
gov_url <- "https://pt.wikipedia.org/wiki/Lista_de_governadores_das_unidades_federativas_do_Brasil_(2019%E2%80%932023)"
govs <- gov_url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  dplyr::select(-bandeira,-unidade_federativa) %>%
  rename(UF=abreviacao) %>%
  # ideologia e orientacao 
  mutate(
    partido = mgsub(partido,c("PSB\nCidadania [2]","PHS\nDEM","Sem Partido"),c("Cidadania","DEM","PSL")),
    ideologia = case_when(
      partido %in% c("DEM","MDB","NOVO",
                     "DEM","PP","PSC",
                     "PSD","PSDB","PSL") ~ "Direita",
      partido %in% c("PCdoB","PDT","PSB",
                     "Cidadania",
                     "PT") ~ "Esquerda"
    ),
    # ideologia_cont = 
    #   case_when(
    #     partido == ,
    #     
    #   )
    partido_join = str_to_lower(partido),
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
  dplyr::select(UF,partido,partido_join,
                ideologia,federalismo,federalismo2,orientacao) #%>%
  # left_join(
  #   mds %>% group_by(legislator_party) %>%
  # summarise(governismo = mean(dim1)) %>% 
  # mutate(partido_join = str_to_lower(legislator_party))
  # )



# Carta 17.fev
# "GO","MT","PR","RO","RR","SC","TO"
# Leia mais em: https://www.gazetadopovo.com.br/parana/breves/ratinho-nao-assina-carta-governadores-criticas-bolsonaro/

# Carta 19.abr outros não assinaram
# "AC","AM","DF","MG","PR","RO","RR"
# https://www.em.com.br/app/noticia/politica/2020/04/19/interna_politica,1140171/zema-nao-assina-carta-de-governadores-contra-bolsonaro.shtml



# Dados de testagem
testes_url_18_06 <-'http://web.archive.org/web/20200618071354/https://g1.globo.com/bemestar/coronavirus/noticia/2020/06/10/veja-taxa-de-ocupacao-nas-utis-testes-feitos-e-pacientes-recuperados-da-covid-19-em-cada-estado-do-brasil.ghtml'

testes_url <- "https://g1.globo.com/bemestar/coronavirus/noticia/2020/06/10/veja-taxa-de-ocupacao-nas-utis-testes-feitos-e-pacientes-recuperados-da-covid-19-em-cada-estado-do-brasil.ghtml"

testes <- testes_url_18_06 %>%
  read_html() %>%
  html_nodes('table') #%>% html_node(xpath = path_url) 

testagem <- testes[[4]] %>%
  html_table(header=T) %>%
  janitor::clean_names() %>%
  rename(testes = nº_de_testes,
         Estado = estado) %>%
  mutate(testes=as.numeric(gsub("\\.","",testes))) %>% 
  dplyr::filter(Estado != "Total") 


testagem %>% left_join(SRAG) %>%
  mutate(tx_teste = testes*10^5/populacao) %>%
  ggplot(aes(fill=Regiao,y=reorder(UF,desc(-tx_teste)),x=tx_teste)) +
  geom_bar(stat = "identity") +
  labs(x="Testes por 100k habitantes",y="UF",fill="") +
  theme_minimal() +
 # facet_grid(.~orientacao,space="free",scales = "free") +
  theme(legend.position = "bottom")

# Normas relacionadas à Saúde ----
normas_plan <- "https://docs.google.com/spreadsheets/d/1uZuSbqxywEwJiF4uBnc4yofIE9EH8oFG2maqhVfOrWk/edit#gid=1846588799"
# normas <- googlesheets4::read_sheet(normas_plan,sheet = 3,range = "B2:AC985",
#                                 col_types = "cDcccccccccccccccccccccccccc") 

normas_data <- googlesheets4::read_sheet(normas_plan,sheet = 3,range = "B2:C985",
                                    col_types = "cD") %>%
  rename(UF=Estado)

normas <- 
  expand_grid(UF=levels(factor(normas_data$UF)),
              Semana = 1:26) %>% full_join(
  normas_data %>%
  mutate(Semana = lubridate::epiweek(Data)) %>%
  group_by(Semana,UF) %>%
  summarise(Normas=n()) 
              ) %>%
  replace_na(list(Normas = 0)) %>%
  group_by(UF) %>%
  mutate(Normas_acum=cumsum(Normas))

# Normas por estado gráfico
normas %>%
  dplyr::filter(Semana<22) %>%
  drop_na() %>%
  ggplot(aes(x=Semana,fill=UF)) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA) +
 # geom_
  facet_wrap(vars(UF)) +
  theme_minimal() +
  theme(legend.position = "none")

## BASE COMPLETA ####

df <- SRAG %>%
  left_join(govs) %>%
  left_join(testagem) %>%
  mutate(tx_teste = testes*10^5/populacao) %>%
  left_join(normas)


# Normas e cvid por estado
df %>% dplyr::filter(Semana<23) %>%
  ggplot(aes(x=Semana,fill=orientacao,color=orientacao)) +
  geom_bar(aes(y=Normas_acum),stat = "identity",color=NA,alpha=.5) +
  geom_line(aes(y=covid_acum_100k/2,color="covid")) +
  geom_line(aes(y=covid_sub_acum_100k/2,color="subnot.")) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~.*2, name="Confirmados por 100k (linhas)")
  ) +
  facet_wrap(~UF) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Normas acumuladas por semana e por região
df %>% dplyr::filter(Semana<23) %>%
  group_by(Semana,Regiao) %>%
  summarise(Normas_acum = mean(Normas_acum,na.rm=T),
            Normas = mean(Normas,na.rm=T),
            pop = sum(populacao,na.rm=T),
            covid_acum_100k = mean(covid_acum_100k,na.rm=T)) %>%
  ggplot(aes(x=Semana,color=Regiao,fill=Regiao,group=Regiao)) +
  geom_bar(aes(y=Normas_acum),stat = "identity",color=NA,alpha=.5) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA,alpha=.5) +
  geom_line(aes(y=covid_acum_100k*2),size=1) +
  geom_hline(data=.%>% 
               group_by(Regiao) %>% 
               summarise(Normas = mean(Normas,na.rm=T)),
             aes(yintercept=Normas,color=Regiao,group=Regiao),
   size=2,alpha=.5
    ) +
  facet_wrap(vars(Regiao),nrow = 1) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~./2, name="Casos confirmados/100k hab (linhas)")
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

df %>%
  group_by(Semana,Estado,orientacao) %>%
  summarise(Normas = sum(Normas,na.rm=T),
            pop = sum(populacao,na.rm=T),
            Casos = sum(covid,na.rm=T),
            Sub = sum(covid_sub,na.rm=T)) %>%
  group_by(Semana,orientacao) %>%
  summarise(Normas = mean(Normas,na.rm=T),
            pop = sum(pop,na.rm=T),
            Casos = sum(Casos,na.rm=T),
            Sub = sum(Sub,na.rm=T)) %>%
  group_by(orientacao) %>%
  mutate(Normas_acumuladas = cumsum(Normas),
         Casos=cumsum(Casos),
         Sub=cumsum(Sub),
    Casos_100k = Casos*100000/pop,
    Sub_100k = Sub*100000/pop) %>% # data.frame() -> covid_normas_fig
  ggplot(aes(x=Semana,color=orientacao,fill=orientacao,group=orientacao)) +
  geom_bar(aes(y=Normas_acumuladas),stat = "identity",color=NA,alpha=.5) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA,alpha=.5,fill="grey50") +
  geom_line(aes(y=Casos_100k/2),size=1) +
  geom_line(aes(y=Sub_100k/2),linetype=2) +
  facet_wrap(vars(orientacao),nrow = 1) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~.*2, 
                         name="Casos confirmados/100k hab (linhas)")
  ) +
  labs(caption = "Linha contínua: Confirmados\nLinha tracejada: subnotificação estimada") +
  theme_minimal() +
  theme(legend.position = "none")


df %>%
  dplyr::filter(Semana==20) %>%
  left_join(testagem) %>%  
  mutate(tx_teste = testes*10^5/populacao) %>%
  ggplot(aes(
             y=log(covid_sub_acum_100k-covid_acum_100k),
             x=log(tx_teste),
             fill=orientacao,
             size=populacao)) +
  geom_vline(data=.%>%
               group_by(orientacao) %>%
               summarise(teste=mean(log(tx_teste))),
             aes(color=orientacao,group=orientacao,xintercept=teste),
             size=1,alpha=.5,linetype=2) +
  geom_point(alpha=.5,shape=21,color="black") +
  ggrepel::geom_label_repel(aes(color=orientacao,label=paste0(UF,"(",partido,")")),
                            fill=NA,size=3,label.size = 0,fontface="bold") +
  scale_size(range = c(1, 15), name="População (log)") +
  labs(x="Testagem (log da taxa)",y="Subnotificação (log subnotificados/100k hab)",
       caption = "Linhas tracejadas: médias na variável por orientação",
       title = "Covid19: subnotificação por testagem (ref. 10.jun)") +
  hrbrthemes::theme_ipsum() +
  theme(axis.title = element_text(size=14))


write_rds(df,"df.rds")

write_excel_csv2(df,"dados_srag_covid_uf.csv")

receitas_DF_jun2019 %<>% mutate(ano=2019)

write_excel_csv2(receitas_DF,"receitas_DF_atejun.csv")
