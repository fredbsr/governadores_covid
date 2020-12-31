# leitura de dados
#remotes::install_github("liibre/coronabr")
#devtools::install_github("tbrugz/ribge")
library(ribge)
library(coronabr)
library(googlesheets4)
library(tidyverse)
library(magrittr)
library(geobr)
library(rvest)
library(sf)
library(textclean)
library(data.table)

# Regiões e estadosbrasil ----
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
#raw = "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv"
SRAG_raw <- read_csv2(raw) %>% 
  janitor::clean_names() 

# write_rds(SRAG_raw,"SRAG_raw.rds")
# SRAG_raw <- read_rds("SRAG_raw.rds")

SRAG <- SRAG_raw %>% 
  dplyr::filter(tipo=="Estado",escala=="casos",sexo=="Total",#semana_epidemiologica<22,
                dado%in%c("srag","sragcovid")) %>%
  group_by(codigo_uf=uf,unidade_da_federacao,semana_epidemiologica,ano_epidemiologico,dado) %>%
  summarise(casos=sum(casos_semanais_reportados_ate_a_ultima_atualizacao,
                      #total_reportado_ate_a_ultima_atualizacao,## mudou o nome da variavel
                      na.rm = T)) %>%
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

#rm(cods_ibge,pop,estados,SRAG_raw,raw)
# write_excel_csv2(SRAG,"dados_srag_2020_uf.csv")

# Verificando semana com dado estável
verf <- SRAG_raw %>% 
  dplyr::filter(ano_epidemiologico==2020,uf %in% c(11,53)) %>% 
  dplyr::select(uf,semana_epidemiologica,situacao_do_dado) %>% 
  distinct()

# Por UF (DF)
SRAG %>%
#  dplyr::filter(Semana<29) %>% # Semana
#  dplyr::filter(UF=="DF") %>% # DF
  ggplot(aes(x=Semana)) +
#  geom_area(aes(y=srag_excesso_100k,fill="SRAG excedente"),alpha=.5) + #Portugues
  geom_area(aes(y=srag_excesso_100k,fill="Exceeding SARS"),alpha=.5) + #Ingles
  geom_area(aes(y=covid_100k,fill="COVID-19")) +
  # scale_x_continuous(breaks = c(4,8,12,16,20,24,28),
  #                    labels = c("25.jan","22.fev","21.mar","18.abr",
  #                               "16.mai","13.jun","11.jul"),
  #                    limits = c(10,25)) +
  # scale_x_continuous("Dia (dado semanal)",
  #                    breaks = c(10,15,20,25),
  #                    labels = c("07.mar","11.abr",
  #                               "16.mai","20.jun"),
  #                    limits = c(10,28)) +
  facet_wrap(vars(UF)) +
  #labs(fill="",y="Casos por 100k hab") + #Por 
  labs(fill="",y="Cases/100k",x="Week") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.1, "lines"),
        plot.margin = ggplot2::margin(1, 1, 1, 1))


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
#  dplyr::filter(Semana<24) %>%
  ggplot(aes(x=Semana)) +
  geom_area(aes(y=srag_excesso,fill="SRAG excedente"),alpha=.5) +
  geom_area(aes(y=covid,fill="COVID-19")) +
  scale_x_continuous(limits = c(7,47),
                     breaks = c(3,7.5,11.5,15.5,19.5,23.5,27.5,31.5,35.5,39.5,43.5,47.5),
                     labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")) +
  labs(fill="",y="Casos novos",x="") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.15, "lines"),
        plot.margin = ggplot2::margin(1, 1, 1, 1))  
  

# COVID Brasil.io ----
br.io <- fread("https://data.brasil.io/dataset/covid19/caso_full.csv.gz") %>% 
  dplyr::filter(place_type=="state")
br.io %<>% group_by(UF=state,Semana=epidemiological_week) %>%
  summarise(casos=sum(new_confirmed,na.rm=T),
            obitos=sum(new_deaths,na.rm=T)) %>% 
  ungroup() %>%
  arrange(Semana) %>%
  group_by(UF) %>%
  mutate(casos_acum=cumsum(casos),
         obitos_acum=cumsum(obitos)) %>%
  ungroup() %>%
  left_join(cods_ibge) %>%
  left_join(pop) %>%
  mutate(casos_100k=casos*10^5/populacao,
         casos_acum_100k=casos_acum*10^5/populacao,
         obitos_milhao = obitos*10^6/populacao,
         obitos_acum_milhao = obitos_acum*10^6/populacao
  ) %>%
  arrange(codigo_uf,Semana)


rc.obito <- fread("https://data.brasil.io/dataset/covid19/obito_cartorio.csv.gz")

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
    partido = gsub("PSL\nPSL","PSL",partido),
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



# Dados de testagem ----
testes_url_18_06 <-'http://web.archive.org/web/20200618071354/https://g1.globo.com/bemestar/coronavirus/noticia/2020/06/10/veja-taxa-de-ocupacao-nas-utis-testes-feitos-e-pacientes-recuperados-da-covid-19-em-cada-estado-do-brasil.ghtml'

testes_url <- "https://g1.globo.com/bemestar/coronavirus/noticia/2020/06/10/veja-taxa-de-ocupacao-nas-utis-testes-feitos-e-pacientes-recuperados-da-covid-19-em-cada-estado-do-brasil.ghtml"

testes <- 
#  testes_url_18_06 %>%
  testes_url %>%
  read_html() %>%
  html_nodes('table') #%>% html_node(xpath = path_url) 

testagem <- testes[[1]] %>%
  html_table(header=T) %>%
  janitor::clean_names() %>%
  rename(testes = nº_de_testes,
         Estado = estado) %>%
  mutate(testes=gsub("\\(.*\\)","", testes),
         testes=gsub("[^0-9,-]", "", testes),
         testes=gsub(" ", "", testes),
         testes=as.numeric(testes)) %>% 
#  mutate(testes=as.numeric(gsub("\\.","",testes))) %>% 
  dplyr::filter(Estado != "Total") 


testagem %>% left_join(SRAG) %>%
  mutate(tx_teste = testes*10^4/populacao) %>%
  ggplot(aes(fill=Regiao,y=reorder(UF,desc(-tx_teste)),x=tx_teste)) +
  geom_bar(stat = "identity") +
  labs(x="Testes por 10mil habitantes",y="UF",fill="") +
  theme_minimal() +
 # facet_grid(.~orientacao,space="free",scales = "free") +
  theme(legend.position = "bottom")

# Outro dado de testagem
testagem2 <- read_csv("testes.csv") %>%
  dplyr::select(UF,testes=Testes)

testagem2 %>% left_join(SRAG) %>%
  mutate(tx_teste = testes*10^5/populacao) %>%
  ggplot(aes(fill=Regiao,y=reorder(UF,desc(-tx_teste)),x=tx_teste)) +
  geom_bar(stat = "identity") +
  labs(x="Testes por 100mil habitantes",y="UF",fill="") +
  theme_minimal() +
  # facet_grid(.~orientacao,space="free",scales = "free") +
  theme(legend.position = "bottom")




# Normas relacionadas à Saúde ----
normas_plan <- "https://docs.google.com/spreadsheets/d/1uZuSbqxywEwJiF4uBnc4yofIE9EH8oFG2maqhVfOrWk/edit#gid=1846588799"
# normas <- googlesheets4::read_sheet(normas_plan,sheet = 3,range = "B2:AC985",
#                                 col_types = "cDcccccccccccccccccccccccccc") 

normas_data <- googlesheets4::read_sheet(normas_plan,sheet = 3,range = "A1:AD2835",
                  col_types = "cDDccccccccccccccccccccccccccc") %>%
  janitor::clean_names() %>%
  mutate_at(vars(9:30),as.numeric) %>%
  mutate_if(is.numeric,replace_na,replace = 0) %>%
  rename(UF=estado)

normas <- 
  expand_grid(UF=levels(factor(normas_data$UF)),
              Semana = 1:49) %>% full_join(
  normas_data %>%
  mutate(Semana = lubridate::epiweek(data)) %>%
  group_by(Semana,UF) %>%
  summarise(Normas=n()) 
              ) %>%
  replace_na(list(Normas = 0)) %>%
  group_by(UF) %>%
  mutate(Normas_acum=cumsum(Normas))


# Normas por estado gráfico
normas %>%
  dplyr::filter(Semana>6) %>%
  drop_na() %>%
  ggplot(aes(x=Semana,fill=UF,color=UF)) +
  geom_bar(aes(y=Normas),stat = "identity",color=NA) +
  geom_line(aes(y=Normas_acum)) +
  # geom_
  facet_wrap(vars(UF)) +
  theme_minimal() +
  theme(legend.position = "none")

normas_tema <-  normas_data %>%
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
  mutate(Semana = lubridate::epiweek(data))  %>% drop_na(data)


normas_tema_semana <- normas_tema %>%
  group_by(UF,Semana=as.factor(Semana)) %>%
  summarise_if(is.numeric,sum,na.rm=T) %>%
  ungroup() %>%
  arrange(UF,Semana) 

normas_tema_semana <-
  expand_grid(UF=levels(factor(normas_data$UF)),
            Semana = 1:30) %>% left_join(
              normas_tema_semana %>%
                mutate(Semana=as.numeric(as.character(Semana))) 
            ) %>%
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


normas_tema_acum_long <- normas_tema_acum %>%
  dplyr::select(UF,Semana,
                processos_de_servicos_de_saude,
                recursos_humanos,acoes_de_vigilancia_epidemiologica,
                infraestrutura,relacoes_intergovernamentais,
                gastos) %>%
  pivot_longer(-c(UF,Semana)) %>%
  replace_na(list(value = 0)) 

# Normas acumuladas por tema
normas_tema_acum_long %>%
  mutate(name=mgsub(name,
                    c('processos_de_servicos_de_saude',
                      'recursos_humanos',
                      'acoes_de_vigilancia_epidemiologica',
                      'infraestrutura',
                      'relacoes_intergovernamentais',
                      'gastos'),
                    c("Processos de serviços de saúde",
                      "Recursos Humanos",
                      "Ações de vigilância epidemiológica",
                      "Infraestrutura",
                      "Relações intergovernamentais",
                      "Gastos"))) %>%
  dplyr::filter(Semana>6) %>%
  ggplot(aes(x=Semana,fill=name,color=name,y=value)) +
  geom_area(position = "stack",color=NA,stat = "identity",alpha=.75) +
  facet_wrap(vars(UF)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Normas acumuladas por tema com obitos do brasil.io
normas_tema_acum_long %>%
  mutate(name=mgsub(name,
                    c('processos_de_servicos_de_saude',
                      'recursos_humanos',
                      'acoes_de_vigilancia_epidemiologica',
                      'infraestrutura',
                      'relacoes_intergovernamentais',
                      'gastos'),
                    c("Health Processes",
                      "Human Resources",
                      "Epidemiological Surveillance",
                      "Infrastructure",
                      "Intergovernmental relations",
                      "Spending"))) %>%
 # dplyr::filter(Semana>6) %>%
  #left_join(br.io) %>% #depois colocar empilhado
  
  ggplot(aes(x=Semana,fill=name,color=name,y=value)) +
  geom_area(position = "stack",color=NA,stat = "identity",alpha=.75) +
  geom_line(aes(y=obitos_acum_milhao/6),
            color="black") +
  facet_wrap(vars(UF),ncol=7) +
  # scale_x_continuous(name="Mês",
  #                    limits = c(7,30),
  #                    breaks = c(11.5,19.5,27.5),
  #                    labels = c("Mar","Mai","Jul")) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~.*12, 
                         name="Óbitos por milhão (linhas)")
  ) +
  labs(
       fill="") +
  theme_minimal() +
  theme(legend.position = "bottom")



# covid e normas ao longo do tempo
normas_tema_acum_long %>%
  group_by(UF, Semana) %>%
  summarise(normas=sum(value,na.rm=T))
  left_join(SRAG %>% dplyr::select()) %>% 
  ggplot(aes(x=Semana,fill=name,color=name,y=value)) +
  geom_area(position = "stack",color=NA,stat = "identity",alpha=.75) +
  geom_line(aes(y=obitos_acum_milhao/6),
            color="black") +
  facet_wrap(vars(UF),ncol=7) +
  # scale_x_continuous(name="Mês",
  #                    limits = c(7,30),
  #                    breaks = c(11.5,19.5,27.5),
  #                    labels = c("Mar","Mai","Jul")) +
  scale_y_continuous(
    name = "Normas (barras)",
    sec.axis = sec_axis( trans=~.*12, 
                         name="Óbitos por milhão (linhas)")
  ) +
  labs(
    fill="") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Cestas de normas
cestas <- normas_tema_acum_long %>%
  mutate(name=mgsub(name,
                    c('processos_de_servicos_de_saude',
                      'recursos_humanos',
                      'acoes_de_vigilancia_epidemiologica',
                      'infraestrutura',
                      'relacoes_intergovernamentais',
                      'gastos'),
                    c("Processos",
                      "RH",
                      "Vigilância",
                      "Infraestrutura",
                      "Articulação",
                      "Gastos"))) %>%
  dplyr::filter(Semana==30) %>%
  dplyr::select(-Semana) %>%
  group_by(UF) %>% 
  mutate(pct=prop.table(value))


cestas %<>% left_join(cestas %>%
                        group_by(UF) %>%
                        summarise(normas=sum(value,na.rm=T))) %>%
  left_join(cestas %>% group_by(UF) %>%
              dplyr::filter(value==max(value)) %>%
              dplyr::select(UF,tipo=name)) %>%
  left_join(br.io %>% 
              dplyr::filter(Semana==30) %>% 
              dplyr::select(UF,obitos_acum_milhao))

cestas %>%
  ggplot() +
  geom_point(aes(color=name,x=name,
               y=pct
                        ),
           #    position = "stack",
           stat = "identity") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(vars(paste0(UF," (",round(obitos_acum_milhao,0)," +/mi)")),
             #cols = vars(obitos_acum_milhao>mean(cestas$obitos_acum_milhao)),
           #  space = "free",
            # switch = "y",
           #  scales = "free"
            ) +
  labs(fill="",y="",x="") + 
  theme_minimal() +
  coord_polar() +
  theme(strip.text.y = element_text(angle = 0),
                        legend.position = "top")
    


require(ggiraph)
library(ggiraphExtra)

ggRadar(data=cestas,aes(
  facet=UF),interactive=F)




# Subtemas
normas_subtemas_long <- normas_tema_acum %>%
  dplyr::select(-c(
                processos_de_servicos_de_saude,
                recursos_humanos,acoes_de_vigilancia_epidemiologica,
                infraestrutura,relacoes_intergovernamentais,
                gastos)) %>%
  pivot_longer(-c(UF,Semana)) %>%
  replace_na(list(value = 0)) %>%
  mutate(tema=factor(case_when(
    name %in% c("diretrizes_de_tratamento",                      
                "reorientacao_da_provisao_de_servicos",          
                "outros_processos")~"Processos de serviços de saúde" ,
    
    name %in% c("aumentar_o_numero_de_profissionais_de_saude" ,  
                "medidas_de_protecao_dos_profissionais_de_saude",
                "treinamento_dos_profissionais_de_saude",        
                "outros_rh")~"Recursos Humanos",
    
    name %in% c("identificacao_de_casos",                        
                "isolamento_de_casos_positivos",                 
                "outros_vigilancia")~"Ações de vigilância epidemiológica", 
                
    name %in% c("aumento_da_capacidade_da_urgencia_e_emergencia",
                "aumento_da_capacidade_de_internacao",           
                "aumento_da_capacidade_da_atencao_basica",       
                "outros_infraestrutura")~"Infraestrutura",
                
    name %in% c("aquisicao_de_insumo",     
                "realocacao_de_recurso",                         
                "aumento_de_recurso",                            
                "contratacao_de_pessoal",                        
                "investimento_em_infraestrutura",                
                "outros_gastos")~"Gastos",
    name %in% c("estabelece_questoes_envolvendo_outros_entes")~"Relações intergovernamentais"
    ))
  )  %>%
  left_join(br.io)

# Fazendo varios graficos ao mesmo tempo
g <- purrr::map(levels(factor(normas_subtemas_long$tema)),
                function(x) {
                  ggplot(data = filter(normas_subtemas_long,tema == x),
                         aes(x=Semana,fill=name,color=name,y=value)) +
                              geom_area(position = "stack",color=NA,stat = "identity",alpha=.75) +
                              geom_line(aes(y=obitos_acum_milhao/12),
                                        color="black") +
                              facet_wrap(vars(UF),ncol=7) +
                    scale_x_continuous(name="Mês",
                                       limits = c(7,30),
                                       breaks = c(11.5,19.5,27.5),
                                       labels = c("Mar","Mai","Jul")) +
                    scale_y_continuous(
                      name = "Normas (barras)",
                      sec.axis = sec_axis( trans=~.*12, 
                                           name="Óbitos por milhão (linhas)")
                    ) +
                    labs(title = x,
                         fill="") +
                              theme_minimal() +
                              theme(legend.position = "bottom")
                })


ggsave(file='g1.png',plot=g[[1]],width = 10,height = 6)
ggsave(file='g2.png',plot=g[[2]],width = 10,height = 6)
ggsave(file='g3.png',plot=g[[3]],width = 10,height = 6)
ggsave(file='g4.png',plot=g[[4]],width = 10,height = 6)
ggsave(file='g5.png',plot=g[[5]],width = 10,height = 6)
ggsave(file='g6.png',plot=g[[6]],width = 10,height = 6)


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

## Dados infectados DF ----
library(readr)
dados_abertos <- read_delim("dados-abertos.csv", 
                            ";", escape_double = FALSE, 
                            col_types = cols(Data = col_date(format = "%d/%m/%Y"), 
                                            `Data Cadastro` = col_date(format = "%d/%m/%Y"), 
                                            dataPrimeirosintomas = col_date(format = "%d/%m/%Y")), 
                            trim_ws = TRUE)

# Perfil
url_dados <- "https://covid19.ssp.df.gov.br/resources/dados/dados-abertos.csv"

dados_abertos <- read_delim(url_dados, ";",escape_double = FALSE, 
                            col_types = cols(Data = col_date(format = "%d/%m/%Y"), 
                                             `Data Cadastro` = col_date(format = "%d/%m/%Y"), 
                                              dataPrimeirosintomas = col_date(format = "%d/%m/%Y")), 
                            trim_ws = TRUE)




dados_abertos_perfil <- dados_abertos %>% janitor::clean_names() 

serie_tempo <- dados_abertos_perfil %>%
  mutate(uf=53,
         #Semana=lubridate::epiweek(data_primeirosintomas)
         Semana=lubridate::epiweek(data_cadastro)
         ) %>%
  group_by(uf,Semana,obito) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = obito,values_from=n,values_fill=0)  %>% 
  janitor::clean_names() %>% dplyr::rename(Semana=semana,obitos=sim,vivos=nao) %>%
  left_join(pop,by=c("uf"="codigo_uf")) %>%
  mutate(covid=obitos+vivos,
         covid_100k=covid*10^5/populacao,
         obitos_milhao=obitos*10^6/populacao,
         obitos_100k=obitos*10^5/populacao) 

serie_tempo %>%
  dplyr::filter(Semana<27) %>%
  ggplot(aes(x=Semana)) +
  geom_area(aes(y=covid_100k,fill="COVID-19")) +
#  geom_area(aes(y=obitos_milhao,fill="Óbitos"),alpha=.5) +
  scale_x_continuous("Dia (dado semanal)",
                     breaks = c(10,15,20,25,29),
                     labels = c("07.mar","11.abr",
                                "16.mai","20.jun","18.jul"),
                     limits = c(10,25)) +
  facet_wrap(vars(uf)) +
#  labs(fill="",y="Casos por 100k hab") +
  labs(fill="",y="Óbitos por milhão de habitante") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))


serie_dia <- dados_abertos_perfil %>%
  mutate(uf=53,
         # data=lubridate::as_date(data_primeirosintomas)
         data=lubridate::as_date(data_cadastro)
  ) %>%
  dplyr::filter(data>lubridate::dmy("01-03-2020")) %>%
  group_by(data,obito) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = obito,values_from=n,values_fill=0)  %>% 
  janitor::clean_names() %>% dplyr::rename(obitos=sim,vivos=nao)  %>% 
  ungroup() %>%
#  left_join(pop,by=c("uf"="codigo_uf")) %>%
  mutate(covid=obitos+vivos,
         mm3_covid=forecast::ma(covid,3),
         mm5_covid=forecast::ma(covid,5),
         mm7_covid=forecast::ma(covid,7),
         mm14_covid=forecast::ma(covid,14))

serie_dia %>%
  dplyr::filter(data>lubridate::dmy("01-03-2020")) %>%

  ggplot(aes(x=data)) +
  geom_area(aes(y=covid,fill="Registros do dia"),alpha=.5) +
#  geom_line(aes(y=mm7_covid,color="Média móvel (7 dias)"),size=1.5) +
  geom_line(aes(y=mm14_covid,color="Média móvel (14 dias)"),size=1.5) +
  # geom_text(data=.%>%
  #             dplyr::filter(mm7_covid==max(mm7_covid,na.rm=T)),
  #      aes(x=data,y=mm7_covid,
  #          label=paste0("Pico(mm): ",lubridate::day(data) ,".",
  #          lubridate::month(data,label = T),"->",round(mm7_covid,0)," casos")),
  #          vjust=-.50,hjust=1,fontface="bold") +
  geom_text(data=.%>%
              dplyr::filter(mm14_covid==max(mm14_covid,na.rm=T)),
            aes(x=data,y=mm14_covid,
                label=paste0("Pico(mm): ",lubridate::day(data) ,".",
                             lubridate::month(data,label = T),"->",round(mm14_covid,0)," casos")),
            vjust=-.50,hjust=1,fontface="bold") +
  labs(fill="",y="Casos novos (registrados)",color="",x="",
       caption="Fonte: SSP-DF, 2020.") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))


tabela_perfil <- dados_abertos_perfil %>%
  dplyr::select(sexo,faixa_etaria,ra,obito) %>%
  mutate(ra=forcats::as_factor(ra))

grupos <- compareGroups::compareGroups(~ . , 
                                       dados_abertos_perfil %>% dplyr::filter(obito=="Sim")
                                       )
grupotab <- compareGroups::createTable(grupos, hide.no = "no",show.n = FALSE)


grupotab

dados_abertos_perfil %>%
 # dplyr::filter(obito=="Sim") %>% 
  janitor::tabyl(ra) %>% janitor::adorn_pct_formatting()

dados_abertos_perfil %>% 
  dplyr::filter(obito=="Sim") %>% 
  janitor::tabyl(faixa_etaria) %>% 
  janitor::adorn_pct_formatting()

# Letalidade por RA
letalidade <- dados_abertos_perfil %>%
  group_by(ra,obito) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = obito,values_from=n,values_fill=0)  %>% 
  janitor::clean_names() %>% dplyr::rename(RA=ra,obitos=sim,vivos=nao) %>%
  mutate(covid=obitos+vivos,
         obitos_caso=obitos/covid) 

obitos_vs_pop <-
  tibble(var=c("Sexo","Idade","RA"),
       cat=c("Masculino","60 anos ou mais","Ceilândia (e Pôr do Sol)"),
       obitos=c(60,73,21),
       pop=c(48,11,15)) %>%
  pivot_longer(obitos:pop)


obitos_vs_pop %>%
  ggplot(aes(x=cat,y=value,fill=name,#color=name,
             label=value)) +
  geom_bar(color=NA,
           stat ="identity",
           position = "dodge",
           width = .75,
           alpha=.75) +
  geom_text(fontface="bold",family="Arial Narrow",vjust=-.5,
            size=4,
           position = position_dodge(width = .75)) +
  hrbrthemes::theme_ipsum() +
  scale_fill_manual(labels=c("% dos óbitos","% na população"),
                    values = c("indianred4","dodgerblue3")) +
  scale_color_manual(labels=c("% dos óbitos","% na população"),
                    values = c("indianred4","dodgerblue3")) +
  labs(y="%",x="",fill="",color="",caption="Fonte: SSP-DF, 2020 e PDAD, 2018.") +
  coord_cartesian(clip="off") +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))


  
