library(readr)
SRAG_raw <- 
  read_delim(SRAG_raw,";",escape_double = FALSE, 
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
             locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

View(dados_semanais_faixa_etaria_sexo_virus_sem_filtro_sintomas)


SRAG_raw_select %>% dplyr::select(tipo,escala,sexo,
                                  dado,uf,unidade_da_federacao,
                                  semana_epidemiologica,ano_epidemiologico,
                                  casos_semanais_reportados_ate_a_ultima_atualizacao) %>%
  dplyr::filter(tipo=="Estado",escala=="casos",sexo=="Total",#semana_epidemiologica<22,
                dado %in% c("srag","sragcovid"),
                ano_epidemiologico!=2021) -> check %>%
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
              values_from = casos) 


raw2 = "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/valores_esperados_por_localidade_sem_filtro_febre.csv"
SRAG_raw2 <- read_csv2(raw2) %>% as_tibble() %>%
  janitor::clean_names() 




raw3 = "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv"
SRAG_raw3 <- read_csv2(raw3) %>% as_tibble() %>%
  janitor::clean_names() 

names(SRAG_raw3)

SRAG_raw3 %>% 
  dplyr::select(tipo,escala,
                dado,uf,unidade_da_federacao,
                semana_epidemiologica,ano_epidemiologico,
                casos_semanais_reportados_ate_a_ultima_atualizacao,
                casos_estimados,populacao) %>%
  dplyr::filter(tipo=="Estado",escala=="casos",
                dado =="sragcovid",
                ano_epidemiologico==2020) %>%
  group_by(codigo_uf=uf,unidade_da_federacao,semana_epidemiologica,ano_epidemiologico) %>%
  summarise(casos=sum(casos_semanais_reportados_ate_a_ultima_atualizacao,na.rm = T)) -> check


raw4 = "https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/estados_e_pais_serie_estimativas_tendencia_sem_filtro_febre.csv"
SRAG_raw4 <- read_csv2(raw4) %>% as_tibble() %>%
  janitor::clean_names() 

SRAG_raw4 %>%
  dplyr::filter(semana_epidemiologica==31,ano_epidemiologico==2020,escala=="casos") %>%
  dplyr::select(uf=ds_uf_sigla,casos=casos_semanais_reportados_ate_a_ultima_atualizacao) %>%
  pull(casos) %>% sum()


names(SRAG_raw3)
summary(SRAG_raw3)
SRAG3 <- 
  bind_rows(
    SRAG_raw_select %>% dplyr::select(tipo,escala,
                                      dado,uf,unidade_da_federacao,
                                      semana_epidemiologica,ano_epidemiologico,
                                      casos_semanais_reportados_ate_a_ultima_atualizacao) %>%
      dplyr::filter(tipo=="Estado",escala=="casos",
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
      left_join(cods_ibge) %>%
      mutate(ano=2020),
    
    SRAG_raw_select %>% dplyr::select(tipo,escala,sexo,
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
      left_join(pop) %>%
      mutate(covid_100k=covid*10^5/populacao,
             covid_sub_100k=covid_sub*10^5/populacao,
             covid_acum_100k=covid_acum*10^5/populacao,
             covid_sub_acum_100k=covid_sub_acum*10^5/populacao,
             srag_excesso_100k = srag_excesso*10^5/populacao,
             srag_excesso_acum_100k = srag_excesso_acum*10^5/populacao
      ) %>%
      left_join(cods_ibge) %>%
      mutate(ano=2021) %>% dplyr::filter(Semana<10) 
    # filtrando para semanas existentes em 2021
  ) %>% 
  mutate(
    ano_semana=tsibble::yearweek(paste(ano,Semana,sep = " W"))
  )