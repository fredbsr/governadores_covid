library(readr)
library(magrittr)
library(tidyverse)
library(readxl)
library(scales)
library(patchwork)
library(ggrepel)

<<<<<<< HEAD
IGP <- read_excel("igp.xlsx",sheet = 2)

transf_SIGA <- read_excel("siga_brasil.xlsx",sheet = 1)

=======
#Inflacao ----
IGP <- read_excel("igp.xlsx",sheet = 2)

# SIGA ----
transf_SIGA <- read_excel("siga_brasil.xlsx",sheet = 1)

# Dados mobilidade ----
>>>>>>> 4386ef3b44920ae8e0d81adefb323c5ddd3b60e5
mobilidade_dia <- read_delim("Global_Mobility_Report.csv",
                                ",", escape_double = FALSE, skip = 0,
                                locale = locale(decimal_mark = ".",
                                                grouping_mark = ","), trim_ws = TRUE) %>%
  janitor::clean_names() %>%
<<<<<<< HEAD
  dplyr::filter(iso_3166_2_code=="BR-DF") %>%
  mutate(Semana = lubridate::epiweek(date)) 

mobilidade <- mobilidade_dia %>%
  group_by(Semana) %>%
  summarise_if(is.numeric,mean,na.rm=T)


=======
#  dplyr::filter(iso_3166_2_code=="BR-DF") %>%
  dplyr::filter(country_region=="Brazil") %>%
  mutate(Semana = lubridate::epiweek(date)) 

mobilidade <- mobilidade_dia %>%
  mutate(UF=case_when(is.na(sub_region_1) ~ " Brasil",
                      sub_region_1=="Federal District"~"DF",
                      sub_region_1=="State of Acre"   ~"AC",
                      sub_region_1=="State of Alagoas"  ~"AL",
                      sub_region_1=="State of Amapá"  ~"AP", 
                      sub_region_1=="State of Amazonas"     ~"AM",  
                      sub_region_1=="State of Bahia"            ~"BA"  ,
                      sub_region_1=="State of Ceará"     ~"CE",
                      sub_region_1=="State of Espírito Santo"~"ES", 
                      sub_region_1=="State of Goiás"              ~"GO",
                      sub_region_1=="State of Maranhão"  ~"MA", 
                      sub_region_1=="State of Mato Grosso" ~"MT"  , 
                      sub_region_1=="State of Mato Grosso do Sul" ~"MS",
                      sub_region_1=="State of Minas Gerais"~"MG", 
                      sub_region_1=="State of Pará"     ~"PA"      , 
                      sub_region_1=="State of Paraíba"     ~"PB" ,
                      sub_region_1=="State of Paraná"       ~"PR", 
                      sub_region_1=="State of Pernambuco"   ~"PE"   , 
                      sub_region_1=="State of Piauí"             ~"PI" ,
                      sub_region_1=="State of Rio de Janeiro"~"RJ",      
                      sub_region_1=="State of Rio Grande do Norte"~"RN", 
                      sub_region_1=="State of Rio Grande do Sul" ~"RS" ,
                      sub_region_1=="State of Rondônia"       ~"RO",     
                      sub_region_1=="State of Roraima"            ~"RR" ,
                      sub_region_1=="State of Santa Catarina"     ~"SC",
                      sub_region_1=="State of São Paulo"       ~"SP",    
                      sub_region_1=="State of Sergipe"            ~"SE" , 
                      sub_region_1=="State of Tocantins"~"TO"
                      )
  ) %>%
  group_by(UF,Semana) %>%
  summarise_if(is.numeric,mean,na.rm=T)

# Dados de mobilidade do google
# mob_plot <- mobilidade_dia %>%
#   dplyr::select(-c(1:4,6,14)) %>%
#   pivot_longer(3:8) %>%
#   mutate(name=gsub("_percent_change_from_baseline","",name))

mob_plot <- mobilidade %>%
  pivot_longer(3:8) %>%
  mutate(name=gsub("_percent_change_from_baseline","",name)) %>%
  mutate(nome=mgsub(name,
                    c("grocery_and_pharmacy",
                      "parks",
                      "residential",
                      "retail_and_recreation",
                      "transit_stations",
                      "workplaces"),
                    c("Lanchonetes e farmácias",
                      "Parques",
                      "Residências",
                      "Varejo e recreação",
                      "Estações de transporte",
                      "Locais de trabalho")))


mob_plot %>%
  ggplot() +
  geom_line(aes(group=nome,color=nome,x=Semana,y=value),size=.75) +
  scale_x_continuous(breaks = c(3,#7.5,
                                11.5,#15.5,
                                19.5,#23.5,
                                27.5#,
                                ),
                     labels = c("Jan",#"Fev",
                                "Mar",#"Abr",
                                "Mai",#"Jun",
                                "Jul"#,"Ago"
                                )) +
  geom_vline(xintercept = 13) +
  geom_hline(yintercept = 0,size=.35,color="grey50") +
 # annotate(geom = "label",label=" Pronunciamento do Presidente: 24.mar",x=13,y=35,hjust=0,vjust=.5,size=1.5) +
  labs(y="Variação % em relação ao normal",color="Atividade",
     #  title="Isolamento Social e mobilidade",
       x="",
       caption = "Fonte: Google Mobility Report, 2020") +
  facet_wrap(vars(UF),ncol=7) + theme_minimal() + theme(legend.position = "top")
hrbrthemes::theme_ipsum() +
  theme(panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))

# Mobilidade Brasil
mob_plot %>%
  dplyr::filter(UF==" Brasil",
              nome  %in% c("Lanchonetes e farmácias",
                      # "Parques",
                       "Residências",
                       "Varejo e recreação",
                       "Estações de transporte"#,
                      # "Locais de trabalho"
                      )
                ) %>%
  dplyr::mutate(nome=mgsub(nome,c("Lanchonetes e farmácias",
                             "Residências",
                             "Varejo e recreação",
                             "Estações de transporte"
  ),c("drogaria","casa","varejo","transporte"))) %>%
  
  ggplot() +
  geom_line(aes(group=nome,color=nome,x=Semana,y=value),size=1.5) +
  scale_x_continuous(breaks = c(3,7.5,
                                11.5,15.5,
                                19.5,23.5,
                                27.5,31.5
  ),
  labels = c("Jan","Fev",
             "Mar","Abr",
             "Mai","Jun",
             "Jul","Ago"
  )) +
  geom_vline(xintercept = 13,color="grey50") +
  geom_hline(yintercept = 0,size=.35,color="grey75") +
#  annotate(geom = "label",label=" Pronunciamento do Presidente: 24.mar",x=13,y=35,hjust=0,vjust=.5,size=4) +
  annotate(geom = "text",
           #label=" Dia 24 de março",
           label=" vinte e quatro de  de marco",
           family = "Elanor Free Font",
           x=13,y=35,hjust=0,vjust=.5,size=4) +
  labs(#title="Nível de atividade em relação ao normal",
  #  title="Nivel de atividade em relacao ao normal",   
    color="Atividade",
     #  title="Isolamento Social e mobilidade",
     #y="%, normal = 0 ",
     y="normal igual a zero",
    #   x="Mês"
    x="",
       caption = "Fonte: Google Mobility Report, 2020") +
 # facet_wrap(vars(UF),ncol=7) + theme_minimal() + theme(legend.position = "top")
  #hrbrthemes::theme_ipsum() +
  theme_minimal() + 
  theme(text = element_text(family = "Elanor Free Font"),
        axis.title.y = element_text(size=11),
        axis.text.y = element_text(family = "Roboto Condensed"),
        panel.spacing=grid::unit(.25, "lines"),
        legend.position = "top",
        plot.margin = ggplot2::margin(2, 2, 2, 2))


write_rds(mob_plot,"mob_plot.rds")


# Relatorio receita SEFAZ DF ----
>>>>>>> 4386ef3b44920ae8e0d81adefb323c5ddd3b60e5
relatorio_receita <- read_delim("relatorio_df_jan2019_jun2020.csv",
                                  ";", escape_double = FALSE, skip = 1,
                                  locale = locale(decimal_mark = ",",
                                                  grouping_mark = "."), trim_ws = TRUE) %>%
  janitor::clean_names()

relatorio_receita %<>% dplyr::filter(mes %in% c(1:5))

# icms = "IMPOSTOS SOBRE A PRODUÇÃO, CIRCULAÇÃO DE MERCADORIAS E SERVIÇOS"
# 
# receita_ICMS <- relatorio_receita %>% 
#   dplyr::filter(alinea == icms) %>%
#   dplyr::select(subalinea,exercicio,mes,receita_realizada) %>%
#   pivot_wider(names_from = exercicio,values_from=receita_realizada)

icms_sub = "IMPOSTO SOBRE OPERAÇÕES RELATIVAS À CIRCULAÇÃO DE MERCADORIAS E SOBRE"
iss_sub = "IMPOSTO SOBRE SERVIÇOS DE QUALQUER NATUREZA - PRINCIPAL"

receita_impostos <- relatorio_receita %>% 
  dplyr::filter(subalinea %in% c(icms_sub,iss_sub)) %>%
  dplyr::select(subalinea,exercicio,mes,receita_realizada) %>%
  left_join(IGP) %>%
  mutate(receita = receita_realizada*inflator,
         exercicio=factor(exercicio),
      #   mes = factor(mes),
         subalinea = mgsub(subalinea,
                           c(icms_sub,iss_sub),
                           c("ICMS","ISS"))) %>%
  mutate(mes_ajuste_2019=ifelse(mes %in% c(1,2) & subalinea == "ISS" & exercicio ==2019,
                                "ajustar","ok")) %>%
  group_by(mes_ajuste_2019) %>%
  mutate(receita_ajuste = mean(receita,na.rm=T)) %>%
  ungroup() %>%
  mutate(receita= ifelse(mes_ajuste_2019=="ok",receita,receita_ajuste)) %>%
  dplyr::select(-receita_ajuste,-mes_ajuste_2019)

  
#write_excel_csv2(receitas_DF,"receitas_DF_atejun.csv")

receita_impostos %>%
  
  ggplot() +
  geom_bar(aes(fill=exercicio,group=exercicio,x=mes,y=receita),
           color=NA,position = "dodge",stat = "identity") +
  geom_text(aes(y=receita,x=mes,group=exercicio,label=paste0(round(receita/10^6,0),"mi")),
             color="white",fontface="bold",
             position = position_dodge(width=1),
             angle = 90, hjust = 'right'
            ) + hrbrthemes::theme_ipsum() + facet_wrap(vars(subalinea))



receita_impostos %>%
  
  group_by(exercicio,mes) %>%
  summarise(receita=sum(receita,na.rm=T)) %>%
  # left_join(SRAG %>%
  # dplyr::filter(codigo_uf==53) %>%
  # mutate(
  #   mes=case_when(
  #     Semana %in% c(1:5) ~ 1,
  #     Semana %in% c(6:9) ~ 2,
  #     Semana %in% c(10:13) ~ 3,
  #     Semana %in% c(14:17) ~ 4,
  #     Semana %in% c(18:22) ~ 5,
  #   )
  # ) %>%
  # group_by(mes) %>%
  # summarise(semana_mes=mean(Semana))) %>%
  ggplot() +
  geom_bar(aes(fill=exercicio,group=exercicio,x=mes,y=receita),
           color=NA,position = "dodge",stat = "identity",width = .7) +
  geom_text(aes(y=receita,x=mes,group=exercicio,label=paste0(round(receita/10^6,0)," mi ")),
            color="white",fontface="bold",
            position = position_dodge(width=.7),
            angle = 90, hjust = 'right'
  ) + 
  scale_x_continuous(breaks = c(1:5),
                         labels = c("Jan","Fev","Mar","Abr","Mai")) +
  hrbrthemes::theme_ipsum()


milh <- function (x) { number_format(accuracy = 1,
                                   scale = 1/10^6,
                                   suffix = "mi",
                                   big.mark = ",")(x) }

receita_impostos %>%
  
  group_by(exercicio,mes) %>%
  summarise(receita=sum(receita,na.rm=T)) %>%
  pivot_wider(names_from = exercicio,values_from=receita) %>%
  mutate(receita=`2020`-`2019`) %>%

  left_join(
    SRAG %>%
      dplyr::filter(codigo_uf==53) %>%
      left_join(mobilidade) %>%
      mutate(
        mes=case_when(
          Semana %in% c(1:5) ~ 1,
          Semana %in% c(6:9) ~ 2,
          Semana %in% c(10:13) ~ 3,
          Semana %in% c(14:17) ~ 4,
          Semana %in% c(18:22) ~ 5,
        )
      ) %>%
      group_by(mes) %>%
      summarise(isolamento =mean(retail_and_recreation_percent_change_from_baseline,na.rm=T),
                covid=sum(covid,na.rm=T),
                srag_excesso=sum(srag_excesso,na.rm=T),
                populacao = mean(populacao,na.rm=T)) %>%
      mutate(isolamento=-isolamento*10^6,
             covid_100k=covid*10^5/populacao,
             srag_excesso_100k = srag_excesso*10^5/populacao)
  ) %>%
  
  
  ggplot(aes(x=mes)) +
  # geom_area(aes(y=srag_excesso_100k*10^6,fill="SRAG excedente"),alpha=.5) +
  # geom_area(aes(y=covid_100k*10^6,fill="COVID-19")) +
  # geom_point(aes(y=isolamento)) +
  geom_bar(aes(y=receita,fill=(receita>0)),
           color=NA,stat = "identity",width = .7,alpha=.75) +
  geom_text(aes(y=max(receita),color=(receita>0),
                label=paste0(round(receita/10^6,0)," mi")),
            fontface="bold",family="Arial Narrow",
             vjust = -.50
  ) + 

  scale_x_continuous(breaks = c(1:5),
                     labels = c("Jan","Fev","Mar","Abr","Mai")) +
  scale_y_continuous(labels = milh) +
  labs(y="Receita2020 - Receita2019\n",
       x="",
       title="Evolução das receitas (ICMS+ISS)\n",
       caption = "Fonte: Portal de Transparência do DF, 2020\nObs.:valores em R$ de jun/2020") +
  hrbrthemes::theme_ipsum() +
  coord_cartesian(clip = "off",
                  #ylim = c(0,110),
                  expand = F) +
  theme(legend.position = "none",
        panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2)) -> fig.receitas


# Tentando plotar

aberturas <- data.frame(atividade=c("Bancos",
                                    "Eletrodomésticos e móveis",
                                    "Lojas de Roupas e calçados",
                                    "Autônomos",
                                    "Comércio varejista e shoppings",
                                    "Parques públicos"),
                        atividade_short=c("Bancos",
                                    "Eletrodom.",
                                    "Roupas",
                                    "Autônomos",
                                    "Varejo",
                                    "Parques"),
                        dia=c("2020-04-07",
                              "2020-04-09",
                              "2020-04-16",
                              "2020-04-22",
                              "2020-05-24",
                              "2020-05-30"),
                        dia_texto=c("7.abr",
                                    "09.abr",
                                    "16.abr",
                                    "22.abr",
                                    "24.mai",
                                    "30.mai"),
                        Semana=c(23,15,16,17,21,22))
                      #  Semana=c(15,15,16,17,22,22)) real, com semana 22


SRAG %>%
 dplyr::filter(codigo_uf==53) %>%
  left_join(aberturas) %>%
  left_join(mobilidade) %>%
  ggplot(aes(x=Semana)) +
  geom_area(aes(y=srag_excesso_100k,fill="SRAG excedente"),alpha=.5) +
  geom_area(aes(y=covid_100k,fill="COVID-19")) +
 # geom_line(aes(y=-parks_percent_change_from_baseline/5),color="black") +
  geom_text_repel(data=.%>%drop_na(),aes(y=srag_excesso_100k,label=str_wrap(paste0(dia_texto,"\n",atividade_short),10)),
             color="black",hjust="left",size=4,fontface="bold",family="Arial Narrow") +
  scale_x_continuous(breaks = c(3,7.5,11.5,15.5,19.5),
                     labels = c("Jan","Fev","Mar","Abr","Mai")) +
  labs(fill="",y="Casos novos por 100k hab",x="",title = "Evolução da COVID-19 no DF\n",
       caption = "Fonte: OpenDataSUS, 2020") +
  hrbrthemes::theme_ipsum() +
  coord_cartesian(clip = "off",
                  #ylim = c(0,110),
                  expand = F) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2)) -> fig.covid



fig.covid/fig.receitas +plot_layout(heights = c(5,4))

<<<<<<< HEAD
mob_plot <- mobilidade_dia %>%
  dplyr::select(-c(1:4,6,14)) %>%
  pivot_longer(3:8) %>%
  mutate(name=gsub("_percent_change_from_baseline","",name))

mob_plot <- mobilidade %>%
  pivot_longer(2:7) %>%
  mutate(name=gsub("_percent_change_from_baseline","",name)) %>%
  mutate(nome=mgsub(name,
                    c("grocery_and_pharmacy",
                      "parks",
                      "residential",
                      "retail_and_recreation",
                      "transit_stations",
                      "workplaces"),
                    c("Lanchonetes e farmácias",
                      "Parques",
                      "Residências",
                      "Varejo e recreação",
                      "Estações de transporte",
                      "Locais de trabalho")))


mob_plot %>%
  ggplot() +
  geom_line(aes(group=nome,color=nome,x=Semana,y=value),size=2) +
  scale_x_continuous(breaks = c(3,7.5,11.5,15.5,19.5,23.5),
                     labels = c("Jan","Fev","Mar","Abr","Mai","Jun")) +
  geom_vline(xintercept = 13) +
  annotate(geom = "label",label=" Pronunciamento do Presidente: 24.mar",x=13,y=35,hjust=0,vjust=.5,size=3) +
  labs(y="Variação % em relação ao normal",color="Atividade",
       title="Isolamento Social e mobilidade no DF",
       x="",
       caption = "Fonte: Google Mobility Report, 2020") +
  hrbrthemes::theme_ipsum() +
  theme(panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))

write_rds(mob_plot,"mob_plot.rds")
=======



# dados impostos nacional ----
ICMS_br <- read_excel("Arrecadação ICMS por estado.xlsx")%>% 
  pivot_longer(-1,values_to = "receita_realizada") %>%
  mutate(name=gsub("x","",name)) %>%
  separate(name,into=c("mes","exercicio"),sep = "_") %>%
  mutate(mes=as.numeric(mes),
         exercicio=as.numeric(exercicio),) %>%
  left_join(IGP) %>%
  mutate(receita = receita_realizada*inflator)

ICMS_br %>%
  left_join(cods_ibge) %>% left_join(pop) %>%
  dplyr::filter(#UF !="PA",
                mes>1) %>%
  mutate(receita=receita/populacao,
         exercicio=factor(exercicio)) %>%
  ggplot() +
  geom_bar(aes(fill=exercicio,group=exercicio,x=mes,y=receita),
           color=NA,position = "dodge",stat = "identity",width = .7) +
  # geom_text(aes(y=receita,x=mes,group=exercicio,label=paste0(round(receita,0)," mi ")),
  #           color="white",fontface="bold",
  #           position = position_dodge(width=.7),
  #           angle = 90, hjust = 'right'
  # ) + 
  labs(x="",y="ICMS per capita") +
  scale_x_continuous(breaks = c(1:6),
                     labels = c("Jan","Fev","Mar","Abr","Mai","Jun")) +
  facet_wrap(vars(UF),ncol = 7) + #hrbrthemes::theme_ipsum() +
  theme(legend.position = "bottom")

ICMS_br_wide <- ICMS_br %>%
  dplyr::select(-inflator,-receita_realizada) %>%
  pivot_wider(names_from = exercicio,values_from=receita) %>%
  mutate(dif_receita=`2020`-`2019`,
         perc_receita=round(dif_receita*100/`2020`,1),
         resultado=ifelse(dif_receita>0,'positivo','negativo')) 

# mensalizando covid e plotando resultados
ICMS_br_wide %>%
 
  dplyr::filter(#!(UF %in% c("PA","RN")),
                mes>1) %>%
  group_by(UF) %>%
  mutate(soma=sum(dif_receita,na.rm=T),
         perc_receita_tot=round((sum(`2020`,na.rm = T)-sum(`2019`,na.rm = T))*100/sum(`2020`,na.rm = T),0),
         
         legenda=paste0(UF,": ",round(soma/10^6,0),"mi"," (",perc_receita_tot,"%)")) %>%
  left_join(
    SRAG %>%
      mutate(
        mes=case_when(
          Semana %in% c(1:5) ~ 1,
          Semana %in% c(6:9) ~ 2,
          Semana %in% c(10:13) ~ 3,
          Semana %in% c(14:17) ~ 4,
          Semana %in% c(18:22) ~ 5,
          Semana %in% c(23:27) ~ 6,
        )
      ) %>%
      group_by(UF,mes) %>%
      summarise(
                covid=sum(covid,na.rm=T),
                srag_excesso=sum(srag_excesso,na.rm=T),
                populacao = mean(populacao,na.rm=T)) %>%
      mutate(
             covid_100k=covid*10^5/populacao,
             srag_excesso_100k = srag_excesso*10^5/populacao)
  ) %>%
  ggplot(aes(x=mes)) +
  geom_bar(aes(y=perc_receita,fill=resultado),
           color=NA,stat = "identity",width = .7,alpha=.75) +
  # geom_text(aes(y=0,color=resultado,
  #               label=paste0(round(dif_receita/10^6,0)," mi")),
  #           fontface="bold",family="Arial Narrow",
  #           vjust = -.50
  # ) + 
  geom_line(aes(y=srag_excesso_100k/2),color="black") +
  scale_x_continuous(breaks = c(1:6),
                     labels = c("Jan","Fev","Mar","Abr","Mai","Jun")) +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*2, name="Infectados por 100k habitantes (linha)",breaks = c(0,25,50,75,100))) +
  labs(y="Receita2020 - Receita2019 (% - barras)",
       x="",
       #title="Evolução das receitas tributárias (ICMS 2020) - Fev-Jun e avanço da COVID-19",
       caption = "Fonte (receitas): CONFAZ, 2020\nFonte (COVID-19): FioCruz, 2020\nObs.:valores em R$ de jul/2020") +
  facet_wrap(vars(legenda),ncol=7) + theme_minimal()+ theme(legend.position = "none") #+
  # hrbrthemes::theme_ipsum() +
  # coord_cartesian(clip = "on",
  #                 #ylim = c(0,110),
  #                 expand = F) +
  # theme(legend.position = "none",
  #       panel.spacing=grid::unit(.75, "lines"),
  #       plot.margin = ggplot2::margin(2, 2, 2, 0)) 
  
  
# mensalizando covid e plotando resultados
SRAG %>%
    dplyr::filter(Semana>6,
                  Semana<28) %>%
    
    left_join(
      ICMS_br_wide %>%
        mutate(
          Semana=case_when(
            mes == 1 ~ 4,
            mes == 2 ~ 8,
            mes == 3 ~ 12,
            mes == 4 ~ 16,
            mes == 5 ~ 20,
            mes == 6 ~ 24
          )
        )) %>%
    group_by(UF) %>%
    mutate(soma=sum(dif_receita,na.rm=T),
           perc_receita_tot=round((sum(`2020`,na.rm = T)-sum(`2019`,na.rm = T))*100/sum(`2020`,na.rm = T),0),
           
           legenda=paste0(UF,": ",perc_receita_tot,"%"," (",round(soma/10^6,0),"mi)")) %>%
    ggplot(aes(x=Semana)) +
    geom_bar(aes(y=perc_receita,fill=resultado),
             color=NA,stat = "identity",width = 3,alpha=.75) +
    # geom_text(aes(y=0,color=resultado,
    #               label=paste0(round(dif_receita/10^6,0)," mi")),
    #           fontface="bold",family="Arial Narrow",
    #           vjust = -.50
    # ) +
    geom_line(aes(y=srag_excesso_100k/2),color="black") +
    scale_x_continuous(breaks = c(8,12,16,20,24),
                       labels = c("Fev","Mar","Abr","Mai","Jun")) +
    scale_y_continuous(sec.axis = sec_axis( trans=~.*2, name="Novos casos por 100k habitantes (confirmados e SRAG excedente) (linha)",
                                            breaks = c(0,25,50,75,100))) +
    labs(y="Receita2020 - Receita2019 (% - barras)",
         x="",
         #title="Evolução das receitas tributárias (ICMS 2020) - Fev-Jun e avanço da COVID-19",
         caption = "Fonte: CONFAZ, 2020\nInfoGripe, 2020\n1 valores em R$ de jul/2020\n2 SRAG excedente = valor acima\nda mediana 2009-2019 do total de\ninfecções por Síndrome Respiratória\nAguda Grave") +
    facet_wrap(vars(legenda),ncol=7) + theme_minimal()+ theme(legend.position = "none") #+


# Queda geral ICMS
ICMS_br_resumo <- ICMS_br_wide %>%
  dplyr::filter(mes>1) %>%
  group_by(UF) %>%
  summarise(soma=sum(dif_receita,na.rm=T),
         perc_receita_tot=round((sum(`2020`,na.rm = T)-sum(`2019`,na.rm = T))*100/sum(`2020`,na.rm = T),0)) %>%
  left_join(cods_ibge) %>%
  left_join(pop) %>%
  mutate(pcapita=soma/populacao)
  ggplot(aes(x=Semana)) +
  geom_bar(aes(y=perc_receita,fill=resultado),
           color=NA,stat = "identity",width = 3,alpha=.75) +
  # geom_text(aes(y=0,color=resultado,
  #               label=paste0(round(dif_receita/10^6,0)," mi")),
  #           fontface="bold",family="Arial Narrow",
  #           vjust = -.50
  # ) + 
  geom_line(aes(y=srag_excesso_100k/2),color="black") +
  scale_x_continuous(breaks = c(8,12,16,20,24),
                     labels = c("Fev","Mar","Abr","Mai","Jun")) +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*2, name="Novos confirmados por 100k habitantes + SRAG excedente (linha)",breaks = c(0,25,50,75,100))) +
  labs(y="Receita2020 - Receita2019 (% - barras)",
       x="",
       #title="Evolução das receitas tributárias (ICMS 2020) - Fev-Jun e avanço da COVID-19",
       caption = "Fonte (receitas): CONFAZ, 2020\nFonte (COVID-19): FioCruz, 2020\nObs.:valores em R$ de jul/2020") +
  facet_wrap(vars(legenda),ncol=7) + theme_minimal()+ theme(legend.position = "none") #+
>>>>>>> 4386ef3b44920ae8e0d81adefb323c5ddd3b60e5
