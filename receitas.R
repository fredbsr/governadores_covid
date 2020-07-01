library(readr)
library(magrittr)
library(tidyverse)
library(readxl)
library(scales)
library(patchwork)
library(ggrepel)

IGP <- read_excel("igp.xlsx",sheet = 2)

transf_SIGA <- read_excel("siga_brasil.xlsx",sheet = 1)

mobilidade_dia <- read_delim("Global_Mobility_Report.csv",
                                ",", escape_double = FALSE, skip = 0,
                                locale = locale(decimal_mark = ".",
                                                grouping_mark = ","), trim_ws = TRUE) %>%
  janitor::clean_names() %>%
  dplyr::filter(iso_3166_2_code=="BR-DF") %>%
  mutate(Semana = lubridate::epiweek(date)) 

mobilidade <- mobilidade_dia %>%
  group_by(Semana) %>%
  summarise_if(is.numeric,mean,na.rm=T)


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
