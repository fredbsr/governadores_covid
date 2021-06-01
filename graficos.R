## Graficos

# Lendo dados
source("leitura_governadores_covid_fiocruz_v2.R")

# Grafico Por UF
SRAG %>%
  dplyr::filter(ano_semana<yearweek("2021 W12")) %>%
  #  dplyr::filter(Semana<29) %>% # Semana
  #   dplyr::filter(Semana>10) %>% # Semana
  #  dplyr::filter(UF=="DF") %>% # DF
  ggplot(aes(x=ano_semana)) +
  #  geom_area(aes(y=srag_excesso_100k,fill="SRAG excedente"),alpha=.5) + #Portugues
  geom_area(aes(y=srag_excesso_100k,fill="Exceeding SARS"),alpha=.5) + #Ingles
  geom_area(aes(y=covid_100k,fill="COVID-19")) +
  # scale_x_continuous(breaks = c(4,8,12,16,20,24,28),
  #                    labels = c("25.jan","22.fev","21.mar","18.abr",
  #                               "16.mai","13.jun","11.jul"),
  #                    limits = c(10,25)) +
  scale_x_yearweek(date_breaks = "3 months",
                   date_labels = "%b%y") +
  
  # scale_x_continuous("Dia (dado semanal)",
  #                    breaks = c(10,15,20,25),
  #                    labels = c("07.mar","11.abr",
  #                               "16.mai","20.jun"),
  #                    limits = c(10,28)) +
  facet_wrap(vars(UF),ncol = 5) +
  #labs(fill="",y="Casos por 100k hab") + #Por 
  labs(fill="",y="Cases/100k",x="Epidemiological Week",
       caption = "Source: InfoGripe,2020\n
                  (Exceeding SARS is current values 
                  excedding 2009-2019 median)") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.15, "lines"),
        plot.margin = ggplot2::margin(1, 1, 1, 1))


# Graficos por regiao


# Fazendo varios graficos ao mesmo tempo de normas
g.srag <- purrr::map(levels(factor(SRAG$Regiao)),
                     function(x) {
                       ggplot(data = filter(SRAG,
                                            Regiao == x,
                                            ano_semana<yearweek("2021 W08")),
                              aes(x=ano_semana)) +
                         geom_area(aes(y=srag_excesso_100k,fill="Covid-19 Estimado"),alpha=.5) +
                         geom_area(aes(y=covid_100k,fill="Covid-19 oficial")) +
                         scale_x_yearweek(date_breaks = "3 months",
                                          date_labels = "%b%y") +
                         facet_wrap(vars(UF),ncol = 3) +
                         labs(fill="",y="Casos/100k",x="",
                              caption = "InfoGripe,2020",
                              title = x) +
                         hrbrthemes::theme_ipsum() +
                         theme(legend.position = "top",
                               panel.spacing=grid::unit(.15, "lines"),
                               plot.margin = ggplot2::margin(1, 1, 1, 1))
                     })


ggsave(file='srag1.png',plot=g.srag[[1]],width = 8,height = 4.5)
ggsave(file='srag2.png',plot=g.srag[[2]],width = 8,height = 6.5)
ggsave(file='srag3.png',plot=g.srag[[3]],width = 8,height = 6.5)
ggsave(file='srag4.png',plot=g.srag[[4]],width = 8,height = 4.5)
ggsave(file='srag5.png',plot=g.srag[[5]],width = 8,height = 2.5)




# Grafico Brasil
SRAG %>%
  group_by(ano_semana) %>%
  summarise(covid=sum(covid,na.rm=T),
            srag_excesso = sum(srag_excesso,na.rm=T),
            populacao=sum(populacao,na.rm=T)) %>%
  ungroup() %>%
  mutate(covid_100k=covid*10^5/populacao,
         srag_excesso_100k = srag_excesso*10^5/populacao,
         tx_subnotifica = srag_excesso_100k/covid_100k,
         total_subnotifica = srag_excesso_100k-covid_100k
  ) %>%
  dplyr::filter(ano_semana<yearweek("2021 W08")) %>%
  ggplot(aes(x=ano_semana)) +
  geom_area(aes(y=srag_excesso_100k,fill="Covid-19 Estimado"),alpha=.5) +
  geom_area(aes(y=covid_100k,fill="Covid-19 Oficial")) +
  geom_segment(data = . %>%
                 dplyr::filter(total_subnotifica==max(total_subnotifica)),
               aes(x = ano_semana, y = covid_100k, xend = ano_semana, yend = srag_excesso_100k),
               lineend = "square",linejoin = "mitre",
               arrow = arrow(angle = 90, length = unit(0.1, "inches"),
                             ends = "both", type = "open")) +
  geom_text(data = . %>%
              dplyr::filter(total_subnotifica==max(total_subnotifica)),
            aes(x = ano_semana, y = mean(c(covid_100k,srag_excesso_100k)),
                label=paste0(round(tx_subnotifica,2)," veces los datos oficiales ")),
            hjust=1,size=3) +
  scale_x_yearweek(date_breaks = "2 months",
                   date_labels = "%b%y") +
  labs(fill="",y="Casos/100k",x="",
       caption = "InfoGripe,2021") +
  # hrbrthemes::theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "top",
        panel.spacing=grid::unit(.15, "lines"),
        plot.margin = ggplot2::margin(1, 1, 1, 1))  

# Grafico Brasil Ingles
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

# Grafico Por UF
br.io %>%
  dplyr::filter(ano_semana<yearweek(last(br.io$ano_semana)-1)) %>%
  ggplot(aes(x=ano_semana)) +
  geom_area(aes(y=casos_100k,fill="Casos")) +
  scale_x_yearweek(date_breaks = "3 months",
                   date_labels = "%b%y") +
  facet_wrap(vars(UF),ncol = 5) +
  labs(fill="",y="Cases/100k (moving average)",x="Epidemiological Week",
       caption = "Source: Brasil.io,2021") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "none",
        panel.spacing=grid::unit(.15, "lines"),
        plot.margin = ggplot2::margin(1, 1, 1, 1))
