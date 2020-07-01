
## Mapas acs ----
library(tidyverse)
library(sf)
library(tm) 
library(readODS)
library(magrittr)
library(ggplot2)
library(geobr)
library(cowplot)

# Lendo shape das regioes de saude
rs_map <- sf::read_sf("br_ars/br_regsaud_pol.shp")

rs_map %>% ggplot() +
  geom_sf(aes(fill=IBGE_CODE),show.legend = F,color=NA) +
  theme_void()

# Lendo dados de agentes comunitarios
rs_data <- read_ods("acs_por_regiao.ods") %>%
  janitor::clean_names() %>% 
  mutate(regiao_de_saude_cir = str_squish(regiao_de_saude_cir)) %>%
  separate(regiao_de_saude_cir,into=c("IBGE_CODE","nome"),extra = "merge") %>%
  pivot_longer(3:6) %>%
  dplyr::mutate(ano=str_replace(name,"dez_",""))


# Regiões brasil
regioes <- read_region()

# Join do mapa com o dado
rs_map_data <- rs_map %>%
  left_join(rs_data) %>%
  mutate(code_region=as.numeric(str_sub(IBGE_CODE,1,1))) %>%
  left_join(regioes %>% dplyr::select(1:2) %>% tibble())

# Mapa
ggplot(rs_map_data %>%
         dplyr::filter(!is.na(value),
                       ano=="2019"),
       aes(fill=value)) +
  geom_sf(show.legend = T,
          colour="NA") +
  viridis::scale_fill_viridis(direction = -1,
                              begin = .1,
                              end = .9,
                              limits=c(min(rs_map_data$value),
                                       max(rs_map_data$value))) +
  theme_void() +
  labs(fill="CHW Total") +
  theme(legend.position = "none") -> br_mapa


rs_map_data %<>%
  dplyr::filter(!is.na(value),
                ano=="2019")


g <- purrr::map(levels(factor(rs_map_data$name_region)),
                function(x) {
                  ggplot() +
                    geom_sf(data = filter(rs_map_data,
                                          name_region == x),
                            aes(fill=value),
                            show.legend = T,colour="NA") +
                    viridis::scale_fill_viridis(direction = -1,
                                                begin = .1,
                                                end = .9,
                                                limits=c(min(rs_map_data$value),
                                                         max(rs_map_data$value))) +
                    theme_void() +
                    labs(fill="CHW Total") +
                    theme(legend.position = "none")
                })

legend <- get_legend(
  # create some space to the left of the legend
  g[[1]] +
    guides(fill = guide_colourbar(barwidth = 1, 
                                  barheight = 5,
                                  ticks = FALSE,
                                  title.vjust = 0,
    )) +
    theme(legend.position = "right")
)

g2 <- cowplot::plot_grid(g[[1]],
                         g[[2]],
                         g[[3]],
                         g[[4]],
                         g[[5]],
                         legend,
                         labels = c("Center-West","Northeast","North","Southeast","South"))                 
#labels = levels(factor(rs_map_data$name_region)))

cowplot::plot_grid(g2,br_mapa)

br_mapa %>%
  dplyr::filter(.data$code_region ==1)

# Olhando dados

ggplot(rs_map_data %>%
         dplyr::filter(!is.na(value),
                       ano!="2009"),
       aes(x=ano,y=value,color=value)) +
  # 
  # geom_jitter(data=. %>% dplyr::filter(value>2500),
  #             color="red",size=3) +
  geom_jitter(data=. %>% dplyr::filter(value<2500),
              #color="grey50") +
              alpha=.7,size=1) +
  geom_text(data=. %>% dplyr::filter(value>2500),
            aes(label=paste(nome,value)),size=2.5,fontface="bold") +
  # gghighlight::gghighlight(value>5000,
  #                          label_key = nome) +
  viridis::scale_color_viridis(limits=c(min(rs_map_data$value),
                                        max(rs_map_data$value))) +
  guides(color = guide_colourbar(barwidth = 1, 
                                 barheight = 5,
                                 ticks = FALSE,
                                 title.vjust = 0,
  )) +
  labs(color="CHW Total") +
  theme_minimal() +
  theme(legend.position = "right")

missing <-
  rs_map_data %>%
  filter(is.na(value))

write_csv2(missing %>% dplyr::select(1:3),"missing_regsaud.csv")