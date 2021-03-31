library(outbreaks)
library(tidyverse)
library(incidence)
library(patchwork)

dat <- dados_abertos_perfil$data_cadastro
class(dat)
head(dat)

i <- incidence(dat)
i
plot(i)

# weekly, starting on Monday (ISO week, default)
i.7 <- incidence(dat, interval = "1 week")
plot(i.7)

# semi-weekly, starting on Saturday
i.14 <- incidence(dat, interval = "2 saturday weeks")
plot(i.14, border = "white")

i.7.sex <- incidence(dat, interval = "1 week", groups = dados_abertos_perfil$sexo)
i.7.sex
plot(i.7.sex, stack = TRUE, border = "grey")

#Muitos grupos
i.7.hosp <- with(dados_abertos_perfil,
                 incidence(data_cadastro, interval = "week", groups = faixa_etaria))
i.7.hosp

plot(i.7.hosp, stack=TRUE) +
  theme(legend.position= "top") +
  labs(fill="",y="Incidência")


best.fit <- fit_optim_split(i.7)
best.fit
best.fit$split

plot(i.7, fit=best.fit$fit)


# weekly, starting on Monday (ISO week, default)
i.7.new <- incidence(dados_abertos_perfil %>%
                   dplyr::filter(data_cadastro>dmy("31-01-2021"),
                                 data_cadastro<dmy("10-03-2021")) %>%
                 pull(data_cadastro) )
plot(i.7.new)

new.fit <- fit(i.7.new)
plot(i.7.new,fit=new.fit) +
  scale_x_date(limits = c(dmy("31-01-2021"),ymd(dmy("31-01-2021") + 46))) +
  labs(y="Incidência 2a. onda") -> w2


# weekly, starting on Monday (ISO week, default)
i.7.old <- incidence(dados_abertos_perfil %>%
                       dplyr::filter(data_cadastro>dmy("30-04-2020"),
                                     data_cadastro<ymd(best.fit$split)) %>%
                       pull(data_cadastro) )
plot(i.7.old)

old.fit <- fit(i.7.old)
plot(i.7.old,fit=old.fit) +
  scale_x_date(limits = c(dmy("30-04-2020"),ymd(best.fit$split))) +
  labs(y="Incidência 1a. onda") -> w1

w1 / w2
