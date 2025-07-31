library(tidyverse)
library(extrafont)
library(ggtext)

pregunta <- c("ES HABITUAL QUE LAS MUJERES TENGAN MIEDO DE IR POR LA CALLE Y/O VOLVER SOLAS POR LA NOCHE",
              "HAY QUE SANCIONAR A LAS EMPRESAS QUE DISCRIMINAN, SALARIALMENTE O EN TÉRMINOS DE PROMOCIÓN, A LAS MUJERES CUANDO TIENEN HIJOS",
              "LA VIOLENCIA DE GÉNERO ES UNO DE LOS PROBLEMAS SOCIALES MÁS IMPORTANTES",
              "HAY QUE GARANTIZAR POR LEY LA PRESENCIA EQUILIBRADA DE MUJERES Y HOMBRES EN LOS ALTOS CARGOS PÚBLICOS Y EMPRESAS",
              "HABRÍA QUE ESTABLECER UNA REMUNERACIÓN PARA EL TRABAJO DOMÉSTICO EN EL PROPIO HOGAR*",
              "HABRÍA QUE MODIFICAR EL LENGUAJE PARA HACERLO MÁS INCLUSIVO CON LAS MUJERES")

hombre_2024 <- matrix(c(64.6, 19, 14.3, 2.1,
                        73, 16.7, 8.4, 1.9,
                        55.6, 21, 21.8, 1.5,
                        48.3, 21.9, 27.8, 2,
                        44.8, 26.8, 25.4, 2.9,
                        26, 25.6, 46.5, 1.8), ncol = 4, byrow = T)
colnames(hombre_2024) <- c("De acuerdo",
                          "Ni de acuerdo ni en desacuerdo",
                          "En desacuerdo",
                          "NS/NC")

hombre_2019 <- matrix(c(74.5, 13.6, 11.8,
                       85, 8.5, 6.5,
                       77.1, 12.2, 10.6,
                       64.7, 20, 15.3,
                       52.3, 23.3, 24.3,
                       33.9, 31.5, 34.6), ncol = 3, byrow = T)
colnames(hombre_2019) <- c("De acuerdo",
                          "Ni de acuerdo ni en desacuerdo",
                          "En desacuerdo")

mujer_2024 <- matrix(c(81.6, 9.9, 7.1, 1.4,
                       86.6, 8.2, 3.3, 1.9,
                       75.2, 14.9, 8.2, 1.6,
                       70, 16.9, 10.8, 2.4,
                       60.9, 21.3, 14.2, 3.6,
                       38.3, 28.2, 30.7, 2.8), ncol = 4, byrow = T)
colnames(mujer_2024) <- c("De acuerdo",
                          "Ni de acuerdo ni en desacuerdo",
                          "En desacuerdo",
                          "NS/NC")

mujer_2019 <- matrix(c(85.1, 6.9, 7.8,
                       92.5, 4.2, 3.3,
                       89.6, 6.4, 3.9,
                       81.6, 11.4, 6.9,
                       65.9, 18.7, 15.7,
                       51.3, 27.5, 21.1), ncol = 3, byrow = T)
colnames(mujer_2019) <- c("De acuerdo",
                          "Ni de acuerdo ni en desacuerdo",
                          "En desacuerdo")

d24 <- rbind(mujer_2024, hombre_2024) %>% as.data.frame() %>% mutate(genero = rep(c("Mujeres", "Hombres"), each = 6)) %>%
  mutate(pregunta = rep(pregunta, 2),
         anio = 2024) %>% pivot_longer(-c("pregunta", "genero", "anio"), names_to = "resp", values_to = "porc")

d19 <- rbind(mujer_2019, hombre_2019) %>% as.data.frame() %>% mutate(genero = rep(c("Mujeres", "Hombres"), each = 6)) %>%
  mutate(pregunta = rep(pregunta, 2),
         anio = 2019) %>% pivot_longer(-c("pregunta", "genero", "anio"), names_to = "resp", values_to = "porc")

rbind(d24, d19) %>% mutate(resp = factor(resp,
                                         levels = c("De acuerdo", "Ni de acuerdo ni en desacuerdo",
                                                    "En desacuerdo", "NS/NC"))) %>%
  ggplot(aes(x = anio, y = porc, fill = resp)) + geom_col() + facet_wrap(~pregunta+genero) +
  theme(legend.position = "bottom")

rbind(d24, d19) %>% mutate(resp = factor(resp,
                                         levels = c("De acuerdo", "Ni de acuerdo ni en desacuerdo",
                                                    "En desacuerdo", "NS/NC"))) %>%
  mutate(pregunta = paste0(substr(pregunta, 1,1),
                           tolower(str_sub(pregunta, 2, -1)))) %>%
  filter(resp == "De acuerdo") %>%
  ggplot(aes(x = genero, y = porc, group = genero, fill = as.factor(anio),
             colour = as.factor(anio),
             label = paste0(porc, "%"))) +
  geom_line(colour = "black") + geom_point(size = 4, pch = 21, colour = "black") +
  geom_text(hjust = rep(c(-0.3, 1.2), each = 12), size = 4,
            family = "Roboto Black") +
  # geom_text(aes(x = genero, y = porc, group = genero, colour = as.factor(anio),
  #               label = anio),
  #           nudge_y = rep(c(-5, 5), each = 12), size = 3) +
  facet_wrap(~str_wrap(pregunta,25), scales = "free_x") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_viridis_d("Año", begin = 0.35, end = 0.65) +
  scale_colour_viridis_d("Año", begin = 0.35, end = 0.55) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", axis.title.x = element_blank(),
        plot.background = element_rect(fill = "#F6F4ED", colour = "#F6F4ED"),
        text = element_text(family = "Roboto"),
        axis.text.x = element_text(size = 15),
        plot.title = element_markdown(family = "Roboto Black"),
        plot.subtitle = element_markdown(),
        plot.caption = element_text(family = "Roboto Condensed")) +
  labs(title = "El feminismo entre la juventud: cambio de <span style='color:#2F6C8E;'>2019</span> a <span style='color:#2FB47C;'>2024</span>",
       subtitle = "Porcentaje de jóvenes que se muestran **de acuerdo** con<br>algunas afirmaciones relacionadas con la igualdad de género,<br>según el año de la encuesta (<span style='color:#2F6C8E;'>azul</span>, 2019; <span style='color:#2FB47C;'>verde</span>, 2024).",
       y = "Porcentaje (%) de acuerdo con la afirmación",
       caption = "Fuente: Informes Juventud en España 2020 y 2024.\nEn 2019 la población objetivo son jóvenes de 15 a 29 años; en 2024, de 15 a 34 años.\n*En 2019 esta afirmación se formula simplemente como\n'Habría que establecer una remuneración para el trabajo doméstico'")
ggsave("s2_deacuerdo.png", dpi = 300, width = 7.64, height = 10.64, device = grDevices::png)

rbind(d24, d19) %>% mutate(resp = factor(resp,
                                         levels = c("De acuerdo", "Ni de acuerdo ni en desacuerdo",
                                                    "En desacuerdo", "NS/NC"))) %>%
  mutate(pregunta = paste0(substr(pregunta, 1,1),
                           tolower(str_sub(pregunta, 2, -1)))) %>%
  filter(resp == "En desacuerdo") %>%
  ggplot(aes(x = genero, y = porc, group = genero, fill = as.factor(anio),
             colour = as.factor(anio),
             label = paste0(porc, "%"))) +
  geom_line(colour = "black") + geom_point(size = 4, pch = 21, colour = "black") +
  geom_text(hjust = rep(c(-0.3, 1.2), each = 12), size = 4,
            family = "Roboto Black") +
  # geom_text(aes(x = genero, y = porc, group = genero, colour = as.factor(anio),
  #               label = anio),
  #           nudge_y = rep(c(-5, 5), each = 12), size = 3) +
  facet_wrap(~str_wrap(pregunta,25), scales = "free_x") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_viridis_d("Año", begin = 0.35, end = 0.65) +
  scale_colour_viridis_d("Año", begin = 0.35, end = 0.55) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", axis.title.x = element_blank(),
        plot.background = element_rect(fill = "#F6F4ED", colour = "#F6F4ED"),
        text = element_text(family = "Roboto"),
        axis.text.x = element_text(size = 15),
        plot.title = element_markdown(family = "Roboto Black"),
        plot.subtitle = element_markdown(),
        plot.caption = element_text(family = "Roboto Condensed")) +
  labs(title = "El feminismo entre la juventud: cambio de <span style='color:#2F6C8E;'>2019</span> a <span style='color:#2FB47C;'>2024</span>",
       subtitle = "Porcentaje de jóvenes que se muestran **en desacuerdo** con<br>algunas afirmaciones relacionadas con la igualdad de género,<br>según el año de la encuesta (<span style='color:#2F6C8E;'>azul</span>, 2019; <span style='color:#2FB47C;'>verde</span>, 2024).",
       y = "Porcentaje (%) en desacuerdo con la afirmación",
       caption = "Fuente: Informes Juventud en España 2020 y 2024.\nEn 2019 la población objetivo son jóvenes de 15 a 29 años; en 2024, de 15 a 34 años.\n*En 2019 esta afirmación se formula simplemente como\n'Habría que establecer una remuneración para el trabajo doméstico'")
ggsave("s2_endesacuerdo.png", dpi = 300, width = 7.64, height = 10.64, device = grDevices::png)

##############################

feminismo24 <- data.frame(matrix(c(1:10, "NC",
                                  7.7, 3.2, 6.0, 6.6, 12.0, 9.7, 13.6, 15.9, 9.7, 13.7, 1.9,
                                  11.7, 4.4,8.2,8.9,14.7,10.0, 13.0,12.6,7.0,7.6,1.9,
                                  3.5, 2.0, 3.7, 4.1, 9.2, 9.3, 14.2, 19.5, 12.6, 20.1, 1.9
                                  ), ncol = 4, byrow = F))
colnames(feminismo24) <- c("punt", "total", "hombres", "mujeres")

igualdad24 <- data.frame(matrix(c(1:10, "NC",
                                  5.3,1.6,3.7,4.4,12.7,8.6,13.9,17.7,12.0,17.7,2.3,
                                  8.4,2.2,6.2,6.0,17.2,9.1,14.4,12.5,7.2, 14.2, 2.6,
                                  2.1,1.0,1.2,2.7,8.1,8.1,13.5,23.1,16.9,21.3,1.9
                                  ), ncol = 4, byrow = F))
colnames(igualdad24) <- c("punt", "total", "hombres", "mujeres")

feminismo19 <- data.frame(matrix(c("Bajo", "Medio", "Alto",
                                   10.3, 38.2, 51.5,
                                   4.8, 12.5, 82.7,
                                   8, 30.1, 61.1,
                                   3.1, 13.8, 83.1,
                                   12.3, 38, 49.6,
                                   3.5, 19.3, 77.2
                                   ), ncol = 7, byrow = F))
colnames(feminismo19) <- c("punt", "h15_19", "m15_19",
                           "h20_24", "m20_24",
                           "h25_29", "m25_29")

igualdad19 <- data.frame(matrix(c("Bajo", "Medio", "Alto",
                                   6.5, 28.1, 65.5,
                                  2.8, 13.9, 83.3,
                                  5.6, 24.1, 70.4,
                                  1.8, 11, 87.2,
                                  5.3, 21.7, 73,
                                  2.8, 14, 83.1
                                  ), ncol = 7, byrow = F))
colnames(igualdad19) <- c("punt", "h15_19", "m15_19",
                           "h20_24", "m20_24",
                           "h25_29", "m25_29")

g1 <- feminismo24 %>%
  mutate(total = as.numeric(total), mujeres = as.numeric(mujeres), hombres = as.numeric(hombres)) %>%
  mutate(total_sinNC = as.numeric(c(total[1:10], NA)),
                       mujeres_sinNC = as.numeric(c(mujeres[1:10], NA)),
                       hombres_sinNC = as.numeric(c(hombres[1:10], NA))) %>%
  mutate(total_sinNC = 100*total_sinNC/sum(total_sinNC, na.rm = T),
         mujeres_sinNC = 100*mujeres_sinNC/sum(mujeres_sinNC, na.rm = T),
         hombres_sinNC = 100*hombres_sinNC/sum(hombres_sinNC, na.rm = T)) %>%
  pivot_longer(-punt, names_to = "genero", values_to = "porc") %>%
  group_by(genero) %>%
  summarise(sieteomas = sum(porc[which(punt %in% 7:10)]),
            seisomas = sum(porc[which(punt %in% 6:10)]),
            ochoomas = sum(porc[which(punt %in% 8:10)])) %>% ungroup() %>%
  pivot_longer(-genero, names_to = "punt", values_to = "porc") %>%
  mutate(punt = factor(punt, levels = c("seisomas", "sieteomas", "ochoomas"),
                       labels = c("6 o más", "7 o más", "8 o más"))) %>%
  filter(genero %in% c("hombres_sinNC", "mujeres_sinNC") & punt == "7 o más") %>%
  mutate(genero = c("Hombres", "Mujeres")) %>%
  ggplot(aes(x = genero, y = porc,
             label = paste0("Total 15-34 años: ", round(porc,1), "%"))) + 
  geom_point(alpha = 0) +
  geom_segment(aes(x = 1, xend = 1, y = 41, yend = 61.1)) +
  geom_segment(aes(x = 2, xend = 2, y = 67.6, yend = 83.1)) +
  geom_point(size = 5, pch = 21, fill = "#2FB47C", col = "black") +
  geom_point(data = feminismo19 %>%
               mutate_at(c("h15_19", "m15_19",
                           "h20_24", "m20_24",
                           "h25_29", "m25_29"), as.numeric) %>%
               pivot_longer(-punt, names_to = "gen_edad", values_to = "porc") %>%
               filter(punt == "Alto") %>%
               mutate(genero = rep(c("Hombres", "Mujeres"), 3),
                      edad = rep(c("15-19 años", "20-24 años", "25-29 años"), each = 2)),
               mapping = aes(x = genero, y = porc, fill = edad), size = 5, pch = 21) +
  ggrepel::geom_text_repel(data = feminismo19 %>%
                             mutate_at(c("h15_19", "m15_19",
                                         "h20_24", "m20_24",
                                         "h25_29", "m25_29"), as.numeric) %>%
                             pivot_longer(-punt, names_to = "gen_edad", values_to = "porc") %>%
                             filter(punt == "Alto") %>%
                             mutate(genero = rep(c("Hombres", "Mujeres"), 3),
                                    edad = rep(c("15-19 años", "20-24 años", "25-29 años"), each = 2)),
                           mapping = aes(x = genero, y = porc, colour = edad,
                                         label = paste0(edad, ": ", round(porc,1), "%")),
                           family = "Roboto Black", nudge_x = rep(c(-0.3,0.3,-0.3), 2)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(size = 4,
            family = "Roboto Black", nudge_y = -5, col = "#2FB47C") +
  scale_fill_viridis_d("Año", begin = 0.35, end = 0.65, option = "magma") +
  scale_colour_viridis_d("Año", begin = 0.35, end = 0.55, option = "magma") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", axis.title.x = element_blank(),
        plot.background = element_rect(fill = "#F6F4ED", colour = "#F6F4ED"),
        text = element_text(family = "Roboto"),
        plot.title = element_markdown(family = "Roboto Black"),
        plot.subtitle = element_markdown(),
        axis.text.x = element_text(size = 20)) +
  labs(subtitle = "Porcentaje de jóvenes con un nivel de identificación<br>alto
       con la lucha **feminista**, según año de la encuesta<br>
       (<span style='color:#782281;'>tonos de morado</span>, 2019; <span style='color:#2FB47C;'>verde</span>, 2024).",
       y = "Porcentaje (%)")


g2 <- igualdad24 %>%
  mutate(total = as.numeric(total), mujeres = as.numeric(mujeres), hombres = as.numeric(hombres)) %>%
  mutate(total_sinNC = as.numeric(c(total[1:10], NA)),
         mujeres_sinNC = as.numeric(c(mujeres[1:10], NA)),
         hombres_sinNC = as.numeric(c(hombres[1:10], NA))) %>%
  mutate(total_sinNC = 100*total_sinNC/sum(total_sinNC, na.rm = T),
         mujeres_sinNC = 100*mujeres_sinNC/sum(mujeres_sinNC, na.rm = T),
         hombres_sinNC = 100*hombres_sinNC/sum(hombres_sinNC, na.rm = T)) %>%
  pivot_longer(-punt, names_to = "genero", values_to = "porc") %>%
  group_by(genero) %>%
  summarise(sieteomas = sum(porc[which(punt %in% 7:10)]),
            seisomas = sum(porc[which(punt %in% 6:10)]),
            ochoomas = sum(porc[which(punt %in% 8:10)])) %>% ungroup() %>%
  pivot_longer(-genero, names_to = "punt", values_to = "porc") %>%
  mutate(punt = factor(punt, levels = c("seisomas", "sieteomas", "ochoomas"),
                       labels = c("6 o más", "7 o más", "8 o más"))) %>%
  filter(genero %in% c("hombres_sinNC", "mujeres_sinNC") & punt == "7 o más") %>%
  mutate(genero = c("Hombres", "Mujeres")) %>%
  ggplot(aes(x = genero, y = porc,
             label = paste0("Total 15-34 años: ", round(porc,1), "%"))) + 
  geom_point(alpha = 0) +
  geom_segment(aes(x = 1, xend = 1, y = 49.6, yend = 73)) +
  geom_segment(aes(x = 2, xend = 2, y = 76.3, yend = 87.2)) +
  geom_point(size = 5, pch = 21, fill = "#2FB47C", col = "black") +
  geom_point(data = igualdad19 %>%
               mutate_at(c("h15_19", "m15_19",
                           "h20_24", "m20_24",
                           "h25_29", "m25_29"), as.numeric) %>%
               pivot_longer(-punt, names_to = "gen_edad", values_to = "porc") %>%
               filter(punt == "Alto") %>%
               mutate(genero = rep(c("Hombres", "Mujeres"), 3),
                      edad = rep(c("15-19 años", "20-24 años", "25-29 años"), each = 2)),
             mapping = aes(x = genero, y = porc, fill = edad), size = 5, pch = 21) +
  ggrepel::geom_text_repel(data = igualdad19 %>%
               mutate_at(c("h15_19", "m15_19",
                           "h20_24", "m20_24",
                           "h25_29", "m25_29"), as.numeric) %>%
               pivot_longer(-punt, names_to = "gen_edad", values_to = "porc") %>%
               filter(punt == "Alto") %>%
               mutate(genero = rep(c("Hombres", "Mujeres"), 3),
                      edad = rep(c("15-19 años", "20-24 años", "25-29 años"), each = 2)),
             mapping = aes(x = genero, y = porc, colour = edad,
                           label = paste0(edad, ": ", round(porc,1), "%")),
             family = "Roboto Black", nudge_x = rep(c(-0.3,0.3,-0.3), 2)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(size = 4,
            family = "Roboto Black", nudge_y = -5, col = "#2FB47C") +
  scale_fill_viridis_d("Año", begin = 0.35, end = 0.65, option = "magma") +
  scale_colour_viridis_d("Año", begin = 0.35, end = 0.55, option = "magma") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", axis.title.x = element_blank(),
        plot.background = element_rect(fill = "#F6F4ED", colour = "#F6F4ED"),
        text = element_text(family = "Roboto"),
        plot.title = element_markdown(family = "Roboto Black"),
        plot.subtitle = element_markdown(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20)) +
  labs(subtitle = "Porcentaje de jóvenes con un nivel de identificación<br>alto
       con la lucha **por la igualdad de género**, según año<br>de la encuesta
       (<span style='color:#782281;'>tonos de morado</span>, 2019; <span style='color:#2FB47C;'>verde</span>, 2024).",
       y = "Porcentaje (%)")

library(patchwork)
g1 + g2 + plot_annotation(title = "Identificación con la lucha feminista: cambio de <span style='color:#782281;'>2019</span> a <span style='color:#2FB47C;'>2024</span>",
                          caption = "Fuente: Informes Juventud en España 2020 y 2024.\nEn 2019: nivel 'alto' de identificación según la clasificación del informe (alto, medio, bajo). En 2024: puntuación ≥ 7 en la escala del 1 al 10.") &
  theme(plot.background = element_rect(fill = "#F6F4ED", colour = "#F6F4ED"),
        plot.title = element_markdown(family = "Roboto Black", size = 24),
        plot.caption = element_text(family = "Roboto Condensed", size = 15))


ggsave("luchas.png", dpi = 300, width = 11.85, height = 6.39, device = grDevices::png)

###########################################

data.frame(tema = c("El matrimonio entre personas del mismo sexo",
                    "La ocupación de viviendas vacías",
                    "Ayudar a morir a un enfermo incurable",
                    "Aplicar la pena de muerte para delitos graves",
                    "La enseñanza religiosa en escuelas"),
           fav2012 = c(77.5, 40.9, 77, 42.8, 30.7),
           fav2024 = c(78.1, 22, 75.8, 52.2, 25.3)) %>%
  pivot_longer(-tema, names_to = "anio", values_to = "porc") %>%
  ggplot(aes(x = porc, y = tema, group = tema, fill = as.factor(anio),
             colour = as.factor(anio),
             label = paste0(porc, "%"))) +
  geom_line(colour = "black") + geom_point(size = 6, pch = 21, colour = "black") +
  geom_text(hjust = c(1.2, -0.3,
                      -0.3, 1.3,
                      -0.3, 1.2,
                      1.2, -0.3,
                      -0.3, 1.2), size = 6,
            family = "Roboto Black") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_fill_viridis_d("Año", begin = 0.35, end = 0.65) +
  scale_colour_viridis_d("Año", begin = 0.35, end = 0.55) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", axis.title.y = element_blank(),
        plot.background = element_rect(fill = "#F6F4ED", colour = "#F6F4ED"),
        text = element_text(family = "Roboto"),
        axis.text.y = element_text(size = 15),
        plot.title = element_markdown(family = "Roboto Black"),
        plot.subtitle = element_markdown(),
        plot.caption = element_text(family = "Roboto Condensed")) +
  labs(title = "Valores de la juventud: cambio de <span style='color:#2F6C8E;'>2011</span> a <span style='color:#2FB47C;'>2024</span>",
       subtitle = "Porcentaje de jóvenes que se muestran **a favor** de<br>algunas posiciones relacionadas con temas controvertidos,<br>según el año de la encuesta (<span style='color:#2F6C8E;'>azul</span>, 2011; <span style='color:#2FB47C;'>verde</span>, 2024).",
       x = "Porcentaje (%) a favor",
       caption = "Fuente: Informes Juventud en España 2012 y 2024. En 2011 la población objetivo son jóvenes de 15 a 29 años; en 2024, de 15 a 34 años.")
ggsave("temas.png", dpi = 300, width = 10.59, height = 5.35, device = grDevices::png)

  
