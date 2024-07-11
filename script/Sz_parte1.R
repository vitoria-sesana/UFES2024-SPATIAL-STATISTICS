# parte 1: apresentação dia 27/06/24

library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(RColorBrewer)
library(tmap)
library(tmaptools)
library(ggspatial)


base_shape <- sf::st_read('../bases/malha_municipal_ES_2022/ES_Municipios_2022.shp') %>%
  mutate(CD_MUN = as.numeric(CD_MUN))
base <- read.table("../bases/base.txt", sep = ";") %>%
  left_join(base_shape, "CD_MUN") %>%
  sf::st_as_sf()
ggplot() +
  geom_sf(data = base, aes(fill = casos), color=grDevices::grey(.1)) +
  labs(title = "") +
  scale_fill_fermenter(palette = "Reds",
                       direction = 1,
                       limits = c(0, 800)
                       #labels = scales::percent()
  ) +
  scale_x_continuous(limits = c(-42, -39.7)) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.24, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(0.4, "in")) +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.grid.major = element_line(color = gray(0.8),
                                        linetype = "dashed",
                                        size = 0.1),
        panel.background = element_rect(fill = "aliceblue"),
        axis.line = element_line(color = "black" ),
        axis.line.x.top = element_line(color = "black" ),
        axis.line.y.right = element_line(color = "black" )
  )
corrplot::corrplot(
  cor(as.data.frame(base)[,2:11]),
  type = 'lower',
  method = "color",
  cl.ratio = 0.2,
  number.cex = 0.9,
  addCoef.col ='black',
  tl.cex = 0.9,
  tl.col = 'black',
  col = corrplot::COL2('PuOr', 10),
  addgrid.col = "black",
)
# Listas de vizinhança ----------------------------------------------------
# lista os municípios vizinhos que cada município possui
# a partir de uma lista de polígonos
ccVizinhos = poly2nb(base)
#  Lista dos pesos da vizinhança -------------------------------------------
# determinando os pesos de cada município a partir da lista de vizinhaça
# style escolhe o tipo de peso de cada vizinho para aquele município
ccPesos_vizinhos = nb2listw(ccVizinhos, style='W')
base$NM_MUN[ccPesos_vizinhos$neighbours[[78]]]
ccPesos_vizinhos$weights[[78]]
# Matriz de vizinhaça -----------------------------------------------------
ccMatriz_vizinhanca = nb2mat(ccVizinhos, style='W')
# Soma dos pesos da matriz de vizinhaça -----------------------------------
ccSoma_pesos_totais = rowSums(ccMatriz_vizinhanca)
# índice de moran ---------------------------------------------------------
lag_espacial <- spdep::lag.listw(ccPesos_vizinhos, base$casos)
plot(lag_espacial ~ base$casos, pch=16, asp=1, xlab ="Quantidade de casos", ylab = "Lag espacial")
abline(lm(lag_espacial ~ base$casos), col="blue")
test_I <- moran.test(base$casos, ccPesos_vizinhos,
                     alternative="greater",
                     zero.policy=TRUE)
# test_I
# test_I$p.value
# test_I[["estimate"]][["Moran I statistic"]]
localmoran <- localmoran(base$casos, ccPesos_vizinhos)
moran.map <- cbind(base, localmoran)
tmap::tm_shape(moran.map, bbox=tmaptools::bb(base, xlim = c(-42.5,-39.5))) +
  tmap::tm_fill(col = "Ii",
                style = "quantile",
                title = "Estatistica de Moran Local")+
  tmap::tm_polygons()
savehistory("C:/Users/Vitoria/Documents/Vitoria/faculdade/faculdade_disciplinas/semestre_atual/9. Estatística Espacial/UFES2024-SPATIAL-STATISTICS/aaaa.Rhistory")
