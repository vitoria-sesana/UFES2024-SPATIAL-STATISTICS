# análises descritivas e espaciais

rm(list = ls())

options(scipen = 999)

# biblioteca e bases ------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(RColorBrewer)
library(tmap)
library(tmaptools)
library(ggspatial)

base_shape <- sf::st_read('bases/malha_municipal_ES_2022/ES_Municipios_2022.shp') %>% 
  mutate(CD_MUN = as.numeric(CD_MUN)) 

base <- read.table("bases/base.txt", sep = ";") %>%
  left_join(base_shape, "CD_MUN") %>% 
  sf::st_as_sf()

# mapa casos -------------------------------------------------------

ggplot() +
  geom_sf() +
  geom_sf(data = base, aes(fill = casos), color=gray(.5)) +
  labs(title = "") +
  scale_fill_distiller(palette = "Reds", direction = 1, 
                       name='\n', 
                       #labels = scales::percent()
  ) +
  scale_x_continuous(limits = c(-42, -39.7)) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.24, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(0.4, "in")) +
  ggtitle("Casos de nascidos vivos de mães menores que 19 anos", subtitle = "78 municípios do Espírito Santo") +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.grid.major = element_line(color = gray(0.8),
                                        linetype = "dashed", 
                                        size = 0.1),
        panel.background = element_rect(fill = "aliceblue"))



# correlação com as variaveis ---------------------------------------------

corrplot::corrplot(
  cor(as.data.frame(base)[,2:11]),
  type = 'lower',
  method = "color",
  cl.ratio = 0.2,
  number.cex = 0.7,
  addCoef.col ='black',
  tl.cex = 0.7,
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


# Matriz de vizinhaça -----------------------------------------------------
ccMatriz_vizinhanca = nb2mat(ccVizinhos, style='W') 


# Soma dos pesos da matriz de vizinhaça -----------------------------------
ccSoma_pesos_totais = rowSums(ccMatriz_vizinhanca)

# índice de moran ---------------------------------------------------------

lag_espacial <- spdep::lag.listw(ccPesos_vizinhos, base$casos)
lag_espacial

plot(lag_espacial ~ base$casos, pch=16, asp=1)
abline(lm(lag_espacial ~ base$casos), col="blue")

# autocorrelacao espacial local -------------------------------------------

I <- moran(base$casos, ccPesos_vizinhos, length(ccVizinhos), Szero(ccPesos_vizinhos))[1]
I
test_I <- moran.test(base$casos, ccPesos_vizinhos,
                     alternative="greater", 
                     zero.policy=TRUE) 
test_I
test_I$p.value
test_I[["estimate"]][["Moran I statistic"]]


# autocorrelacao espacial global ------------------------------------------

moran.plot(base$casos, listw = ccPesos_vizinhos)
localmoran <- localmoran(base$casos, ccPesos_vizinhos)
localmoran
moran.map <- cbind(base, localmoran)
moran.map


# Get points coordinates for sf_large extent
box <- st_sfc(bbox_poly(base)) %>%
  st_set_crs(st_crs(base)) %>%
  st_as_sf() %>%
  st_cast("POINT") %>%
  distinct() %>%
  st_coordinates()

# # Get lon/lat decimal degrees difference
lon_diff <- as.numeric(abs(box[1,1] - box[4,1]))
lat_diff <- as.numeric(abs(box[1,2] - box[2,2]))

sf_small_cent <- st_as_sfc(st_bbox(base), crs = st_crs(base)) %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_coordinates()

# Create bbox for extent of sf_small tmap() plot by offsetting sf_small centroid
bb <- st_as_sfc(st_bbox(c(xmin = sf_small_cent[1] - (lon_diff / 2),
                          xmax = sf_small_cent[1] + (lon_diff / 2),
                          ymin = sf_small_cent[2] - (lat_diff / 2),
                          ymax = sf_small_cent[2] + (lat_diff / 2)))) %>%
  st_as_sf(crs = st_crs(sf_small))


tmap::tm_shape(moran.map) +
  tmap::tm_fill(col = "Ii",
                style = "quantile",
                title = "Estatistica de Moran Local")+
  tmap::tm_polygons()
