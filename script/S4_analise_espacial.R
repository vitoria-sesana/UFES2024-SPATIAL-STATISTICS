options(scipen = 999)
# análise espacial

library(sf)
library(spdep)
library(RColorBrewer)
library(tmap)
library(tmaptools)

source(file = "script/S3_matriz_vizinhaca.R")
# usar taxa bruta

# índice de moran ---------------------------------------------------------

inc.lag <- spdep::lag.listw(ccListw, base_gravidez_tratado$qntd)
inc.lag

plot(inc.lag ~ base_gravidez_tratado$qntd, pch=16, asp=1)
abline(lm(inc.lag ~ base_gravidez_tratado$qntd), col="blue")

# autocorrelacao espacial local -------------------------------------------

I <- moran(base_gravidez_tratado$qntd, ccListw, length(ccNb), Szero(ccListw))[1]
I
test_I <- moran.test(base_gravidez_tratado$qntd, ccListw, alternative="greater", zero.policy=TRUE) 
test_I
test_I$p.value
test_I[["estimate"]][["Moran I statistic"]]


# autocorrelacao espacial global ------------------------------------------

moran.plot(base_gravidez_tratado$qntd, listw = ccListw)
localmoran <- localmoran(base_gravidez_tratado$qntd, ccListw)
localmoran
moran.map <- cbind(base_gravidez_tratado, localmoran)
moran.map


tmap::tm_shape(moran.map) +
  tmap::tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic")


# analise 2 ---------------------------------------------------------------

# moran <- localmoran(data_gravidez_tratado$numeros_absolutos, ccListw)
#
# data_gravidez_tratado$moransig <- moran[,5] #p valor
# data_gravidez_tratado$lag_pibpc <- lag.listw(ccListw, data_gravidez_tratado$numeros_absolutos)
#   data_gravidez_tratado <- data_gravidez_tratado %>%
#   mutate(quadrante = case_when(numeros_absolutos >= 0 & lag_pibpc >= 0 & moransig < 0.05~"Alto-Alto",
#                                numeros_absolutos <= 0 & lag_pibpc <= 0 & moransig < 0.05~"Baixo-Baixo",
#                                numeros_absolutos >= 0 & lag_pibpc <= 0 & moransig < 0.05~"Alto-Baixo",
#                                numeros_absolutos <= 0 & lag_pibpc >= 0 & moransig < 0.05~"Baixo-Alto"))
# 
# 
# ggplot(data_gravidez_tratado) +
#   geom_point(aes(x = numeros_absolutos, y =lag_pibpc)) +
#   theme_minimal() +
#   xlab("Pib per capita") +
#   ylab("Pib per capita medio dos vizinhos")
# 
# 
# ggplot(data_gravidez_tratado) +
#   geom_sf(aes(fill = quadrante), color=NA) +
#   scale_fill_manual(values = c("blue", "red"), na.value = "lightgrey") +
#   theme_minimal()


set.seed(987654)
n <- length(Sy0_nb)
uncorr_x <- rnorm(n)
rho <- 0.5
autocorr_x <- invIrW(Sy0_lw_W, rho) %*% uncorr_x
