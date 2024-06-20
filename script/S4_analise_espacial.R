options(scipen = 999)
# análise espacial

library(sf)
library(spdep)
library(RColorBrewer)

source(file = "script/S3_matriz_vizinhaca.R")
# usar taxa bruta

# índice de moran ---------------------------------------------------------

inc.lag <- spdep::lag.listw(ccListw, data_gravidez_tratado$numeros_absolutos)
inc.lag

plot(inc.lag ~ data_gravidez_tratado$numeros_absolutos, pch=16, asp=1)
abline(lm(inc.lag ~ data_gravidez_tratado$numeros_absolutos), col="blue")

#  estatistica

I <- moran(data_gravidez_tratado$numeros_absolutos, ccListw, length(ccNb), Szero(ccListw))[1]
I
test_I <- moran.test(data_gravidez_tratado$numeros_absolutos, ccListw, alternative="greater", zero.policy=TRUE) 
test_I
test_I$p.value

moran <- localmoran(data_gravidez_tratado$numeros_absolutos, ccListw)



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
