# análise espacial

library(sf)
library(spdep)
library(RColorBrewer)

source(file = "script/S3_matriz_vizinhaca.R")
# usar taxa bruta

# índice de moran ---------------------------------------------------------

inc.lag <- lag.listw(ccListw)
inc.lag

plot(inc.lag ~ data_gravidez_tratado$numeros_absolutos, pch=16, asp=1)
abline(lm(inc.lag ~ data_gravidez_tratado$numeros_absolutos), col="blue")

#  estatistica

I <- moran(data_gravidez_tratado$numeros_absolutos, ccListw, length(ccNb), Szero(ccListw))[1]
I
moran.test(data_gravidez_tratado$numeros_absolutos, ccListw, alternative="greater", zero.policy=TRUE) 
