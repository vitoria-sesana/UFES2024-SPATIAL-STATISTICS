# Modelagem em dados de área

library(spdep)
library(spatialreg)
# install.packages("INLA")
# library(INLA)

# https://www.paulamoraga.com/book-geospatial/sec-arealdatatheory.html
# https://journal.r-project.org/archive/2018/RJ-2018-036/RJ-2018-036.pdf

attach(base)
m1_idf <- lagsarlm(log(casos) ~ d1 + d2 + d3 + d4 + d5 + d6, data = base, listw = ccPesos_vizinhos, zero.policy = TRUE)
m2_ideb <- lagsarlm(log(casos) ~ ideb_f1 + ideb_f2 + ideb_em, data = base, listw = ccPesos_vizinhos, zero.policy = TRUE)
m3_all <- lagsarlm(casos ~ d1 + d2 + d3 + d4 + d5 + d6 + ideb_f1 + ideb_f2 +ideb_em, data = base, listw = ccPesos_vizinhos, zero.policy = TRUE)

## 
m4 <- spatialreg::lagsarlm(casos ~ idf + ideb_f1 + ideb_f2 +ideb_em,
               data = base,
               listw = ccPesos_vizinhos,
               method = "Matrix")
m4
summary(m4)

## 
err90 <- spatialreg::errorsarlm(casos ~ idf + ideb_f1 + ideb_f2 +ideb_em,
                    data = base,
                    listw = ccPesos_vizinhos)
summary(err90)

## 
lag90durbin <- spatialreg::lagsarlm(casos ~ idf + ideb_f1 + ideb_f2 +ideb_em, 
                        data = base, 
                        listw = ccPesos_vizinhos,
                        type="mixed")
summary(lag90durbin)

## 
m4s <- spatialreg::sacsarlm(casos ~ idf + ideb_f1 + ideb_f2 +ideb_em, 
                           data = base, 
                           listw = ccPesos_vizinhos)



AIC(m4, err90, lag90durbin, m4s)




# testes de residuos ------------------------------------------------------
# Derive the residuals from the regression. Need to handle those missed values
slResiduals <- rep(0, length(base$casos))
resIndex <- m3_all$residuals %>% names() %>% as.integer();
slResiduals[resIndex] <- m3_all$residuals


x <- ccPesos_vizinhos %>% spdep::moran.test(slResiduals, ., zero.policy = TRUE) 
x$p.value

m3_all
