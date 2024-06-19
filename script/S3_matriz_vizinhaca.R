# bibliotecas -------------------------------------------------------------
library(spdep)
source("script/S1_tratamento.R")

# data polígono -----------------------------------------------------------
ccShape = data_gravidez_tratado

# Listas de vizinhança ----------------------------------------------------
# lista os municípios vizinhos que cada município possui 
# a partir de uma lista de polígonos
ccNb=poly2nb(ccShape) 

#  Lista dos pesos da vizinhança -------------------------------------------
# determinando os pesos de cada município a partir da lista de vizinhaça
# style escolhe o tipo de peso de cada vizinho para aquele município 
ccListw=nb2listw(ccNb,style='B') 


# Matriz de vizinhaça -----------------------------------------------------
ccMat=nb2mat(ccNb,style='B') 


# Soma dos pesos da matriz de vizinhaça -----------------------------------
ccNumNeighbors=rowSums(ccMat)
