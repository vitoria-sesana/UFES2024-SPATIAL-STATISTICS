
model <- lagsarlm(casos ~ d1 + d2 + d3 + d4 + d5 + d6 + ideb_f1 + ideb_f2 +ideb_em, data = base, listw = ccPesos_vizinhos, zero.policy = TRUE)
