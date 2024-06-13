install.packages("basedosdados")
library("basedosdados")

# Defina o seu projeto no Google Cloud
set_billing_id("<YOUR_PROJECT_ID>")

# Para carregar o dado direto no R
query <- bdplyr("br_bd_vizinhanca.municipio")
df <- bd_collect(query)


# spdep
# geobr 

# espaço não necessariamente é geografico, pode ser com base na idade 
# correlação entre pontos que estão próximos ou distantes


