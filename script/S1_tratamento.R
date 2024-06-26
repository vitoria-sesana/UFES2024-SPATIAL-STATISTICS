library(sf)
library(dplyr)
library(ggplot2)


# variável resposta -------------------------------------------------------
# base_gravidez <- readxl::read_xlsx("bases/caderno-09-gravidez-adolescencia.xlsx", 
#                                  sheet = "Anexo 02") %>% 
#   slice(2:n()) %>% 
#   janitor::row_to_names(row_number = 1) %>% 
#   janitor::clean_names() %>% 
#   rename(name_muni = municipios) %>% 
#   mutate(percent = as.numeric(percent),
#          numeros_absolutos = as.numeric(numeros_absolutos),
#          name_muni = stringr::str_replace(name_muni,
#                                           pattern = "Atílio Vivacqua", 
#                                           replacement = "Atílio Vivácqua"))

base_gravidez <- read.table("bases/tabnet_gravidez_2022.txt", sep = ";") %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>%  
  rename(casos = nascim_p_resid_mae) %>% 
  mutate(cd = as.numeric(stringr::str_sub(municipio, 1, 7)),
         casos = as.numeric(casos) 
         ) %>% 
  select(cd, casos)

# base_ibge <- readxl::read_xlsx("bases/Município em Unidade da Federação - Espírito Santo.xlsx", col_names = FALSE) %>%
#   janitor::clean_names() %>%
#   rename(CD_MUN = x1,
#          name_muni = x2) %>%
#   mutate(CD_MUN = as.numeric(CD_MUN),
#          name_muni = gsub(" \\(|ES\\)",
#                           "",
#                           name_muni))


base_shape <- sf::st_read('bases/malha_municipal_ES_2022/ES_Municipios_2022.shp') %>% 
  mutate(cd = as.integer(stringr::str_sub(CD_MUN, 1, 6)))

# IDEB covariável ---------------------------------------------------------

base_ideb_f1 <- readxl::read_xlsx("bases/ideb/divulgacao_anos_iniciais_municipios_2021.xlsx") %>%  
  slice(9:n()) %>%  
  janitor::row_to_names(1) %>%  
  janitor::clean_names() %>% 
  filter(sg_uf == "ES") %>% 
  filter(vl_observado_2021 != "-") %>% 
  mutate(vl_observado_2021 = as.numeric(vl_observado_2021)) %>% 
  group_by(co_municipio) %>% 
  summarise(ideb_f1 = mean(vl_observado_2021)) %>% 
  rename(CD_MUN = co_municipio)

base_ideb_f2 <- readxl::read_xlsx("bases/ideb/divulgacao_anos_finais_municipios_2021.xlsx")  %>%  
  slice(9:n()) %>%  
  janitor::row_to_names(1) %>%  
  janitor::clean_names() %>% 
  filter(sg_uf == "ES") %>% 
  filter(vl_observado_2021 != "-") %>% 
  mutate(vl_observado_2021 = as.numeric(vl_observado_2021)) %>% 
  group_by(co_municipio) %>% 
  summarise(ideb_f2 = mean(vl_observado_2021)) %>% 
  rename(CD_MUN = co_municipio)

base_ideb_em <- readxl::read_xlsx("bases/ideb/divulgacao_ensino_medio_municipios_2021.xlsx")  %>%  
  slice(9:n()) %>%  
  janitor::row_to_names(1)  %>%  
  janitor::clean_names() %>% 
  filter(sg_uf == "ES") %>% 
  filter(vl_observado_2021 != "-") %>% 
  mutate(vl_observado_2021 = as.numeric(vl_observado_2021)) %>% 
  group_by(co_municipio) %>% 
  summarise(ideb_em = mean(vl_observado_2021)) %>% 
  rename(CD_MUN = co_municipio)

# idf ---------------------------------------------------------------------

base_idf <- read.csv("bases/idf_municipio_es_DEZ2022.xlsx - base_de_dados.csv") %>% 
  # filter(COD_idf == "idf") %>% 
  filter(COD_idf %in% c("d1", "d2", "d3", "d4", "d5", "d6")) %>% 
  mutate(indice = as.numeric(stringr::str_replace(indice, ",", ".")),
         cod_ibge = as.character(cod_ibge)
         ) %>% 
  rename(CD_MUN = cod_ibge) %>% 
  select(CD_MUN, COD_idf, indice)

base_idf <- base_idf %>%
  tidyr::pivot_wider(names_from = COD_idf, values_from = indice)

base_idf

# saidas ------------------------------------------------------------------

base <- base_shape %>% 
  left_join(base_gravidez, by = "cd") %>% 
  left_join(base_ideb_f1, by = "CD_MUN") %>% 
  left_join(base_ideb_f2, by = "CD_MUN") %>% 
  left_join(base_ideb_em, by = "CD_MUN") %>% 
  left_join(base_idf, by = "CD_MUN") %>% 
  mutate(ideb_f2 = data.table::fifelse(is.na(ideb_f2), 0, ideb_f2),
         ideb_em = data.table::fifelse(is.na(ideb_em), 0, ideb_em)) %>% 
  as.data.frame() %>% 
  select(-cd, -geometry, -AREA_KM2, -SIGLA_UF, -NM_MUN)


# saída -------------------------------------------------------------------

write.table(base, file = "bases/base.txt", sep = ";")  


  