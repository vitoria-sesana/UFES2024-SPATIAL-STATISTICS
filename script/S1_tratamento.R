library(geobr)
library(sf)
library(dplyr)
library(ggplot2)


data_gravidez <- readxl::read_xlsx("bases/caderno-09-gravidez-adolescencia.xlsx", 
                                 sheet = "Anexo 02") %>% 
  slice(2:n()) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names() %>% 
  rename(name_muni = municipios) %>% 
  mutate(percent = as.numeric(percent),
         numeros_absolutos = as.numeric(numeros_absolutos),
         name_muni = stringr::str_replace(name_muni,
                                          pattern = "Atílio Vivacqua", 
                                          replacement = "Atílio Vivácqua"))

data_ibge <- readxl::read_xlsx("bases/Município em Unidade da Federação - Espírito Santo.xlsx", col_names = FALSE) %>%
  janitor::clean_names() %>%
  rename(CD_MUN = x1,
         name_muni = x2) %>%
  mutate(CD_MUN = as.numeric(CD_MUN),
         name_muni = gsub(" \\(|ES\\)",
                          "",
                          name_muni))


data_shape <- sf::st_read('bases/ES_Municipios_2022/ES_Municipios_2022.shp') %>% 
  mutate(CD_MUN = as.integer(CD_MUN))

data_gravidez_tratado <- 
  left_join(data_gravidez, data_ibge, by = "name_muni") %>% 
  left_join(data_shape, by = "CD_MUN") %>% 
  sf::st_as_sf()




# saidas ------------------------------------------------------------------

data_gravidez_tratado
