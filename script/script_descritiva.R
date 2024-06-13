library(geobr)
library(dplyr)

df_gravidez <- readxl::read_xlsx("bases/caderno-09-gravidez-adolescencia.xlsx", 
                                 sheet = "Anexo 02") %>% 
  slice(2:n()) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names() %>% 
  rename(name_muni = municipios)

df_cd_ibge <- readxl::read_xlsx("bases/Município em Unidade da Federação - Espírito Santo.xlsx", col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  mutate(x2 = gsub(" \\(|ES\\)", "", x2))

df_geobr <- geobr::read_municipality(code_muni = "ES") 

left_join(df_gravidez, df_geobr, by = "name_muni")




ggplot() +
  geom_sf(data=dataset_final, aes(fill=IDHM), color= NA, size=.15)+
  labs(title="IDHM 2013 (ano base 2010) dos Municipíos de MS",
       caption='Fonte: Elaboração própria', size=8)+
  scale_fill_distiller(palette = "Greens", limits=c(0.5, 0.8),
                       name="Code_muni")+
  theme_minimal()

