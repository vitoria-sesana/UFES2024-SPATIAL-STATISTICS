# análises descritivas iniciais


rm(list = ls())

# biblioteca --------------------------------------------------------------


# coletando bases ---------------------------------------------------------
source("script/S1_tratamento.R")


# variável resposta -------------------------------------------------------

# percentual
ggplot() +
  geom_sf(data = data_gravidez_tratado, aes(fill = percent), color=NA) +
  labs(title = "") +
  scale_fill_distiller(palette = "Greens", direction = 1, 
                       name='Percentual de\n Adolescentes Gestantes', 
                       labels = scales::percent) +
  scale_x_continuous(limits = c(-42, -39)) +
  theme_void()

# absoluto
ggplot() +
  geom_sf(data = data_gravidez_tratado, aes(fill = numeros_absolutos), color=NA) +
  labs(title = "") +
  scale_fill_distiller(palette = "Greens", direction = 1, 
                       name='Percentual de\n Adolescentes Gestantes', 
                       labels = scales::percent) +
  scale_x_continuous(limits = c(-42, -39)) +
  theme_void()
