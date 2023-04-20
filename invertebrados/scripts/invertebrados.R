library(sf)
library(tidyverse)
source('utils.R', encoding = 'utf-8')
destinatario <- 'taisemlopes@gmail.com'
bacias <- read_sf('Dados/hidrografia_selected/bacias_ainhadas.shp')

arquivos <- list(
  bivalvia = 'Dados/Invertebrados/bivalvia/Validado_Bivalvia_consolidado_05abr23.xlsx',
  gastropoda = 'Dados/Invertebrados/gastropoda/Validado_Gastropoda_consolidado_05abr23.xlsx',
  decapoda = 'Dados/Invertebrados/arthropoda/Validado_Decapoda_consolidado_05abr23.xlsx'
)

dados <- openxlsx::read.xlsx(arquivos$gastropoda)

names(dados) <- stringr::str_to_lower(names(dados))
colunas <- names(dados) %>% table
colunas <- names(colunas[colunas > 1])
dup_index <- purrr::map_dbl(colunas, ~max(which(names(dados) == .x)))
dados <- dados[, -dup_index]


dados_sf <-
  dados %>%
  dplyr::mutate(deletar = stringr::str_to_lower(deletar)) %>%
  dplyr::filter(deletar != 'Sim') %>%
  dplyr::mutate(
    longitude = decimallongitude,
    latitude = decimallatitude) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))


sf::write_sf(dados_sf, 'Dados/Invertebrados/arthropoda/gastropoda_araguaia.shp')


enviar_email(destinatario, "Mapas Invertebrados 🗺🐞️",
             anexo = c('mapasProntos/mapa_bibalvia.png', 'mapasProntos/mapa_decapoda.png', 'mapasProntos/mapa_gastropoda.png'),
             corpo = "Mor, segue os mapas dos invertebrados (Validados) 😘")

