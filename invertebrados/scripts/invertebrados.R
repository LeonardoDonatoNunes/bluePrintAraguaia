library(sf)
library(tidyverse)

bacias <- read_sf('invertebrados/shpGeral/hidrografia_selected/bacias_ainhadas.shp')
bacias <- bacias[,1]

arquivos <- list(
  bivalvia = 'invertebrados/ocorrencias/Validado_Bivalvia_consolidado_05abr23.xlsx',
  gastropoda = 'invertebrados/ocorrencias/Validado_Gastropoda_consolidado_05abr23.xlsx',
  decapoda = 'invertebrados/ocorrencias/Validado_Decapoda_consolidado_05abr23.xlsx'
)

# Cria camada de riqueza das espécies nativas
for (i in seq_along(arquivos)) {

  nome <- names(arquivos)[i]
  dados <- openxlsx::read.xlsx(arquivos[[i]])

  names(dados) <- stringr::str_to_lower(names(dados))
  colunas <- names(dados) %>% table
  colunas <- names(colunas[colunas > 1])
  dup_index <- purrr::map_dbl(colunas, ~max(which(names(dados) == .x)))
  dados <- dados[, -dup_index]

  dados_sf <-
    dados %>%
    dplyr::mutate(
      deletar = stringr::str_to_lower(deletar),
      deletar = dplyr::if_else(is.na(deletar), 'nao', deletar)) %>%
    dplyr::filter(deletar == 'nao') %>%
    dplyr::mutate(
      longitude = decimallongitude,
      latitude = decimallatitude) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

  intersects <- st_intersects(dados_sf, bacias)
  num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species[x]))
  bacias$numEspecies <- num_species

  sf::write_sf(bacias, glue::glue('invertebrados/shp/{nome}_riqueza_nativas.shp'))

}

# Cria camadas das ocorrências das espécies nativas
for (i in seq_along(arquivos)) {

  nome <- names(arquivos)[i]
  dados <- openxlsx::read.xlsx(arquivos[[i]])

  names(dados) <- stringr::str_to_lower(names(dados))
  colunas <- names(dados) %>% table
  colunas <- names(colunas[colunas > 1])
  dup_index <- purrr::map_dbl(colunas, ~max(which(names(dados) == .x)))
  dados <- dados[, -dup_index]

  dados_sf <-
    dados %>%
    dplyr::mutate(
      deletar = stringr::str_to_lower(deletar),
      deletar = dplyr::if_else(is.na(deletar), 'nao', deletar)) %>%
    dplyr::filter(deletar == 'nao') %>%
    dplyr::mutate(
      longitude = decimallongitude,
      latitude = decimallatitude) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

  intersects <- st_intersects(dados_sf, bacias)
  num_species <- apply(intersects, 2, function(x) length(dados_sf$species[x]))
  bacias$numEspecies <- num_species

  sf::write_sf(dados_sf, glue::glue('invertebrados/shp/{nome}_ocorrencia_nativas_pontos.shp'))
  sf::write_sf(bacias, glue::glue('invertebrados/shp/{nome}_ocorrencia_nativas.shp'))

}

# Cria camadas das ocorrências das espécies não nativas
for (i in seq_along(arquivos)) {

  nome <- names(arquivos)[i]
  dados <- openxlsx::read.xlsx(arquivos[[i]])

  names(dados) <- stringr::str_to_lower(names(dados))

  if (!'origem' %in% names(dados)) {next}

  colunas <- names(dados) %>% table
  colunas <- names(colunas[colunas > 1])
  dup_index <- purrr::map_dbl(colunas, ~max(which(names(dados) == .x)))
  dados <- dados[, -dup_index]

  dados_sf <-
    dados %>%
    dplyr::filter(origem == 'invasor') %>%
    dplyr::mutate(
      longitude = decimallongitude,
      latitude = decimallatitude) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

  if (nrow(dados_sf) == 0) {next}

  intersects <- st_intersects(dados_sf, bacias)
  num_species <- apply(intersects, 2, function(x) length(dados_sf$species[x]))
  bacias$numEspecies <- num_species

  sf::write_sf(dados_sf, glue::glue('invertebrados/shp/{nome}_ocorrencia_invasor_pontos.shp'))
  sf::write_sf(bacias, glue::glue('invertebrados/shp/{nome}_ocorrencia_invasor.shp'))

}
