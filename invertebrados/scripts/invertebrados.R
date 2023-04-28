library(sf)
library(tidyverse)

bacias <- read_sf('shpGeral/hidrografia_selected/bacias_ainhadas.shp')
bacias <- bacias[,1]
limite_bacia <- sf::read_sf('shpGeral/limite_bacia_oficial/limite_bacia_oficiala.shp')

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
    st_as_sf(coords = c('longitude', 'latitude'), crs = sf::st_crs(bacias))

  intersects <- st_intersects(dados_sf, bacias)
  num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species[x]))
  bacias$numEspecies <- num_species

  sf::write_sf(bacias, glue::glue('invertebrados/shp/{nome}_riqueza_nativas.shp'), delete_layer = TRUE)

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

  ocorrencias_pontos <- sf::st_intersection(dados_sf, limite_bacia)
  sf::write_sf(ocorrencias_pontos, glue::glue('invertebrados/shp/{nome}_ocorrencia_nativas_pontos.shp'), delete_layer = TRUE)
  sf::write_sf(bacias, glue::glue('invertebrados/shp/{nome}_ocorrencia_nativas.shp'), delete_layer = TRUE)

  ocorrencias_pontos %>%
    dplyr::mutate(geometry = as.character(geometry)) %>%
    openxlsx::write.xlsx(., glue::glue('invertebrados/ocorrencias/{nome}_ocorrencias_nativas_exportado.xlsx'), overwrite = TRUE)


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
  ocorrencias_pontos <- sf::st_intersection(dados_sf, limite_bacia)

  sf::write_sf(ocorrencias_pontos, glue::glue('invertebrados/shp/{nome}_ocorrencia_invasor_pontos.shp'), delete_layer = TRUE)
  sf::write_sf(bacias, glue::glue('invertebrados/shp/{nome}_ocorrencia_invasor.shp'), delete_layer = TRUE)

  ocorrencias_pontos %>%
    dplyr::mutate(geometry = as.character(geometry)) %>%
    openxlsx::write.xlsx(., glue::glue('invertebrados/ocorrencias/{nome}_ocorrencias_nao_nativas_exportado.xlsx'), overwrite = TRUE)


}



# Bivalvia - ameaçadas --------------------------------------------------
dados <- openxlsx::read.xlsx(arquivos$bivalvia)
names(dados) <- stringr::str_to_lower(names(dados))
colunas <- names(dados) %>% table
colunas <- names(colunas[colunas > 1])
dup_index <- purrr::map_dbl(colunas, ~max(which(names(dados) == .x)))
dados <- dados[, -dup_index]

dados_sf <-
  dados %>%
  dplyr::filter(!is.na(categoria_iucn)) %>%
  dplyr::mutate(
    longitude = decimallongitude,
    latitude = decimallatitude) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))


intersects <- st_intersects(dados_sf, bacias)
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species[x]))
bacias$numEspecies <- num_species

sf::write_sf(bacias, glue::glue('invertebrados/shp/bivalvia_riqueza_ameacadas.shp'), delete_layer = TRUE)


# Decapoda - ameaçadas --------------------------------------------------
dados <- openxlsx::read.xlsx(arquivos$decapoda)
names(dados) <- stringr::str_to_lower(names(dados))
colunas <- names(dados) %>% table
colunas <- names(colunas[colunas > 1])
dup_index <- purrr::map_dbl(colunas, ~max(which(names(dados) == .x)))
dados <- dados[, -dup_index]

dados_sf <-
  dados %>%
  dplyr::filter(!is.na(categoria_iucn)) %>%
  dplyr::mutate(
    longitude = decimallongitude,
    latitude = decimallatitude) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))


intersects <- st_intersects(dados_sf, bacias)
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species[x]))
bacias$numEspecies <- num_species

sf::write_sf(bacias, glue::glue('invertebrados/shp/decapoda_riqueza_ameacadas.shp'), delete_layer = TRUE)
