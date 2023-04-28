library(sf)
library(tidyverse)
if (!dir.exists('aves/shp')) {dir.create('aves/shp')}
nome = 'aves'

# Carrega os a shapefile com os pligonos das bacias aninhadas
bacias <- sf::read_sf('shpGeral/hidrografia_selected/bacias_ainhadas.shp')
bacias <- bacias[,1] # Mantém somente a primeira coluna que tem o ID de cada polígono
limite_bacia <- sf::read_sf('shpGeral/limite_bacia_oficial/limite_bacia_oficiala.shp')

ocorrencias_or <- read.csv('aves/ocorrencias/occ_integradas_aves_limpas_filtradas.csv')
aves_aquaticas <- openxlsx::read.xlsx('aves/ocorrencias/Aves_aquaticas_araguaia.xlsx')

dados_sf <-
  ocorrencias_or %>%
  dplyr::distinct(species_searched, latitude, longitude, .keep_all = TRUE) %>%
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias)) %>%
  sf::st_intersection(limite_bacia)

sp_semi_aquaticas <- unique(aves_aquaticas[aves_aquaticas$tipo == 'semi', ]$species_se)
sp_aquaticas <- unique(aves_aquaticas[aves_aquaticas$tipo == 'aquatica', ]$species_se)

aves_geral <-
  dados_sf %>%
  dplyr::filter(species_searched %in% aves_aquaticas$species_se) %>%
  dplyr::left_join(aves_aquaticas %>%  dplyr::mutate(tipo = dplyr::if_else(tipo == 'semi', 'semi-aquático', 'aquático')) %>% dplyr::rename(species_searched = species_se, ambiente = tipo))

aves_geral %>%
  dplyr::mutate(geometry = as.character(geometry)) %>%
  openxlsx::write.xlsx(., 'aves/ocorrencias/aves_ocorrencias_exportado.xlsx', overwrite = TRUE)


# Calcula a riqueza de espécies
intersects <- st_intersects(aves_geral, bacias)
num_species <- apply(intersects, 2, function(x) n_distinct(aves_geral$species_searched[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('aves/shp/{nome}_riqueza_nativas.shp'), delete_layer = TRUE)
sf::write_sf(aves_geral, glue::glue('aves/shp/{nome}_ocorrencias_nativas_pontos.shp'), delete_layer = TRUE)

num_species <- apply(intersects, 2, function(x) length(aves_geral$species_searched[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('aves/shp/{nome}_ocorrencias_nativas.shp'), delete_layer = TRUE)

# Calcula a riqueza de espécies aquaticas
aquaticas <-
  dados_sf %>%
  dplyr::filter(species_searched %in% sp_aquaticas)

sf::write_sf(aquaticas, glue::glue('aves/shp/{nome}_ocorrencias_aquaticas_pontos.shp'), delete_layer = TRUE)

semi_aquaticas <-
  dados_sf %>%
  dplyr::filter(species_searched %in% sp_semi_aquaticas)

sf::write_sf(semi_aquaticas, glue::glue('aves/shp/{nome}_ocorrencias_semi_aquaticas_pontos.shp'), delete_layer = TRUE)


ameacas <- c('CR', 'EN', 'VU')

aves_geral_amea <-
  dados_sf %>%
  dplyr::filter(species_searched %in% aves_aquaticas$species_se) %>%
  dplyr::mutate(
    categoria_mma  = if_else(categoria_mma  %in% ameacas, 1, NA_integer_),
    categoria_iucn = if_else(categoria_iucn %in% ameacas, 1, NA_integer_),
    ameacadas  = coalesce(categoria_mma, categoria_iucn)
    ) %>%
  dplyr::filter(ameacadas == 1)


# Calcula a riqueza de espécies ameacadas
intersects <- st_intersects(aves_geral_amea, bacias)
num_species <- apply(intersects, 2, function(x) n_distinct(aves_geral_amea$species_searched[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('aves/shp/{nome}_riqueza_nativas_amecadas.shp'), delete_layer = TRUE)
