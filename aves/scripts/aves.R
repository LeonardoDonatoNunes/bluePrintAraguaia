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


aves_aquativas <-
  dados_sf %>%
  dplyr::filter(species_searched %in% aves_aquaticas$species_se)


# Calcula a riqueza de espécies
intersects <- st_intersects(aves_aquativas, bacias)
num_species <- apply(intersects, 2, function(x) n_distinct(aves_aquativas$species_searched[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('aves/shp/{nome}_riqueza_nativas.shp'), delete_layer = TRUE)


# Calcula a riqueza de espécies
intersects <- st_intersects(aves_aquativas, bacias)
num_species <- apply(intersects, 2, function(x) length(aves_aquativas$species_searched[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('aves/shp/{nome}_ocorrencias_nativas.shp'), delete_layer = TRUE)
sf::write_sf(aves_aquativas, glue::glue('aves/shp/{nome}_ocorrencias_nativas_pontos.shp'), delete_layer = TRUE)

ameacas <- c('CR', 'EN', 'VU')

aves_aquativas_amea <-
  dados_sf %>%
  dplyr::filter(species_searched %in% aves_aquaticas$species_se) %>%
  dplyr::mutate(
    categoria_mma  = if_else(categoria_mma  %in% ameacas, 1, NA_integer_),
    categoria_iucn = if_else(categoria_iucn %in% ameacas, 1, NA_integer_),
    categoria_iucn    = coalesce(categoria_mma, categoria_iucn)
    )
