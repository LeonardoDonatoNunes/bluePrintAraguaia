library(sf)
library(tidyverse)
if (!dir.exists('mamiferos/shp')) {dir.create('mamiferos/shp')}
nome = 'mamiferos'

# Carrega os a shapefile com os pligonos das bacias aninhadas
bacias <- sf::read_sf('shpGeral/hidrografia_selected/bacias_ainhadas.shp')
bacias <- bacias[,1] # Mantém somente a primeira coluna que tem o ID de cada polígono
limite_bacia <- sf::read_sf('shpGeral/limite_bacia_oficial/limite_bacia_oficiala.shp')

# Carrega os dados de anfíbios
mamiferos = openxlsx::read.xlsx('mamiferos/ocorrencias/Mamiferos_Araguaia.xlsx')
atrr <- openxlsx::read.xlsx('mamiferos/ocorrencias/Mamiferos_Araguaia.xlsx', sheet = 2)

atrr$Ambiente %>% unique()


# Mamíferos aquáticos
dados_sf <-
  mamiferos %>%
  dplyr::filter(species_se %in% unique(atrr[atrr$Ambiente == 'aquático', ]$species_se)) %>%
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('mamiferos/shp/{nome}_riqueza_aquaticas.shp'), delete_layer = TRUE)

num_species <- apply(intersects, 2, function(x) length(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('mamiferos/shp/{nome}_ocorrencia_aquaticas.shp'), delete_layer = TRUE)
sf::write_sf(dados_sf, glue::glue('mamiferos/shp/{nome}_ocorrencia_aquaticas_pontos.shp'), delete_layer = TRUE)


# Mamíferos semi-aquáticos
dados_sf <-
  mamiferos %>%
  dplyr::filter(species_se %in% unique(atrr[atrr$Ambiente == 'semi-aquático', ]$species_se)) %>%
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('mamiferos/shp/{nome}_riqueza_semi_aquaticas.shp'), delete_layer = TRUE)

num_species <- apply(intersects, 2, function(x) length(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('mamiferos/shp/{nome}_ocorrencia_semi_aquaticas.shp'), delete_layer = TRUE)
sf::write_sf(dados_sf, glue::glue('mamiferos/shp/{nome}_ocorrencia_semi_aquaticas_pontos.shp'), delete_layer = TRUE)


# Mamíferos aquáticos e semi-aquáticos
dados_sf <-
  mamiferos %>%
  dplyr::filter(species_se %in% unique(atrr[atrr$Ambiente %in% c('aquático', 'semi-aquático'), ]$species_se)) %>%
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('mamiferos/shp/{nome}_riqueza_aquaticas_e_semi.shp'), delete_layer = TRUE)

num_species <- apply(intersects, 2, function(x) length(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('mamiferos/shp/{nome}_ocorrencia_aquaticas_e_semi.shp'), delete_layer = TRUE)
sf::write_sf(dados_sf, glue::glue('mamiferos/shp/{nome}_ocorrencia_aquaticas_e_semi_pontos.shp'), delete_layer = TRUE)
