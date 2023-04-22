library(sf)
library(tidyverse)
if (!dir.exists('anfibios/shp')) {dir.create('anfibios/shp')}
nome = 'anfibios'

# Carrega os a shapefile com os pligonos das bacias aninhadas
bacias <- sf::read_sf('shpGeral/hidrografia_selected/bacias_ainhadas.shp')
bacias <- bacias[,1] # Mantém somente a primeira coluna que tem o ID de cada polígono
limite_bacia <- sf::read_sf('shpGeral/limite_bacia_oficial/limite_bacia_oficiala.shp')

# Carrega os dados de anfíbios
anfibios = read.csv('anfibios/ocorrencias/occ_integradas_anfibios_limpas_filtradas.csv')

dados_sf <-
  anfibios %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_searched[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('anfibios/shp/{nome}_riqueza_nativas.shp'), delete_layer = TRUE)


# Calcula a ocorrência de anfíbios
ocorrencias <- apply(intersects, 2, function(x) length(dados_sf$species[x]))
bacias$numEspecies <- ocorrencias
sf::write_sf(bacias, glue::glue('anfibios/shp/{nome}_ocorrencias_nativas.shp'), delete_layer = TRUE)

ocorrencias_pontos <- sf::st_intersection(dados_sf, limite_bacia)
sf::write_sf(ocorrencias_pontos, glue::glue('anfibios/shp/{nome}_ocorrencias_nativas_pontos.shp'), delete_layer = TRUE)


# Riqueza de espécies ameaçadas

    #      Extinta (EX) – Extinct
    #      Extinta na Natureza (EW) – Extinct in the Wild
    #      Regionalmente Extinta (RE) – Regionally Extinct
    #      Criticamente em Perigo (CR) – Critically
    #      Endangered Em Perigo (EN) – Endangered
    #      Vulnerável (VU) – Vulnerable
    #      Quase Ameaçada (NT) – Near Threatened
    #      Menos Preocupante (LC) – Least Concern
    #      Dados Insuficientes (DD) – Data Deficient
    #      Não Aplicável (NA) – Not Applicable
    #      Não Avaliada (NE) – Not Evaluated

dados_sf_ameacadas <-
  dados_sf %>%
  dplyr::filter(categoria_iucn %in% c('CR', 'EN', 'VU'))

intersects <- st_intersects(dados_sf_ameacadas, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf_ameacadas$species_searched[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('anfibios/shp/{nome}_riqueza_ameacadas.shp'), delete_layer = TRUE)
