library(sf)
library(tidyverse)
if (!dir.exists('repteis/shp')) {dir.create('repteis/shp')}
nome = 'repteis'

# Carrega os a shapefile com os pligonos das bacias aninhadas
bacias <- sf::read_sf('shpGeral/hidrografia_selected/bacias_ainhadas.shp')
bacias <- bacias[,1] # Mantém somente a primeira coluna que tem o ID de cada polígono
limite_bacia <- sf::read_sf('shpGeral/limite_bacia_oficial/limite_bacia_oficiala.shp')

# Carrega os dados de anfíbios
repteis = openxlsx::read.xlsx('repteis/ocorrencias/Repteis_Araguaia.xlsx')
repteis_classificacao <- openxlsx::read.xlsx('repteis/ocorrencias/Repteis_Araguaia.xlsx', sheet = 'ExclusãoDuplicadas')

repteis_aquaticos <-
  repteis %>%
  filter(species_se %in% repteis_classificacao[repteis_classificacao$Aquática == 'Sim', ]$species_se)

repteis_semi_aquaticos <-
  repteis %>%
  filter(species_se %in% repteis_classificacao[repteis_classificacao$SemiAquática == 'Sim', ]$species_se)



# Répteis aquáticos
dados_sf <-
  repteis_aquaticos %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_riqueza_aquaticas.shp'), delete_layer = TRUE)

num_species <- apply(intersects, 2, function(x) length(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_ocorrencia_aquaticas.shp'), delete_layer = TRUE)
sf::write_sf(dados_sf, glue::glue('repteis/shp/{nome}_ocorrencia_aquaticas_pontos.shp'), delete_layer = TRUE)



# Répteis repteis_semi_aquaticos
dados_sf <-
  repteis_semi_aquaticos %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_riqueza_semi_aquaticas.shp'), delete_layer = TRUE)

num_species <- apply(intersects, 2, function(x) length(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_ocorrencia_semi_aquaticas.shp'), delete_layer = TRUE)
sf::write_sf(dados_sf, glue::glue('repteis/shp/{nome}_ocorrencia_semi_aquaticas_pontos.shp'), delete_layer = TRUE)
