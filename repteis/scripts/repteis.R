library(sf)
library(tidyverse)
if (!dir.exists('repteis/shp')) {dir.create('repteis/shp')}
nome = 'repteis'

# Carrega os a shapefile com os pligonos das bacias aninhadas
bacias <- sf::read_sf('shpGeral/hidrografia_selected/bacias_ainhadas.shp')
bacias <- bacias[,1] # Mantém somente a primeira coluna que tem o ID de cada polígono
limite_bacia <- sf::read_sf('shpGeral/limite_bacia_oficial/limite_bacia_oficiala.shp')

# Carrega os dados de anfíbios
repteis_or = openxlsx::read.xlsx('repteis/ocorrencias/Repteis_Araguaia_V01.xlsx')
repteis_classificacao <- openxlsx::read.xlsx('repteis/ocorrencias/Repteis_Araguaia_V01.xlsx', sheet = 'ClassificaçãoFinal')

especies_aquaticas <- unique(repteis_classificacao[repteis_classificacao$Aquática == 'Sim', ]$species_se)
especies_semi_aquaticas <- unique(repteis_classificacao[repteis_classificacao$SemiAquática == 'Sim', ]$species_se)

ambientes <-
  expand.grid(
    species_se = especies_aquaticas,
    ambiente = 'aquático'
  ) %>% bind_rows(
    expand.grid(
      species_se = especies_semi_aquaticas,
      ambiente = 'semi-aquático'
    )
  )




repteis <-
  repteis_or %>%
  dplyr::filter(!species_se %in% c("Eretmochelys imbricata", "Lepidochelys olivacea"))

repteis_aquaticos <-
  repteis %>%
  filter(species_se %in% especies_aquaticas)

repteis_semi_aquaticos <-
  repteis %>%
  filter(species_se %in% especies_semi_aquaticas)


# Répteis aquáticos
dados_sf <-
  repteis_aquaticos %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)

# Calcula a riqueza de espécies
num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species
sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_riqueza_aquaticas.shp'), delete_layer = TRUE)
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
sf::write_sf(dados_sf, glue::glue('repteis/shp/{nome}_ocorrencia_semi_aquaticas_pontos.shp'), delete_layer = TRUE)


# Ocorrências geral
dados_sf <-
  repteis %>%
  dplyr::filter(species_se %in% c(especies_aquaticas, especies_semi_aquaticas)) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias))

intersects <- st_intersects(dados_sf, bacias)
num_species <- apply(intersects, 2, function(x) length(dados_sf$species_se[x]))
bacias$numEspecies <- num_species

sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_ocorrencia_semi_aquaticas_aquaticas.shp'), delete_layer = TRUE)

num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species

sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_riqueza_semi_aquaticas_aquaticas.shp'), delete_layer = TRUE)

dados_sf %>%
  dplyr::mutate(geometry = as.character(geometry)) %>%
  as.data.frame() %>%
  dplyr::left_join(ambientes) %>%
  openxlsx::write.xlsx(., 'repteis/ocorrencias/repteis_ocorrencias_exportado.xlsx', overwrite = TRUE)


# Riqueza ameaçados
ameacas <- c('CR', 'EN', 'VU')

dados_sf <-
  repteis %>%
  dplyr::filter(species_se %in% c(especies_aquaticas, especies_semi_aquaticas)) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(bacias)) %>%
  dplyr::mutate(
    cat1 = if_else(categori_1 %in% ameacas, 1, NA_integer_),
    cat2 = if_else(categoria_ %in% ameacas, 1, NA_integer_),
    sp_ameacada = coalesce(cat1, cat2)
  ) %>%
  dplyr::filter(sp_ameacada == 1)

intersects <- st_intersects(dados_sf, bacias)

num_species <- apply(intersects, 2, function(x) n_distinct(dados_sf$species_se[x]))
bacias$numEspecies <- num_species

sf::write_sf(bacias, glue::glue('repteis/shp/{nome}_riqueza_semi_aquaticas_aquaticas.shp'), delete_layer = TRUE)

