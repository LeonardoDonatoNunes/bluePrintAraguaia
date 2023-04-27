arquivos <- list.files(full.names = TRUE, recursive = TRUE)
mapas_path <- arquivos[str_detect(arquivos, 'mapasProntos')]
file.copy(mapas_path, 'D:\\Mapas\\')
