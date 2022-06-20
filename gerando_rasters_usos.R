#===============================================================================

# piloto para gerar rasters de uso do solo para cada uso e cenário do Globiom

#===============================================================================

# caminho pasta de dados globiom

p <- "/dados/projetos_andamento/TRADEhub"

# nome da pasta

pasta <- "GLOBIOM/atualizacao/scen"

# cenarios atualizados 26/05/2022

scen <- list.files(file.path(p,pasta))

#-------------------------------------------------------------------------------

# *** OBS ***
# Os arquivos em .nc contem multiplas dimensoes. Cada cenario tem 5 dimensoes 
# temporais e 12 dimensoes de usos do solo


# pacotes

library(ncdf4) # le arquivos tipo .nc
library(raster)
library(stringr)

# dados primarios plangea_web como ref.de projecao e resolucao
# /dados/projetos_andamento/plangea_web/rawdata/land-use

# resultados finais
# /dados/projetos_andamento/plangea_web/restoration/results/post_processed/priority_maps


#-------------------------------------------------------------------------------

################################################################################
# Piloto com 1 cenario downscalling
################################################################################

# abrindo um cenario como exemplo

ncname <- scen[1]  

ncfname <- file.path(p, pasta,ncname)

# abre a netCDF 

ncin <- nc_open(ncfname)


# mostra os dados armazenados

print(ncin)

# diretorio pra jogar rasters temporarios 

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

# sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 

# diretorio para salvar os cenarios reprojetados

dir.create("projected_scen")

# vetor com 12 usos
usos <- c("cropland_other_1","cropland_2Gbioen_2","grassland_3",
          "forest_unmanaged_4","forest_managed_5","restored_6","other_7",
          "built-up_8","abn_cropland_other_9","abn_cropland_2Gbioen_10",
          "abn_grassland_11","abn_forest_managed_12")


# raster base (dados primarios plangea_web)
#usei cropland

r_b <- raster("/dados/projetos_andamento/plangea_web/rawdata/land-use/croplands.tif")

res(r_b) # ~5kmx5km

crs <- crs(r_b)

# loop pra gerar os rasters 5x5m

adequando_resolucao_5x5km <- function(i){

  # abre para cenario x, um determinado uso (i de 12)
  r <- brick(ncfname, varname="LC_area_share",level=i) #index
  # projeta baseado no rawdata plangea
  r_p <- projectRaster(from = r,to = r_b)
  
  # criando pasta pra cada uso (serao 12 total)
  
  dir.path <- file.path("projected_scen",usos[i]) #index
  
  dir.create(dir.path) 
  
  # subtrair 3 digitos do nome do arquivo(.nc)
  
  raster_name_1 <-str_remove(string = ncname,pattern = ".nc")
  
  # nome do raster final
  
  raster_name_2 <- paste0(raster_name_1,'_',"rep_5x5km","_",usos[i])
  
  # obs: os nomes das bandas se perde quando salvo
  
  writeRaster(r_p,filename = file.path(dir.path,paste0(raster_name_2,".tif")),
              overwrite=TRUE)
}

# rodando o loop (o vetor x define quais usos utilizar (1:12))

lapply(X = 3:12,adequando_resolucao_5x5km)


################################################################################
# desagregando cenarios mantendo a resolucao original (0.5 x 0.5 graus)
################################################################################



desagregando_rasters <- function(i){
  
  # abre para cenario x, um determinado uso (i de 12)
  r <- brick(cenario_path, varname="LC_area_share",level=i) #index
  
  # criando pasta pra cada uso (serao 12 total)
  
  dir.path <- file.path("projected_scen",usos[i]) #index
  
  dir.create(dir.path) 
  
  # subtrair 3 digitos do nome do arquivo(.nc)
  
  raster_name_1 <-str_remove(string = cenario,pattern = ".nc")
  
  # nome do raster final (precisa corrigir, pra gerar nome arquivo final)
  
  raster_name_2 <- paste0(raster_name_1,'_',"55x55km","_",usos[i])
  
  # obs: os nomes das bandas se perde quando salvo
  
  writeRaster(r,filename = file.path(dir.path,paste0(raster_name_2,".tif")),
              overwrite=TRUE)
}



for(cen in 1:20) # loop nos 20 cenarios
  
  {
  
  cenario_path <-  file.path(p, pasta,scen[cen])
  cenario <- scen[cen]
  
  lapply(X = 1:12,desagregando_rasters) # loop nos 12 usos do solo

  } 





#removes entire temp directory without affecting other running processes
unlink(file.path(file.path(p,"/temp")), recursive = TRUE)
gc()
