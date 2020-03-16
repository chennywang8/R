library(raster)
library(rgdal)


# ======== Read Raw Data ===========
fdir <- "NEON-DS-Field-Site-Spatial-Data/SJER"
dsm_file <- "DigitalSurfaceModel/SJER2013_DSM.tif"
dtm_file <- "DigitalTerrainModel/SJER2013_DTM.tif"
dsm  <- raster(file.path(fdir, dsm_file))
dtm  <- raster(file.path(fdir, dtm_file))
GDALinfo(file.path(fdir, dsm_file))

# ============= CREATE CHM ====================
chm <- dsm - dtm
plot(chm, main="Lidar Canopy Height Model \n SJER, California")
# specs
print(chm)
# resolution
res(chm)
ncol(chm)
ncell(chm)
# coordinate reference system (CRS)
projection(chm)
# other API
hasValues(chm)
inMemory(chm)


# ============= SAVE GTiff ===================
writeRaster(chm, file.path(fdir, "chm_sjer_output.tif"), "GTiff", overwrite = TRUE)


