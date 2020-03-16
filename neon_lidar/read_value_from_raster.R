library(raster)
library(rgdal)
library(dplyr)
library(maptools)
library(ggplot2)

# ======== Read Raw Data ===========
fdir <- "NEON-DS-Field-Site-Spatial-Data/SJER"
# read in plot centroids
centroids <- read.csv(file.path(fdir, "PlotCentroids/SJERPlotCentroids.csv"))
# read in vegetation heights
vegStr <- read.csv(file.path(fdir, "VegetationData/D17_2013_vegStr.csv"))
# import the digital terrain model
chm <-raster(file.path(fdir, "CHM_SJER.tif"))

# ====== Plot & Tree Locations =========
myCol <- terrain.colors(6)
plot(chm, col = myCol, breaks = c(-2,0,2,10,40))
points(centroids$easting, centroids$northing, pch = 0, cex = 2)
points(vegStr$easting, vegStr$northing, pch = 19, cex = 0.5, col = 2)

# ===== Convert Data Points(easting&northing) 2 Spatial Points Data Frame =====
centroid_spdf <- SpatialPointsDataFrame(centroids[,4:3],
                                        proj4string = crs(chm),
                                        data = centroids)

# ======= Extract Data From a Circular Buffer =========
cent_max <- raster::extract(chm, centroid_spdf, buffer = 20, fun = max, df = TRUE)
cent_max$plot_id <- centroid_spdf$Plot_ID
names(cent_max)  <- c("ID", "chmMaxHeight", "Plot_ID")

centroids <- centroids %>% inner_join(cent_max, by = "Plot_ID")

# ============ Extract All Pixel heights =============
cent_heightList <- raster::extract(chm, centroid_spdf, buffer = 20)
ff <- sapply(cent_heightList, function(f) max(f)) %>% max() %>% ceiling()
cent_comp <- do.call(rbind, lapply(seq_along(cent_heightList), function(i) {
  ht <- hist(cent_heightList[[i]], breaks = c(-1, 0, seq(ff)), plot = FALSE)
  data.frame(x = ht$mids+0.5, y = ht$counts, name = centroids$Plot_ID[i])
}))

# generate distribution for each plot
ggplot(cent_comp, aes(x = x, y = y, color = name, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(name ~ ., ncol = 6)


# ======= Extract Data Using a Shapefile ==========
centShape <- readOGR(file.path(fdir, "PlotCentroids/SJERPlotCentroids_Buffer.shp"))
centroids$chmMaxShape <- raster::extract(chm, centShape, weights = FALSE, fun = max)


# ==========  Extract Summary Data ============
maxStemHeight <- vegStr %>%
  group_by(plotid) %>%
  summarise(max = max(stemheight), quant = quantile(stemheight, 0.95))

names(maxStemHeight) <- c("Plot_ID", "insituMaxHeight", "insituQuant")

# ============== COMBINATION ==================
centroids <- centroids %>% inner_join(maxStemHeight, by = "Plot_ID")
ggplot(centroids, aes(x = chmMaxShape, y = insituMaxHeight)) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, lty = 2) +
  geom_point() +
  geom_smooth(method = lm)
