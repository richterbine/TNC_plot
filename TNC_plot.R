library(raster)
library(rgdal)

# shapefile with the 14 ecoregions
TNC <- readOGR(here::here("Biomas_TNC.shp"))

names <- unique(TNC$BIOME_NAME)
names <- names[names != "N/A"]

tnc.list <- list()

for (i in 1:length(names)) {
  tnc.list[[i]] <- TNC[TNC$BIOME_NAME == names[i],]
}

names(tnc.list) <- names

plot(tnc.list[[1]])

samp.points <- list()
for (i in 1:length(names)) {
  samp.points[[i]] <- sp::spsample(x = tnc.list[[i]], n = 100, type = "random")
}

as.data.frame(samp.points)
df.points <- rbind(as.data.frame(samp.points[[1]]), as.data.frame(samp.points[[2]]),
                   as.data.frame(samp.points[[3]]), as.data.frame(samp.points[[4]]),
                   as.data.frame(samp.points[[5]]), as.data.frame(samp.points[[6]]),
                   as.data.frame(samp.points[[7]]), as.data.frame(samp.points[[8]]),
                   as.data.frame(samp.points[[9]]), as.data.frame(samp.points[[10]]),
                   as.data.frame(samp.points[[11]]), as.data.frame(samp.points[[12]]),
                   as.data.frame(samp.points[[13]]), as.data.frame(samp.points[[14]]))
df.points$names <- rep(names, each = 100)


# read the bioclimatic data
temp <- raster(here::here("wc2.1_10m_bio_1.tif"))
prec <- raster(here::here("wc2.1_10m_bio_12.tif"))
bios <- raster::stack(temp, prec)


# Extract temperature and precipitation values from the raster datasets
extractions <- raster::extract(bios, df.points[,1:2], df = TRUE)
str(extractions)

names(extractions) <- c("ID", "temp_C", "prec_cm")

# Adjust temperature values to normal scale because WorldClim temperature data
# has a scale factor of 10 (integer storage for saving space).
# Convert precipitation from mm to cm
extractions$prec_cm <- extractions$prec_cm/10

head(extractions)

df.full <- cbind(df.points, extractions[,2:3])
df.full

library(ggplot2)
library(dplyr)

plot_1 <- ggplot() +
  # add biome polygons
  geom_polygon(data = df.full,
               aes(x    = temp_C,
                   y    = prec_cm,
                   fill = names),
               # adjust polygon borders
               colour = "black",
               size   = 1) +
  theme_bw()
plot_1

#getting the convex hull of each unique point set
library(plyr)
find_hull <- function(df.full) df.full[chull(df.full$temp_C, df.full$prec_cm), ]
hulls <- ddply(df.full, "names", find_hull)

plot <- ggplot(data = df.full, aes(x = temp_C, y = prec_cm, 
                                   colour = names, 
                                   fill = names)) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.5) 
#  theme(legend.position = "none")

plot
