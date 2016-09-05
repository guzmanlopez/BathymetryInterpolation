#===========================================================================
#================= River bathymetry Interpolation Part1 ====================
#===========================================================================

# 1) Load Packages --------------------------------------------------------

# Data manipulation
library('data.table')
library('RPostgreSQL')
library('postGIStools')
library('dplyr')

# Geoestatistic
library('gstat')
library('automap')
library('gdalUtils')
library('SpatialPosition')

# GIS
library('sp')
library('raster')
library('rasterVis')
library('maptools')
library('rgdal')
library('rgeos')
library('geosphere')
library('ggmap')
library('mapview')

# The Salesman Problem (resolve shortest path)
library('TSP')

# Colors and plots
library('plotrix')
library('RColorBrewer')

# 2) Load data ---------------------------------------------------------------

load("/path/to/some/RData/filename.RData")

# 3) Creating a new Data Base ---------------------------------------------

# Create a fresh PostGIS database to hold the example data directly from R.

# system("dropdb tablename")
# system("createdb tablename")

# 4) Establishing an ODBC connection --------------------------------------

# Database info
dbName <- 'dbname'
dbHost <- 'localhost'
dbPort <- 5432
dbUser <- 'username'
dbPassword <- 'password'
dbString <- paste("PG:dbname='", dbName, "' host='", dbHost, "' port='", dbPort, "' user='", dbUser, "' password='", dbPassword, "'", sep = "")

# Load data from the PostGIS server
conn <- dbConnect(dbDriver("PostgreSQL"), dbname=dbName, host=dbHost, port=dbPort, user=dbUser, password=dbPassword)

# dbGetQuery(conn, "CREATE EXTENSION POSTGIS")
# dbGetQuery(conn, "CREATE EXTENSION POSTGIS_topology")
# dbSendQuery(conn, "CREATE EXTENSION PLR")
# dbGetQuery(conn, "SELECT PostGIS_full_version();")

# dbDisconnect(conn)

# List tables
dbListTables(conn)

# 5) Load shapefiles ------------------------------------------------------

# Projections
epsg.32721 <- "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# mapview options
mapviewOptions(basemaps = c("CartoDB.DarkMatter", "Esri.WorldImagery"),
               raster.palette = colorRampPalette(brewer.pal(11, "Spectral")),
               vector.palette = colorRampPalette(brewer.pal(9, "YlGnBu")),
               na.color = "magenta",
               layers.control.pos = "topleft")

# Load base shapefiles

# River Polygon
riverPolygon <- readOGR(dsn = '/path/to/some/river/polygon/shapefile', layer = 'riverPolygon')
mapView(x = riverPolygon, layer.name = "River Name", color = "light blue", legend = TRUE)

# Reference Line Smooth
refLineSmooth <- readOGR(dsn = '/path/to/some/Smoothed/Reference/Line/shapefile', layer = 'river-RefLine-Smooth')

# Add temporary unique ID to spatial DF
refLineSmooth$spatial_id <- 1:nrow(refLineSmooth)

# Set column names to lower case
names(refLineSmooth) <- tolower(names(refLineSmooth))

mapView(x = refLineSmooth, layer.name = "Ref. Line", color = "red", legend = TRUE)

# Load Bati Raw data
riverBathy <- fread(input = "/path/to/some/raw/depth/data/rawDepths.csv", sep = ",", header = TRUE, showProgress = TRUE, data.table = FALSE, stringsAsFactors = FALSE, verbose = TRUE)

# Remove bad line status (Line status = 0)
riverBathyF <- subset(x = riverBathy, subset = Line_status == 1)

# Create Spatial Points Data Frame
riverBathyF <- SpatialPointsDataFrame(coords = as.matrix(riverBathyF[, c(5,4)]), data = riverBathyF[,c(1,2,7)], proj4string = CRS(wgs.84))

# Transform coordinates
riverBathyF <- spTransform(x = riverBathyF, CRSobj = CRS(epsg.32721))

# Remove bati points outside river polygon
riverBathyF2 <- riverBathyF[which(!is.na(over(x = riverBathyF, y = riverPolygon))),]

# Remove duplicates
riverBathyF3 <- remove.duplicates(obj = riverBathyF2, remove.second = TRUE)

# Add temporary unique ID to spatial DF
riverBathyF3$spatial_id <- 1:nrow(riverBathyF3)

# Set column names to lower case
names(riverBathyF3) <- tolower(names(riverBathyF3))

mapView(x = riverBathyF3, layer.name = "Bati", color = "green", legend = TRUE, cex = 0.1)

# Create Regular grid
mygrid5m <- CreateGrid(w = riverPolygon, resolution = 5)
plot(mygrid5m, cex = 0.1, pch = ".")
plot(riverPolygon, border="red", lwd = 2, add = TRUE)

# Grid points inside river polygon
mygrid5m <- mygrid5m[which(!is.na(over(x = mygrid5m, y = riverPolygon))),]

# Add temporary unique ID to spatial DF
mygrid5m$spatial_id <- 1:nrow(mygrid5m)

# Set column names to lower case
names(mygrid5m) <- tolower(names(mygrid5m))

# mapView(x = mygrid10m, layer.name = "5m regular point grid", color = "cyan", legend = TRUE, cex = 0.1)

# Load BatiMargen (bati + margen = 0)
riverDepthMargen <- readOGR(dsn = '/path/to/some/river/depth/plus/margin/zero/depth/data/', layer = 'depthPlusMarginZeroDepthData')
riverDepthMargen <- riverDepthMargen[, c(1,2,7)]

# Remove duplicates
riverDepthMargen <- remove.duplicates(obj = riverDepthMargen, remove.second = TRUE)

# Add temporary unique ID to spatial DF
riverDepthMargen$spatial_id <- 1:nrow(riverDepthMargen)

# Set column names to lower case
names(riverDepthMargen) <- tolower(names(riverDepthMargen))

# 6) Write Spatial Objects to PostGIS -------------------------------------

writeOGR(obj = riverBathyF3, layer = "riverdepth", driver = "PostgreSQL", dsn = dbString, layer_options = "geometry_name=geom", overwrite_layer = FALSE)

writeOGR(obj = mygrid5m, layer = "mygrid5m", driver = "PostgreSQL", dsn = dbString, layer_options = "geometry_name=geom", overwrite_layer = FALSE)

writeOGR(obj = refLineSmooth, layer = "reflinesmooth", driver = "PostgreSQL", dsn = dbString, layer_options = "geometry_name=geom", overwrite_layer = FALSE)

writeOGR(obj = riverDepthMargen, layer = "riverdepthmargin", driver = "PostgreSQL", dsn = dbString, layer_options = "geometry_name=geom", overwrite_layer = FALSE)

# 7) Transform coordinates ------------------------------------------------

queryTransformRiverDepth <-

"CREATE TABLE riverdepthsn AS
SELECT points.spatial_id, points.depth AS depth,
st_distance(line.geom, points.geom) AS N,
ST_LineLocatePoint(st_linemerge(line.geom), (ST_Dump(points.geom)).geom) * st_length(st_linemerge(line.geom)) AS S,
st_makepoint(st_distance(line.geom, points.geom), ST_LineLocatePoint(st_linemerge(line.geom), (ST_Dump(points.geom)).geom) * st_length(st_linemerge(line.geom))) AS geom
FROM reflinesmooth AS line, riverdepth AS points;"

resQueryTransformRiverDepth <- dbSendQuery(conn, statement = queryTransformRiverDepth)
# dbSendQuery(conn, "DROP TABLE riverdepthsn;")

queryTransformRiverDepthMargin <-

"CREATE TABLE riverdepthmarginsn AS
SELECT points.spatial_id, points.depth AS depth,
st_distance(line.geom, points.geom) AS N,
ST_LineLocatePoint(st_linemerge(line.geom), (ST_Dump(points.geom)).geom) * st_length(st_linemerge(line.geom)) AS S,
st_makepoint(st_distance(line.geom, points.geom), ST_LineLocatePoint(st_linemerge(line.geom), (ST_Dump(points.geom)).geom) * st_length(st_linemerge(line.geom))) AS geom
FROM reflinesmooth AS line, riverdepthmargin AS points;"

resQueryTransformRiverDepthMargin <- dbSendQuery(conn, statement = queryTransformRiverDepthMargin)
# dbSendQuery(conn, "DROP TABLE riverdepthmarginsn;")

queryTransformRiverGrid <-

"CREATE TABLE rivergridsn AS
SELECT points.spatial_id,
st_distance(line.geom, points.geom) AS N,
ST_LineLocatePoint(st_linemerge(line.geom), (ST_Dump(points.geom)).geom) * st_length(st_linemerge(line.geom)) AS S,
st_makepoint(st_distance(line.geom, points.geom), ST_LineLocatePoint(st_linemerge(line.geom), (ST_Dump(points.geom)).geom) * st_length(st_linemerge(line.geom))) AS geom
FROM reflinesmooth AS line, mygrid5m AS points;"

resQueryTransformRiverGrid <- dbSendQuery(conn, statement = queryTransformRiverGrid)
# dbSendQuery(conn, "DROP TABLE rivergridsn;")

# 8) Load transformed layers ----------------------------------------------

# Points Transformed Bati
riverdepthsn <- readOGR(dsn = dbString, layer = "riverdepthsn")

# Points Transformed BatiMargin
depthData <- readOGR(dsn = dbString, layer = "riverdepthmarginsn")

# Points Transformed River Grid
rivergridsn <- readOGR(dsn = dbString, layer = "rivergridsn")
proj4string(rivergridsn) <- CRS(epsg.32721)

# Create grid pixel
gridPixelsSn <- SpatialPixelsDataFrame(points = rivergridsn@coords, data = rivergridsn@data, tolerance = 0.815886, proj4string = CRS(epsg.32721))

# Use data as:
proj4string(riverdepthmarginsn) <- CRS(epsg.32721)
riverdepthmarginsn$id <- riverdepthmarginsn$spatial_id

# 9) Estimate directional variograms --------------------------------------

# Function degrees to radians
degrees.to.radians <- function(degrees = 45, minutes = 30) {

  if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
  if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
  decimal = minutes / 60
  c.num = degrees + decimal
  radians = c.num*pi/180
  return(radians)

}

# Compute the 10 degree variogram for the following directions:
var1 <- variogram(depth~1, depthData, alpha = 0, tol.hor = 10, cutoff = 4000, width = 10)
var2 <- variogram(depth~1, depthData, alpha = 10, tol.hor = 10, cutoff = 4000, width = 10)
var3 <- variogram(depth~1, depthData, alpha = 20, tol.hor = 10, cutoff = 4000, width = 10)
var4 <- variogram(depth~1, depthData, alpha = 30, tol.hor = 10, cutoff = 4000, width = 10)
var5 <- variogram(depth~1, depthData, alpha = 40, tol.hor = 10, cutoff = 4000, width = 10)
var6 <- variogram(depth~1, depthData, alpha = 50, tol.hor = 10, cutoff = 4000, width = 10)
var7 <- variogram(depth~1, depthData, alpha = 60, tol.hor = 10, cutoff = 4000, width = 10)
var8 <- variogram(depth~1, depthData, alpha = 70, tol.hor = 10, cutoff = 4000, width = 10)
var9 <- variogram(depth~1, depthData, alpha = 80, tol.hor = 10, cutoff = 4000, width = 10)
var10 <- variogram(depth~1, depthData, alpha = 90, tol.hor = 10, cutoff = 4000, width = 10)
var11 <- variogram(depth~1, depthData, alpha = 100, tol.hor = 10, cutoff = 4000, width = 10)
var12 <- variogram(depth~1, depthData, alpha = 110, tol.hor = 10, cutoff = 4000, width = 10)
var13 <- variogram(depth~1, depthData, alpha = 120, tol.hor = 10, cutoff = 4000, width = 10)
var14 <- variogram(depth~1, depthData, alpha = 130, tol.hor = 10, cutoff = 4000, width = 10)
var15 <- variogram(depth~1, depthData, alpha = 140, tol.hor = 10, cutoff = 4000, width = 10)
var16 <- variogram(depth~1, depthData, alpha = 150, tol.hor = 10, cutoff = 4000, width = 10)
var17 <- variogram(depth~1, depthData, alpha = 160, tol.hor = 10, cutoff = 4000, width = 10)
var18 <- variogram(depth~1, depthData, alpha = 170, tol.hor = 10, cutoff = 4000, width = 10)

# 10) Range estimation -----------------------------------------------------

range <- numeric()

# Function to search for the range in no-fit variograms
# psill = psill estimated from fit variograms
# gstatVariogram = variograms
# excludeInterval = exclude first up and down points in variograms to reach the psill
searchRange <- function(psill, gstatVariogram, excludeInterval) {

  x2 = NULL

  nearPsillCell = which(abs(gstatVariogram$gamma[-(excludeInterval)] - psill) == min(abs(gstatVariogram$gamma[-(excludeInterval)] - psill), na.rm = TRUE)) + excludeInterval[length(excludeInterval)]
  dy = gstatVariogram$gamma[nearPsillCell - 1] - gstatVariogram$gamma[nearPsillCell]
  dx = gstatVariogram$dist[nearPsillCell - 1] - gstatVariogram$dist[nearPsillCell]
  m = dy/dx
  y2 = psill
  y1 = gstatVariogram$gamma[nearPsillCell]
  x1 = gstatVariogram$dist[nearPsillCell]
  x2 = ((y2 - y1) / m) + x1

  return(x2)
}

# Fit variograms models using Ordinary Least Squares (OLS)

fitVar1 <- fit.variogram(object = var1, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var1$dist, y = var1$gamma, main = "Fit variogram [alpha = 0]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar1, maxdist = 4000), col = "red", lwd = 1)
range[1] <- fitVar1$range

fitVar2 <- fit.variogram(object = var2, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var2$dist, y = var2$gamma, main = "Fit variogram [alpha = 10]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar2, maxdist = 4000), col = "red", lwd = 1)
range[2] <- fitVar2$range

fitVar18 <- fit.variogram(object = var18, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var18$dist, y =var18$gamma, main = "Fit variogram [alpha = 170]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar18, maxdist = 4000), col = "red", lwd = 1)
range[18] <- fitVar18$range

psill <- mean(fitVar1$psill, fitVar2$psill, fitVar18$psill)

var3b <- var3
# var3b$gamma <- log(var3$gamma + 1)
fitVar3b <- fit.variogram(object = var3b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var3b$dist, y =var3b$gamma, main = "Fit variogram [alpha = 20]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar3b, maxdist = 4000), col = "red", lwd = 1)
range[3] <- searchRange(psill = psill, gstatVariogram = var3, excludeInterval = 1:4)

var4b <- var4
# var4b$gamma <- log(var4$gamma + 1)
fitVar4b <- fit.variogram(object = var4b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var4b$dist, y =var4b$gamma, main = "Fit variogram [alpha = 30]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar4b, maxdist = 4000), col = "red", lwd = 1)
range[4] <- searchRange(psill = psill, gstatVariogram = var4, excludeInterval = 1:5)

var5b <- var5
# var5b$gamma <- log(var4$gamma + 1)
fitVar5b <- fit.variogram(object = var5b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var5b$dist, y =var5b$gamma, main = "Fit variogram [alpha = 40]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar5b, maxdist = 4000), col = "red", lwd = 1)
range[5] <- searchRange(psill = psill, gstatVariogram = var5, excludeInterval = 1:4)

var6b <- var6
# var6b$gamma <- log(var6$gamma + 1)
fitVar6b <- fit.variogram(object = var6b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var6b$dist, y =var6b$gamma, main = "Fit variogram [alpha = 50]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar6b, maxdist = 4000), col = "red", lwd = 1)
range[6] <- searchRange(psill = psill, gstatVariogram = var6, excludeInterval = 1:4)

var7b <- var7
# var7b$gamma <- log(var7$gamma + 1)
fitVar7b <- fit.variogram(object = var7b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var7b$dist, y =var7b$gamma, main = "Fit variogram [alpha = 60]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar7b, maxdist = 4000), col = "red", lwd = 1)
range[7] <- searchRange(psill = psill, gstatVariogram = var7, excludeInterval = 1:4)

var8b <- var8
# var8b$gamma <- log(var8$gamma + 1)
fitVar8b <- fit.variogram(object = var8b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var8b$dist, y =var8b$gamma, main = "Fit variogram [alpha = 70]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar8b, maxdist = 4000), col = "red", lwd = 1)
range[8] <- searchRange(psill = psill, gstatVariogram = var8, excludeInterval = 1:4)

var9b <- var9
# var8b$gamma <- log(var8$gamma + 1)
fitVar9b <- fit.variogram(object = var9b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 1.6, range = 150), fit.method = 6)
plot(x = var9b$dist, y =var9b$gamma, main = "Fit variogram [alpha = 80]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar9b, maxdist = 4000), col = "red", lwd = 1)
range[9] <- searchRange(psill = psill, gstatVariogram = var9, excludeInterval = 1:4)

var10b <- var10
# var10b$gamma <- log(var10$gamma + 1)
fitVar10b <- fit.variogram(object = var10b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var10b$dist, y =var10b$gamma, main = "Fit variogram [alpha = 90]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar10b, maxdist = 4000), col = "red", lwd = 1)
range[10] <- searchRange(psill = psill, gstatVariogram = var10, excludeInterval = 1:4)

var11b <- var11
# var10b$gamma <- log(var10$gamma + 1)
fitVar11b <- fit.variogram(object = var11b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var11b$dist, y =var11b$gamma, main = "Fit variogram [alpha = 100]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar11b, maxdist = 4000), col = "red", lwd = 1)
range[11] <- searchRange(psill = psill, gstatVariogram = var11, excludeInterval = 1:4)

var12b <- var12
# var10b$gamma <- log(var10$gamma + 1)
fitVar12b <- fit.variogram(object = var12b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var12b$dist, y =var12b$gamma, main = "Fit variogram [alpha = 110]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar12b, maxdist = 4000), col = "red", lwd = 1)
range[12] <- searchRange(psill = psill, gstatVariogram = var12, excludeInterval = 1:4)

var13b <- var13
# var10b$gamma <- log(var10$gamma + 1)
fitVar13b <- fit.variogram(object = var13b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var13b$dist, y =var13b$gamma, main = "Fit variogram [alpha = 120]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar13b, maxdist = 4000), col = "red", lwd = 1)
range[13] <- searchRange(psill = psill, gstatVariogram = var13, excludeInterval = 1:4)

var14b <- var14
# var10b$gamma <- log(var10$gamma + 1)
fitVar14b <- fit.variogram(object = var14b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var14b$dist, y =var14b$gamma, main = "Fit variogram [alpha = 130]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar14b, maxdist = 4000), col = "red", lwd = 1)
range[14] <- searchRange(psill = psill, gstatVariogram = var14, excludeInterval = 1:4)

var15b <- var15
# var10b$gamma <- log(var10$gamma + 1)
fitVar15b <- fit.variogram(object = var15b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var15b$dist, y =var15b$gamma, main = "Fit variogram [alpha = 140]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar15b, maxdist = 4000), col = "red", lwd = 1)
range[15] <- searchRange(psill = psill, gstatVariogram = var15, excludeInterval = 1:4)

var16b <- var16
# var10b$gamma <- log(var10$gamma + 1)
fitVar15b <- fit.variogram(object = var16b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var16b$dist, y =var16b$gamma, main = "Fit variogram [alpha = 150]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar16b, maxdist = 4000), col = "red", lwd = 1)
range[16] <- searchRange(psill = psill, gstatVariogram = var16, excludeInterval = 1:4)

var17b <- var17
# var10b$gamma <- log(var10$gamma + 1)
fitVar17b <- fit.variogram(object = var17b, model = vgm(model = c("Exp", "Sph", "Gau", "Exc", "Mat", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav"), psill = 0.2, range = 50), fit.method = 6)
plot(x = var17b$dist, y =var17b$gamma, main = "Fit variogram [alpha = 160]", col = "black", ylab = "Semivariance", xlab = "Distance")
lines(variogramLine(object = fitVar17b, maxdist = 4000), col = "red", lwd = 1)
range[17] <- searchRange(psill = psill, gstatVariogram = var17, excludeInterval = 1:4)

# Query fit variogram object attributes
attr(fitVar1, 'SSErr')
attr(fitVar1, 'singular')

# 11) Plot Anisotropy -----------------------------------------------------

theta <- c(degrees.to.radians(degrees = seq(0, 170, 10), minutes = 0))

# Compute the coordinates:
x1 <- sin(theta[1:10]) * range[1:10]
y1 <- cos(theta[1:10]) * range[1:10]
x2 <- sin(theta[11:18]) * range[11:18]
y2 <- cos(theta[11:18]) * range[11:18]

x11 <- -x1
y11 <- -y1
x22 <- -x2
y22 <- -y2

plot(x1, y1, xlim = c(-max(range), max(range)), ylim = c(-max(range), max(range)), xaxt = "n", yaxt = "n", ylab = "y", xlab = "x", pch = 19, asp = 1)
points(x11, y11, pch = 19)
points(x2, y2, pch = 19)
points(x22, y22, pch = 19)
segments(x1,y1, x11, y11)
segments(x2,y2, x22, y22)
segments(0, -max(range), 0, max(range), lty = 2)
segments(-max(range), 0, max(range), 0, lty = 2)

# model the major and minor axes of anisotropy and radio
anisRatio <- min(range)/max(range)
anisAngle <- 0

plot(x1, y1, xlim = c(-max(range), max(range)), ylim = c(-max(range), max(range)), yaxt = "n", ylab = "y", xlab = "x", pch = 19, asp = 1)
points(x11, y11, pch = 19)
points(x2, y2, pch = 19)
points(x22, y22, pch = 19)
segments(x1,y1, x11, y11)
segments(x2,y2, x22, y22)
segments(0, -max(range), 0, max(range), lty = 2)
segments(-max(range), 0, max(range), 0, lty = 2)
plotrix::draw.ellipse(x = 0, y = 0, a = min(range), b = max(range), angle = anisAngle, border = "blue")

# 12) Fit anisotropic variogram -------------------------------------------

# Manually fit variogram
vgmModel <- vgm(model = fitVar1$model, psill = fitVar1$psill, range = fitVar1$range, anis = c(anisAngle, anisRatio))
plot(var1, vgmModel, main = "Manual fit anisotropic variogram")

# 13) Clip SPDF by regions ------------------------------------------------

# Clip Spatial Objects
ClipSpObj <- function(xmin, xmax, ymin, ymax, depthData, gridSn) {

  vectorClip = which(depthData@coords[,1] >= xmin & depthData@coords[,1] < xmax & depthData@coords[,2] >= ymin & depthData@coords[,2] < ymax)
  depthDataClipped = depthData[vectorClip,]

  vectorClip2 = which(gridSn@coords[,1] >= xmin & gridSn@coords[,1] < xmax & gridSn@coords[,2] >= ymin & gridSn@coords[,2] < ymax)
  gridSnClipped = gridSn[vectorClip2,]

  # gridPixelsSnClipped = SpatialPixelsDataFrame(points = gridSnClipped,  data = data.frame(rep(1, nrow(gridSnClipped))), tolerance = 0.815886, proj4string = CRS(epsg.32721))

  returnList = list()
  returnList$"depthDataClipped" = depthDataClipped
  returnList$"gridSnClipped" = gridSnClipped
  # returnList$"gridPixelsSnClipped" = gridPixelsSnClipped

  return(returnList)

}

projection(depthData) <- CRS(epsg.32721)
projection(rivergridsn) <- CRS(epsg.32721)

clippedSpObj1 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 0, ymax = 2000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj2 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 2000, ymax = 4000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj3 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 4000, ymax = 6000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj4 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 6000, ymax = 8000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj5 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 8000, ymax = 10000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj6 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 10000, ymax = 12000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj7 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 12000, ymax = 14000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj8 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 14000, ymax = 16000, depthData = depthData, gridSn = rivergridsn)
clippedSpObj9 <- ClipSpObj(xmin = 0, xmax = 180, ymin = 16000, ymax = 18000, depthData = depthData, gridSn = rivergridsn)

# 14) Model interpolation validation --------------------------------------

# Independent validation
# in order to try an independent validation we first need to subset the data into
# a training and a test subset.
# Then, we use the training dataset to predict the value in the test dataset.

IndependentValidation <- function(training, test, vgmModel, maxDist, nMax) {

  gof = data.frame("Pearsons.R.Squared" = NA, "Root.Mean.Square.Deviation" = NA)

  # krige the trainig subset
  OKdepth = krige(formula = depth~1, locations = training, newdata = test, model = vgmModel, maxdist = maxDist, nmax = nMax)

  # Goodness of Fit of the Cross Validation
  RSQR = as.numeric(cor.test(test$depth, OKdepth$var1.pred)$estimate)^2  # Pearson's R Squared
  RMSD = sqrt(sum((test$depth-OKdepth$var1.pred)^2)/length(test$depth)) # Root Mean Square Deviation

  gof[1,1] = RSQR
  gof[1,2] = RMSD

  # Plot observed versus residuals
  plot(test$depth, OKdepth$var1.pred, asp = 1, xlab = "Test depth (m)", ylab = "Predicted depth (m)", main = paste("Exclude = ", exclude, ", maxDist = ", maxDist, ", nMax = ", nMax))
  abline(0,1, col = "red", cex = 0.5)

  return(gof)

}

exclude <- 30  # exclude xx% of the data

i <- sample(nrow(clippedSpObj$depthDataClipped),round(nrow(clippedSpObj$depthDataClipped)*exclude/100))
trainingSet <- clippedSpObj$depthDataClipped[-i,]
testSet <- clippedSpObj$depthDataClipped[i,]

IVmD300nM1000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 1000)
IVmD300nM2000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 2000)
IVmD300nM3000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 3000)
IVmD300nM4000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 4000)
IVmD300nM5000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 5000)
IVmD300nM6000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 6000)
IVmD300nM7000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 7000)
IVmD300nM8000 <- IndependentValidation(training = trainingSet, test = testSet, vgmModel = vgmModel, maxDist = 300, nMax = 8000)

# Pearsons R Squared

# max dist = 75
plot(y = c(IVmD75nM1000$Pearsons.R.Squared, IVmD75nM2000$Pearsons.R.Squared, IVmD75nM3000$Pearsons.R.Squared, IVmD75nM4000$Pearsons.R.Squared, IVmD75nM5000$Pearsons.R.Squared, IVmD75nM6000$Pearsons.R.Squared, IVmD75nM7000$Pearsons.R.Squared, IVmD75nM8000$Pearsons.R.Squared), x = seq(1000, 8000, 1000), ylab = "Pearsons R Squared", xlab = "nmax", type = "b", main = "max dist = 75")

# max dist = 300
plot(y = c(IVmD300nM1000$Pearsons.R.Squared, IVmD300nM2000$Pearsons.R.Squared, IVmD300nM3000$Pearsons.R.Squared, IVmD300nM4000$Pearsons.R.Squared, IVmD300nM5000$Pearsons.R.Squared, IVmD300nM6000$Pearsons.R.Squared), x = seq(1000, 6000, 1000), ylab = "Pearsons R Squared", xlab = "nmax", main = "", col = "blue", type = "b", main = "max dist = 300")

# Root Mean Square Deviation

# max dist = 75
plot(y = c(IVmD75nM1000$Root.Mean.Square.Deviation, IVmD75nM2000$Root.Mean.Square.Deviation, IVmD75nM3000$Root.Mean.Square.Deviation, IVmD75nM4000$Root.Mean.Square.Deviation, IVmD75nM5000$Root.Mean.Square.Deviation, IVmD75nM6000$Root.Mean.Square.Deviation, IVmD75nM7000$Root.Mean.Square.Deviation, IVmD75nM8000$Root.Mean.Square.Deviation), x = seq(1000, 8000, 1000), ylab = "Root Mean Square Deviation", xlab = "nmax", main = "max dist = 75", pch = 19, type = "b")

# max dist = 300
plot(y = c(IVmD300nM1000$Root.Mean.Square.Deviation, IVmD300nM2000$Root.Mean.Square.Deviation, IVmD300nM3000$Root.Mean.Square.Deviation, IVmD300nM4000$Root.Mean.Square.Deviation, IVmD300nM5000$Root.Mean.Square.Deviation, IVmD300nM6000$Root.Mean.Square.Deviation), x = seq(1000, 6000, 1000), ylab = "Root Mean Square Deviation", xlab = "nmax", main = "max dist = 300", pch = 19, type = "b")

# 15) Interpolation models -------------------------------------------------

# Predict - Ordinary Kriging interpolation
OKDepthmD300nM4000Part1 <- krige(formula = depth~1, locations = clippedSpObj1$depthDataClipped, newdata = clippedSpObj1$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part2 <- krige(formula = depth~1, locations = clippedSpObj2$depthDataClipped, newdata = clippedSpObj2$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part3 <- krige(formula = depth~1, locations = clippedSpObj3$depthDataClipped, newdata = clippedSpObj3$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part4 <- krige(formula = depth~1, locations = clippedSpObj4$depthDataClipped, newdata = clippedSpObj4$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part5 <- krige(formula = depth~1, locations = clippedSpObj5$depthDataClipped, newdata = clippedSpObj5$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part6 <- krige(formula = depth~1, locations = clippedSpObj6$depthDataClipped, newdata = clippedSpObj6$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part7 <- krige(formula = depth~1, locations = clippedSpObj7$depthDataClipped, newdata = clippedSpObj7$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part8 <- krige(formula = depth~1, locations = clippedSpObj8$depthDataClipped, newdata = clippedSpObj8$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)
OKDepthmD300nM4000Part9 <- krige(formula = depth~1, locations = clippedSpObj9$depthDataClipped, newdata = clippedSpObj9$gridSnClipped, model = vgmModel, maxdist = 300, nmax = 4000)

# 16) Transform interpolated data -----------------------------------------

TransformInterpolatedData <- function(intData, gridSn, grid, gridResolution) {

  batiOKaux = intData
  gridded(batiOKaux) = FALSE

  # Combine kriged data with transformed grid to obtain ID
  gridOverDataKriged = spCbind(obj = gridSn, x = batiOKaux@data)

  # Combinar capa de datos interpolados con capa grid original sin coordenadas transformadas
  gridOverDataKriged = merge(x = grid, y = gridOverDataKriged@data, by.x = "spatial_id", by.y = "spatial_id")

  # Convert points centroids to polygon

  # set the radius (meters) for the plots
  radius = gridResolution/2

  #define the plot boundaries based upon the plot radius.
  #NOTE: this assumes that plots are oriented North and are not rotated.
  #If the plots are rotated, you'd need to do additional math to find
  #the corners.
  yPlus = gridOverDataKriged@coords[,2] + radius
  xPlus = gridOverDataKriged@coords[,1] + radius
  yMinus = gridOverDataKriged@coords[,2] - radius
  xMinus = gridOverDataKriged@coords[,1] - radius

  #Extract the plot ID information. NOTE: because we set
  #stringsAsFactor to false above, we can import the plot
  #ID's using the code below. If we didn't do that, our ID's would
  #come in as factors by default.
  #We'd thus have to use the code ID=as.character(centroids$Plot_ID)
  ID = as.character(gridOverDataKriged@data[,1])

  # Calculate polygon coordinates for each plot centroid.
  square = cbind(xMinus, yPlus, xPlus, yPlus, xPlus, yMinus, xMinus, yMinus, xMinus, yPlus, xMinus, yPlus)

  # Create spatial polygons
  polys = SpatialPolygons(mapply(function(poly, id) {

    xy = matrix(poly, ncol = 2, byrow = TRUE)
    Polygons(list(Polygon(xy)), ID = id)

  },

  split(square, row(square)), ID),
  proj4string = CRS(epsg.32721)
  )

  # Combinar kriged data con polys
  polysOverDataKriged = over(x = polys, y = gridOverDataKriged)

  # recombinar
  polysOverDataKriged = SpatialPolygonsDataFrame(Sr = polys, data = polysOverDataKriged, match.ID = TRUE)
  proj4string(polysOverDataKriged) = CRS(epsg.32721)

  polysOverDataKriged <- polysOverDataKriged[-which(is.na(polysOverDataKriged$var1.pred)), ]

  return(polysOverDataKriged)

}

OKDepthmD300nM4000Part1Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part1, gridSn = clippedSpObj1$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part2Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part2, gridSn = clippedSpObj2$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part3Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part3, gridSn = clippedSpObj3$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part4Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part4, gridSn = clippedSpObj4$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part5Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part5, gridSn = clippedSpObj5$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part6Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part6, gridSn = clippedSpObj6$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part7Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part7, gridSn = clippedSpObj7$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part8Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part8, gridSn = clippedSpObj8$gridSnClipped, grid = mygrid5m, gridResolution = 5)
OKDepthmD300nM4000Part9Polys <- TransformInterpolatedData(intData = OKDepthmD300nM4000Part9, gridSn = clippedSpObj9$gridSnClipped, grid = mygrid5m, gridResolution = 5)

# 17) Export model to polygon shapefile -----------------------------------

writeOGR(obj = OKDepthmD300nM4000Part9Polys, dsn = "/path/to/exported/models/", layer = "OKDepthmD300nM4000Part9", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# 18) Find Thalweg points -------------------------------------------------

# Load paralell lines (margins) from river
riverMargins <- readOGR(dsn = '/path/to/margin/lines/', layer = 'riverMargins')

# 10m line segments
line1 <- SpatialPoints(coords = as.data.frame(coordinates(riverMargins[1,])), proj4string = CRS(wgs.84))
sample <- seq(1, nrow(line1@coords), 10)
line1 <- line1[sample,]

# Line
line2 <- SpatialLines(LinesList = riverMargins[2,]@lines, proj4string = CRS(wgs.84))

# Minimum distance between lines
distBetweenLines <- dist2Line(p = line1, line = line2)
pointsDist <- SpatialPoints(coords = distBetweenLines[, c(2,3)], proj4string = CRS(wgs.84))

# Centerline points
midPoints <- midPoint(p1 = line1, p2 = pointsDist)
midPoints <- SpatialPoints(coords = midPoints, proj4string = CRS(wgs.84))

# Make Centerline
midLine <- SpatialLines(LinesList = list(Lines(slinelist = Line(coords = midPoints), ID = "midline")), proj4string = CRS(wgs.84))
midLine <- SpatialLinesDataFrame(sl = midLine, data = data.frame(ID = NA), match.ID = FALSE)

# Write centerline shapefile
writeOGR(obj = midLine, dsn = "/path/to/centerline/", layer = "midLine", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Mapview
# map <- mapView(x = riverMargins, layer.name = "Arroyo Valizas", color = "blue", legend = TRUE)
# map2 <- mapView(x = SpatialPointsDataFrame(coords = line1@coords, data = data.frame(ID = 1:nrow(line1@coords)), proj4string = CRS(wgs.84)), layer.name = "Line1", color = "red", legend = TRUE)
# map3 <- mapView(x = SpatialLinesDataFrame(sl = line2, data = data.frame(ID = 1)), layer.name = "Line3", color = "red", legend = TRUE)
# map4 <- mapView(x = SpatialPointsDataFrame(coords = pointsDist@coords, data = data.frame(ID = 1:nrow(pointsDist@coords)), proj4string = CRS(wgs.84)), layer.name = "Distances", color = "green", legend = TRUE)
# map5 <- mapView(x = SpatialPointsDataFrame(coords = midPoints@coords, data = data.frame(ID = 1:nrow(midPoints@coords)), proj4string = CRS(wgs.84)), layer.name = "Mid Points", color = "orange", legend = TRUE)
# map6 <- mapView(x = midLine, layer.name = "Midline", color = "red", legend = TRUE)
# map + map6

# Edit centerline in QGIS and smooth it using the plugin "Generalizer" (Algorithm: Ckaiken, level: 2, weight: 3)
# Load edited Centerline

centerLine <- readOGR(dsn = "/path/to/midLine/shapefile/", layer = "midLine")
centerLineUTM <- spTransform(centerLine, CRSobj = CRS(epsg.32721))

# Equal spaced points along centerline
norm_vec <- function(x) sqrt(sum(x^2))
new_point <- function(p0, p1, di) {

  # Finds point in distance di from point p0 in direction of point p1
  v = p1 - p0
  u = v / norm_vec(v)

  return (p0 + u * di)
}
find <- function(line, distance) {

  result = line[1, , drop = FALSE]

  # for all subsequent points p1, p2 in this data.frame norm_vec(p2 - p1) = distance at all times
  equidistantPoints = line[1, , drop = FALSE]
  line = tail(line, n = -1)
  accDist = 0


  while (nrow(line) > 0) {
    point = line[1,]
    lastPoint = result[1,]

    dist = norm_vec(point - lastPoint)

    if ( accDist + dist > distance ) {
      np = new_point(lastPoint, point, distance - accDist)
      equidistantPoints = rbind(np, equidistantPoints) # add np to equidistantPoints
      result = rbind(np, result) # add np to result
      accDist = 0 # reset accDist
    } else {
      # move point from line to result
      line = tail(line, n = -1)
      result = rbind(point, result)
      # update accDist
      accDist = accDist + dist
    }
  }
  allPoints = result[NROW(result):1,] # reverse result
  return(list(newPoints = equidistantPoints, allPoints = allPoints))
}

equalSpacedPoints <- find(line = as.data.frame(coordinates(centerLineUTM)), distance = 10)

equalSpacedCenterLine <- SpatialLinesDataFrame(sl = SpatialLines(LinesList = list(Lines(slinelist = Line(coords = equalSpacedPoints$newPoints), ID = "equalspaced")), proj4string = CRS(epsg.32721)), data = data.frame(ID = NA), match.ID = FALSE)

equalSpacedCenterPoints <- SpatialPointsDataFrame(coords = equalSpacedPoints$newPoints, proj4string = CRS(epsg.32721), data = data.frame(ID = 1:nrow(equalSpacedPoints$newPoints)), match.ID = FALSE)

# Perpendicular lines from centerline points
equalSpacedCenterPointsWGS84 <- spTransform(equalSpacedCenterPoints, CRSobj = CRS(wgs.84))

# Center points
midEqualSpacedCenterPointsWGS84 <- midPoint(p1 = equalSpacedCenterPointsWGS84[seq(1, nrow(equalSpacedCenterPointsWGS84) - 1, 1),], p2 = equalSpacedCenterPointsWGS84[seq(2, nrow(equalSpacedCenterPointsWGS84), 1),])

midEqualSpacedCenterPoints <- SpatialPointsDataFrame(coords = midEqualSpacedCenterPointsWGS84, proj4string = CRS(wgs.84), data = data.frame(ID = 1:nrow(midEqualSpacedCenterPointsWGS84)), match.ID = FALSE)

# Segments
segmentsList = list()

for(i in 1:(nrow(equalSpacedCenterPoints)-1)) {
  segmentsList[i] <- Lines(slinelist = Line(coords = equalSpacedCenterPoints@coords[i:(i+1),]), ID = as.character(i))
}

segmentsList <- SpatialLines(LinesList = segmentsList, proj4string = CRS(epsg.32721))

midEqualSpacedCenterPoints <- spTransform(midEqualSpacedCenterPoints, CRSobj = CRS(epsg.32721))

PerpendicularLinesFromSegments <- function(spatialLinesObject, pointsInLines, distance) {

  n = length(spatialLinesObject)

  perpLinesList = list()

  for(i in 1:n) {

    x1 = spatialLinesObject@lines[i][[1]]@Lines[[1]]@coords[1,1]
    y1 = spatialLinesObject@lines[i][[1]]@Lines[[1]]@coords[1,2]

    x2 = spatialLinesObject@lines[i][[1]]@Lines[[1]]@coords[2,1]
    y2 = spatialLinesObject@lines[i][[1]]@Lines[[1]]@coords[2,2]

    x3 = pointsInLines@coords[i,1]
    y3 = pointsInLines@coords[i,2]

    vx = x2-x1
    vy = y2-y1
    len = sqrt( vx*vx + vy*vy )
    ux = -vy/len
    uy = vx/len

    x4 = x3 + distance * ux
    y4 = y3 + distance * uy

    x5 = x3 - distance * ux
    y5 = y3 - distance * uy

    # perpLinesList[[i]] = data.frame('X' = c(x1, x2, x3, x4, x5), 'Y' = c(y1, y2, y3, y4, y5))

    df = data.frame('X' = c(x4, x5), 'Y' = c(y4, y5))

    perpLinesList[[i]] = Lines(slinelist = Line(coords = as.matrix(df)), ID = as.character(i))

  }

  perpLinesListSL = SpatialLines(LinesList = perpLinesList, proj4string = CRS(epsg.32721))

  return(perpLinesListSL)

}

perpLines <- PerpendicularLinesFromSegments(spatialLinesObject = segmentsList, pointsInLines = midEqualSpacedCenterPoints, distance = 100)

# Clip perpendicular lines to parts
perpLinesPart1 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part1Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart2 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part2Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart3 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part3Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart4 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part4Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart5 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part5Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart6 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part6Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart7 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part7Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart8 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part8Polys, spgeom2 = perpLines, byid = FALSE)
perpLinesPart9 <- gIntersection(spgeom1 = OKDepthmD300nM4000Part9Polys, spgeom2 = perpLines, byid = FALSE)

LinesPartX <- function(perpLines) {

  lines <- list()

  for(i in 1:length(perpLines@lines[[1]]@Lines)) {

    lines[[i]] <- Lines(slinelist = Line(coords = perpLines@lines[[1]]@Lines[[i]]@coords), ID = as.character(i))

  }

  perpLines <- SpatialLines(LinesList = lines, proj4string = CRS(epsg.32721))

  return(perpLines)

}

perpLinesPart1 <- LinesPartX(perpLines = perpLinesPart1)
perpLinesPart2 <- LinesPartX(perpLines = perpLinesPart2)
perpLinesPart3 <- LinesPartX(perpLines = perpLinesPart3)
perpLinesPart4 <- LinesPartX(perpLines = perpLinesPart4)
perpLinesPart5 <- LinesPartX(perpLines = perpLinesPart5)
perpLinesPart6 <- LinesPartX(perpLines = perpLinesPart6)
perpLinesPart7 <- LinesPartX(perpLines = perpLinesPart7)
perpLinesPart8 <- LinesPartX(perpLines = perpLinesPart8)
perpLinesPart9 <- LinesPartX(perpLines = perpLinesPart9)

# Sample interpolated polygon data layer

# Add points to perpendicular lines for sampling interpolated data
AddPointsToPerpendicularLine <- function(perpLines, distance) {

  perpLinesPoints <- list()

  for(i in 1:length(perpLines)) {

    perpLinesPoints[[i]] <- find(line = as.data.frame(coordinates(perpLines[i])), distance = distance)

  }

  return(perpLinesPoints)
}

perpLinesPoints5mPart1 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart1, distance = 5)
perpLinesPoints5mPart2 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart2, distance = 5)
perpLinesPoints5mPart3 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart3, distance = 5)
perpLinesPoints5mPart4 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart4, distance = 5)
perpLinesPoints5mPart5 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart5, distance = 5)
perpLinesPoints5mPart6 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart6, distance = 5)
perpLinesPoints5mPart7 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart7, distance = 5)
perpLinesPoints5mPart8 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart8, distance = 5)
perpLinesPoints5mPart9 <- AddPointsToPerpendicularLine(perpLines = perpLinesPart9, distance = 5)

# Extract data from interpolated Polys
extractDataFromInterpolatedPolys <- function(perpLinesPoints, intDataPoly) {

  extractDataFromInterpolatedPolys = list()
  extractDataFromInterpolatedPolysLines = list()

  for(i in 1:length(perpLinesPoints)) {

    extractDataFromInterpolatedPolys[[i]] = over(x = SpatialPoints(coords = perpLinesPoints[[i]][1]$newPoints, proj4string = CRS(epsg.32721)), y = intDataPoly)
    extractDataFromInterpolatedPolysLines[[i]] = data.frame(perpLinesPoints[[i]][1]$newPoints)

  }

  # Merge lines with data
  mergeExtractDataFromInterpolated <- mapply(function(x, y) merge(x, y, by = 0, all = TRUE), x = extractDataFromInterpolatedPolys, y = extractDataFromInterpolatedPolysLines, SIMPLIFY = FALSE)

  return(mergeExtractDataFromInterpolated)

}

mergePart1 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart1, intDataPoly = OKDepthmD300nM4000Part1Polys)
mergePart2 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart2, intDataPoly = OKDepthmD300nM4000Part2Polys)
mergePart3 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart3, intDataPoly = OKDepthmD300nM4000Part3Polys)
mergePart4 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart4, intDataPoly = OKDepthmD300nM4000Part4Polys)
mergePart5 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart5, intDataPoly = OKDepthmD300nM4000Part5Polys)
mergePart6 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart6, intDataPoly = OKDepthmD300nM4000Part6Polys)
mergePart7 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart7, intDataPoly = OKDepthmD300nM4000Part7Polys)
mergePart8 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart8, intDataPoly = OKDepthmD300nM4000Part8Polys)
mergePart9 <- extractDataFromInterpolatedPolys(perpLinesPoints = perpLinesPoints5mPart9, intDataPoly = OKDepthmD300nM4000Part9Polys)

# Format merges
FormatMerges <- function(mergePart) {

  mergePart = lapply(mergePart, function(x) (data.frame("Row.names" = as.numeric(as.character(x[, 1])), x[,-1])))
  lapply(mergePart, function(x) data.table::setorder(x, 'Row.names', 'spatial_id'))

  mergePart = lapply(mergePart, function(x) cbind(x, "along.dist" = gDistance(spgeom1 = SpatialPoints(coords = x[,c("x","y")], proj4string = CRS(epsg.32721)), spgeom2 = SpatialPoints(coords = x[,c("x","y")], proj4string = CRS(epsg.32721)), byid = TRUE)[,1]))

  return(mergePart)

}

mergePart1 <- FormatMerges(mergePart1)
mergePart2 <- FormatMerges(mergePart2)
mergePart3 <- FormatMerges(mergePart3)
mergePart4 <- FormatMerges(mergePart4)
mergePart5 <- FormatMerges(mergePart5)
mergePart6 <- FormatMerges(mergePart6)
mergePart7 <- FormatMerges(mergePart7)
mergePart8 <- FormatMerges(mergePart8)
mergePart9 <- FormatMerges(mergePart9)

# Find channel and return a Spatial Points object
RiverChannelSP <- function(mergePart) {

  # Search for min depth and position in profiles
  riverChannelPositions = lapply(mergePart, function(x) x[which(-x[,"var1.pred"] == min(-x[,"var1.pred"])), c("x","y")])
  riverChannelPoints = do.call("rbind", riverChannelPositions)
  riverChannelPoints = SpatialPointsDataFrame(coords = riverChannelPoints, data = data.frame("ID" = 1:nrow(riverChannelPoints)), proj4string = CRS(epsg.32721))

  return(riverChannelPoints)

}

mergeSPPart1 <- RiverChannelSP(mergePart1)
mergeSPPart2 <- RiverChannelSP(mergePart2)
mergeSPPart3 <- RiverChannelSP(mergePart3)
mergeSPPart4 <- RiverChannelSP(mergePart4)
mergeSPPart5 <- RiverChannelSP(mergePart5)
mergeSPPart6 <- RiverChannelSP(mergePart6)
mergeSPPart7 <- RiverChannelSP(mergePart7)
mergeSPPart8 <- RiverChannelSP(mergePart8)
mergeSPPart9 <- RiverChannelSP(mergePart9)

# Merge all parts
mergePartsSMP <- SpatialMultiPointsDataFrame(coords = list(mergeSPPart1, mergeSPPart2, mergeSPPart3, mergeSPPart4, mergeSPPart5, mergeSPPart6, mergeSPPart7, mergeSPPart8, mergeSPPart9), data = data.frame("ID" = 1:9), proj4string = CRS(epsg.32721))

coordsMerge <- coordinates(mergePartsSMP)
rownames(coordsMerge) <- 1:nrow(coordsMerge)

mergePartsSMP <- SpatialPointsDataFrame(coords = coordsMerge, proj4string = CRS(epsg.32721), data = data.frame("ID" = 1:nrow(coordsMerge)))

# Export to shapefile to edit in QGIS
writeOGR(obj = mergePartsSMP, dsn = "/home/glopez/Documentos/DINARA/SIG/river/BathymetryModel/capasVectoriales/thalweg/", layer = "thalweg-points", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Mapview
# mapEqualSpacedCenterLine <- mapView(x = equalSpacedCenterLine, layer.name = "EqualSpacedCenterLine", color = "red", legend = TRUE)
# mapEqualSpacedCenterPoints <- mapView(x = equalSpacedCenterPoints, layer.name = "EqualSpacedCenterPoints", color = "green", legend = TRUE, cex = 0.5)
# mapPerpLines <- mapView(x = perpLines, layer.name = "Perp. Lines", color = "cyan")
# mapMidEqualSpacedCenterPoints <- mapView(x = midEqualSpacedCenterPoints, layer.name = "MidEqualSpacedCenterPoints", color = "purple", legend = TRUE, cex = 0.5)
# mapRiverChannelPointsP1 <- mapView(x = mergeSPPart1, layer.name = "RiverChannelPointsP1", color = "violet", cex = 0.6)
# mapRiverChannelPointsP2 <- mapView(x = mergeSPPart2, layer.name = "RiverChannelPointsP2", color = "violet", cex = 0.6)
# mapRiverChannelPointsP3 <- mapView(x = mergeSPPart3, layer.name = "RiverChannelPointsP3", color = "violet", cex = 0.6)
# mapRiverChannelPointsP4 <- mapView(x = mergeSPPart4, layer.name = "RiverChannelPointsP4", color = "violet", cex = 0.6)
# mapRiverChannelPointsP5 <- mapView(x = mergeSPPart5, layer.name = "RiverChannelPointsP5", color = "violet", cex = 0.6)
# mapRiverChannelPointsP6 <- mapView(x = mergeSPPart6, layer.name = "RiverChannelPointsP6", color = "violet", cex = 0.6)
# mapRiverChannelPointsP7 <- mapView(x = mergeSPPart7, layer.name = "RiverChannelPointsP7", color = "violet", cex = 0.6)
# mapRiverChannelPointsP8 <- mapView(x = mergeSPPart8, layer.name = "RiverChannelPointsP8", color = "violet", cex = 0.6)
#
# mapEqualSpacedCenterLine +
#   mapEqualSpacedCenterPoints +
#   mapMidEqualSpacedCenterPoints +
#   mapPerpLines +
#   mapRiverChannelPointsP1 +
#   mapRiverChannelPointsP2 +
#   mapRiverChannelPointsP3 +
#   mapRiverChannelPointsP4 +
#   mapRiverChannelPointsP5 +
#   mapRiverChannelPointsP6 +
#   mapRiverChannelPointsP7 +
#   mapRiverChannelPointsP8

# Load edited Thalweg points

# Open edited Thalweg points
mergePartsSMP_edited <- readOGR(dsn = '/path/to/thalweg/shapefile/', layer = 'thalweg-points-edited')

# Convert edited Thalweg points to Lines
MakeSpatialLineFromUnsortedSpatialPoints <- function(spatialPointsObject, startPointCell, projection) {

  # Check if spatialPointsObject is a SpatialPoints object
  isSP = class(spatialPointsObject) == "SpatialPoints"

  if(!isSP) {

    return(print("Warning: the spatialPointsObject argument is not a SpatialPoints object"))

  }

  # Remove duplicate points
  n = length(spatialPointsObject)
  spatialPointsObject = remove.duplicates(obj = spatialPointsObject, remove.second = TRUE)

  # Warn if there are duplicate points in data
  if(n > length(spatialPointsObject)) {

    print("Warning: you have duplicate points in your SpatialPoints object. The duplicated points were removed.")

  }

  seqCell = integer()
  seqCell[1] = startPointCell

  # Now calculate pairwise distances between points with replacement

  for(i in 2:length(spatialPointsObject)) {

    distancesFromPoint = gDistance(spgeom1 = spatialPointsObject[seqCell[i-1]], spgeom2 = spatialPointsObject, byid=TRUE)
    minDist = min(distancesFromPoint[-seqCell,])
    minDistCell = as.numeric(names(which(distancesFromPoint[,][-seqCell] == minDist)))

    seqCell[i] = minDistCell

  }

  SL = SpatialLines(LinesList = list(Lines(slinelist = Line(spatialPointsObject[seqCell]@coords), ID = "1")), proj4string = CRS(projection))

  return(SL)

}

startPointCell <- which(min(mergePartsSMP_edited@coords[,1]) == mergePartsSMP_edited@coords[,1])

plot(mergePartsSMP_edited, pch = 19, cex = 0.5, xlim = c(787961.8, (787961.8 + 500)))
plot(mergePartsSMP_edited[startPointCell,], cex = 1.2, col = "red", add = TRUE)

mergePartsSMP_Line <- MakeSpatialLineFromUnsortedSpatialPoints(spatialPointsObject = SpatialPoints(coords = mergePartsSMP_edited@coords, proj4string = CRS(epsg.32721)), startPointCell = startPointCell, projection = epsg.32721)

mergePartsSMP_Line <- SpatialLinesDataFrame(sl = mergePartsSMP_Line, data = data.frame("ID" = 1))

plot(mergePartsSMP_Line)

# 19) Export thalweg line to shapefile ------------------------------------

writeOGR(obj = mergePartsSMP_Line, dsn = "/path/to/shapefile/thalweg-line", layer = "thalweg-line", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# 20) Write Spatial Object to PostGIS -------------------------------------

# Add temporary unique ID to spatial DF
mergePartsSMP_Line$spatial_id <- 1:nrow(mergePartsSMP_Line)

# Set column names to lower case
names(mergePartsSMP_Line) <- tolower(names(mergePartsSMP_Line))

writeOGR(obj = mergePartsSMP_Line, layer = "thalwegline", driver = "PostgreSQL", dsn = dbString, layer_options = "geometry_name=geom", overwrite_layer = TRUE)

# 21) Transform coordinates to Thalweg line -------------------------------

riverdepthmargin <- readOGR(dsn = dbString, layer = "riverdepthmargin")
mergePartsSMP_Line <- readOGR(dsn = dbString, layer = "thalwegline")

### Find n

# Nearest point to line from depth position points
snapPointsToThalweg <- snapPointsToLines(points = riverdepthmargin[1:1000,], lines = mergePartsSMP_Line, withAttrs = TRUE)

# Minimum distance from line to depth position points
n <- numeric()

for(i in 1:nrow(riverdepthmargin[1:500,])) {

  n[i] <- gDistance(spgeom1 = riverdepthmargin[i,], spgeom2 = snapPointsToThalweg[i,])
  cat(paste("\n Distance ", i, sep = ""))

}


#  ver acá cuando está a la derecha o a la izquierda, arriba o abajo para n < 0 o n 0

nMinus <- which((snapPointsToThalweg@coords[1:500, 1] - riverdepthmargin@coords[1:500, 1] ) < 0)
n[nMinus] <- n[nMinus] * -1

# Plot
plot(riverdepthmargin[1:500,])
plot(snapPointsToThalweg, col = "green", add = TRUE)
plot(mergePartsSMP_Line, col = "red", add = TRUE)

# Add n coordinates attribute to depth points
riverdepthmargin$n <- n

### Find s

# Function to calculate the distance between startpoint of a line and an intersecting point
lengthToPoint <- function(spatialLinesObj, intersectionPoint, bufferWidth) {

  intersectionPointBuffer = gBuffer(spgeom = intersectionPoint, width = bufferWidth)
  intersectionPointCoordinates = intersectionPoint@coords
  lineCoordinates = spatialLinesObj@lines[[1]]@Lines[[1]]@coords
  numOfCoordinates = length(lineCoordinates[,1])
  calculatedLength = 0

  # split line into segments
  for(i in 2:numOfCoordinates){

    # create new spatiallines for the current segment and check if point is intersecting
    currentLine = SpatialLines(LinesList = list(Lines(slinelist = list(Line(coords = lineCoordinates[(i-1):i,])), ID = "1")), spatialLinesObj@proj4string)

    # no intersection
    if(!gIntersects(currentLine, intersectionPointBuffer)) {
      calculatedLength = calculatedLength + gLength(spgeom = currentLine)

      # intersection
    } else {

      # create line from start of current segment to intersection point
      coordinates = matrix(data = c(lineCoordinates[i-1,], intersectionPointCoordinates), nrow = 2, byrow = T)
      lastLine = SpatialLines(LinesList = list(Lines(slinelist = list(Line(coords = coordinates)), ID = "1")), spatialLinesObj@proj4string)
      calculatedLength = calculatedLength + gLength(spgeom = lastLine)
      print(paste("Found point on line segment", (i-1), "! Length from start point: ", calculatedLength))
      return(calculatedLength)
    }
  }
  print("There was no intersection!")
}

s <- 0

for(i in 1:nrow(snapPointsToThalweg)) {

  s[i] <- lengthToPoint(spatialLinesObj = mergePartsSMP_Line, intersectionPoint = snapPointsToThalweg[i,], bufferWidth = 1)
  cat(paste("\n Distance ", i, sep = ""))

}


norm_vec <- function(x) sqrt(sum(x^2))

# Sort snapPointsToThalweg to twalweg line direction
OrderPointsAlongLine <- function(spatialLinesObject, spatialPointsObject) {

  lineCoords = as.data.frame(coordinates(spatialLinesObject))
  lineCoordsLength = nrow(lineCoords)

  # Make a buffer
  cat(paste("\n Making buffer...", sep = ""))
  spatialPointsObjectBuffer = gBuffer(spgeom = spatialPointsObject, width = 1, byid = TRUE)

  # Aux list and data frame
  cellsAndDistances = list()
  cd = data.frame('cell' = NA, 'dist' = NA)

  # Go sequentially through every line segment in the SpatialLine object looking for which SpatialPoints are intersected
  for(i in 1:(lineCoordsLength - 1)) {

    # Unommented: show iteration number
    cat(paste("\n iteration", i, sep = ""))

    # Line segment points
    p0 = lineCoords[i,]
    p1 = lineCoords[i+1,]

    # Build line segment
    SL = SpatialLines(LinesList = list(Lines(Line(coords = rbind(p0,p1)), ID = paste(i, sep = ""))), proj4string = CRS(epsg.32721))

    # Find intersected points with line segment
    intersect = which(gIntersects(spgeom1 = spatialPointsObjectBuffer, spgeom2 = SL, byid = TRUE))

    # Points intersected
    p2 = spatialPointsObject[intersect,]

    p2Length = nrow(p2@coords)

    # If there is almost one point intersected
    if(p2Length != 0) {

      # If there are more than one intersected points
      for(j in 1:p2Length) {

        # Commented: show sub-iteration number
        cat(paste("\n     sub-iteration", j, sep = ""))

        cd[j,] = data.frame('cell' = intersect[j], 'dist' = norm_vec(coordinates(p2)[j,] - p0))

      }
    }

    # Order by distance
    data.table::setorder(cd, dist)
    cellsAndDistances[[i]] = cd

  }

  # Bind list into one data farme
  cellsAndDistancesBind = do.call("rbind", cellsAndDistances)

  # Remove duplicated
  cellsAndDistancesBind <- cellsAndDistancesBind[-which(duplicated(cellsAndDistancesBind$cell)),]

  # Return
  return(cellsAndDistancesBind)

}

orderedPoints <- OrderPointsAlongLine(spatialPointsObject = snapPointsToThalweg, spatialLinesObject = mergePartsSMP_Line)

# Ordered points
snapPointsToThalweg <- snapPointsToThalweg[orderedPoints$cell,]

# Plot

# plot(mergePartsSMP_Line, col = "green")
# plot(snapPointsToThalweg, add = TRUE, pch = 19, cex = 0.5)
# text(coordinates(snapPointsToThalweg), labels = seq_along(snapPointsToThalweg), cex = 0.75, pos = 1)
# plot(snapPointsToThalweg[1,], add = TRUE, pch = 19, col = 'blue')
# plot(nrow(snapPointsToThalweg), add = TRUE, pch = 19, col = 'blue')

# SL <- SpatialLines(LinesList = list(Lines(slinelist = list(Line(coords = snapPointsToThalweg)), ID = "1")), proj4string = CRS(epsg.32721))
# plot(mergePartsSMP_Line, col = "green")
# plot(SL, col = "black", add = TRUE)

SPdf <- as.data.frame(coordinates(snapPointsToThalweg))
colnames(SPdf) <- c("x", "y")
SPdf <- cbind(SPdf, snapPointsToThalweg@data)

SLdf <- as.data.frame(coordinates(mergePartsSMP_Line))
colnames(SLdf) <- c("x", "y")
auxDF <- snapPointsToThalweg@data[1:nrow(SLdf),]
auxDF[1:nrow(SLdf),] <- NA
SLdf <- cbind(SLdf, auxDF)

rBindDFs <- rbind(SPdf, SLdf)

rBindDFsSP <- SpatialPointsDataFrame(coords = rBindDFs[,1:2], data = rBindDFs[,-(1:2)], proj4string = CRS(epsg.32721))

system.time(orderPoints2 <-  OrderPointsAlongLine(spatialPointsObject = rBindDFsSP, spatialLinesObject = mergePartsSMP_Line))

rBindDFsSP <- rBindDFsSP[orderPoints2$cell,]

# SL <- SpatialLines(LinesList = list(Lines(slinelist = list(Line(coords = rBindDFsSP)), ID = "1")), proj4string = CRS(epsg.32721))
# plot(mergePartsSMP_Line, col = "green")
# plot(SL, col = "black", add = TRUE)

s2 <- 0

for(i in 1:(nrow(rBindDFsSP) - 1)) {

  s2[i+1] <- s2[i] + gDistance(spgeom1 = rBindDFsSP[i,], spgeom2 = rBindDFsSP[i+1,])
  cat(paste("\n Distance ", i, sep = ""))

}

# check
tail(s2, 1)
LineLength(cc = Line(coordinates(rBindDFsSP)), sum = TRUE)

rBindDFsSP$s <- s2

# Remove NAs from spatial_id attribute (NAs are line coordinates and not depth positions)
rBindDFsSP <- rBindDFsSP[-which(is.na(rBindDFsSP$spatial_id)),]

merge <- merge(riverdepthmargin@data, rBindDFsSP@data , by = "spatial_id")
merge <- merge[,c(1:5,10)]
colnames(merge) <- c("spatial_id", "ping_date", "ping_time", "depth", "n", "s")

# Add s coordinates attribute to depth points
riverdepthmargin@data <- merge

plot(SpatialPoints(coords = cbind('x' = riverdepthmargin$s, 'y' = riverdepthmargin$n)))

# EIDW --------------------------------------------------------------------

# Elliptical Inverse Distance Weighted:

# Create a properly formatted CSV:
temporary_dir <- tempdir()
tempfname_base <- file.path(temporary_dir, "dem")
tempfname_csv <- paste(tempfname_base, ".csv", sep = "")

pts <- data.frame('Easting' = clippedSpObj$depthDataClipped@coords[,1], 'Northing' = clippedSpObj$depthDataClipped@coords[,2], 'Elevation' = -clippedSpObj$depthDataClipped$depth)

write.csv(pts, file = tempfname_csv, row.names = FALSE)

# Now make a matching VRT file
tempfname_vrt <- paste(tempfname_base, ".vrt", sep = "")

vrt_header <- c(
  '<OGRVRTDataSource>',
  '\t<OGRVRTLayer name="dem">',
  paste("\t<SrcDataSource>",tempfname_csv, "</SrcDataSource>", sep = ""),
  '\t<GeometryType>wkbPoint</GeometryType>',
  '\t<GeometryField encoding="PointFromColumns" x="Easting" y="Northing" z="Elevation"/>',
  '\t</OGRVRTLayer>',
  '\t</OGRVRTDataSource>'
)

vrt_filecon <- file(tempfname_vrt, "w")
writeLines(vrt_header, con = vrt_filecon)
close(vrt_filecon)

tempfname_tif <- paste(tempfname_base, ".tiff", sep = "")

# Interpolation algorithm and parameters
parameters <- "invdist:power=2.0:smoothing=1.0:radius1=38.39079:radius2=135.8657:angle=0"

# Now run gdal_grid:
rasterDepthEIDW1 <- gdal_grid(src_datasource = tempfname_vrt, dst_filename = tempfname_tif, of = "GTiff", ot = "Float64", txe = c(0, 180), tye = c(0, 2000), outsize = c(40,400), a = parameters, l = "dem", output_Raster = TRUE)
proj4string(rasterDepthEIDW1) <- CRS(epsg.32721)

spplot(rasterDepthEIDW1, xlim = c(0, 180))

# Raster to polygon and transform
Raster2PolyTransform <- function(rasterBrick, gridSn, grid, gridResolution) {

  gridSnExtractBati = extract(x = batiEIDW1, y = gridSn)

  gridSnDepth = gridSn[-which(is.na(gridSnExtractBati)),]

  gridSnDepth$depth = gridSnExtractBati[-which(is.na(gridSnExtractBati))]

  # Combinar capa de datos interpolados con capa grid original sin coordenadas transformadas
  gridSnDepth = merge(x = grid, y = gridSnDepth@data, by.x = "ID", by.y = "id")

  # Convert points centroids to polygon

  # set the radius (meters) for the plots
  radius = gridResolution/2

  #define the plot boundaries based upon the plot radius.
  #NOTE: this assumes that plots are oriented North and are not rotated.
  #If the plots are rotated, you'd need to do additional math to find
  #the corners.
  yPlus = gridSnDepth@coords[,2] + radius
  xPlus = gridSnDepth@coords[,1] + radius
  yMinus = gridSnDepth@coords[,2] - radius
  xMinus = gridSnDepth@coords[,1] - radius

  #Extract the plot ID information. NOTE: because we set
  #stringsAsFactor to false above, we can import the plot
  #ID's using the code below. If we didn't do that, our ID's would
  #come in as factors by default.
  #We'd thus have to use the code ID=as.character(centroids$Plot_ID)
  ID = as.character(gridSnDepth@data[,1])

  # Calculate polygon coordinates for each plot centroid.
  square = cbind(xMinus, yPlus, xPlus, yPlus, xPlus, yMinus, xMinus, yMinus, xMinus, yPlus, xMinus, yPlus)

  # Create spatial polygons
  polys = SpatialPolygons(mapply(function(poly, id) {

    xy = matrix(poly, ncol = 2, byrow = TRUE)
    Polygons(list(Polygon(xy)), ID = id)

  },

  split(square, row(square)), ID),
  proj4string = CRS(epsg.32721)

  )

  # Combine eidw data with polys
  polysOverDataEidw = over(x = polys, y = gridSnDepth)

  # recombine
  polysOverDataEidw = SpatialPolygonsDataFrame(Sr = polys, data = polysOverDataEidw, match.ID = TRUE)
  proj4string(polysOverDataEidw) = CRS(epsg.32721)

  polysOverDataEidw <- polysOverDataEidw[-which(is.na(polysOverDataEidw$depth)), ]

  return(polysOverDataEidw)
}

eidw1 <- Raster2PolyTransform(rasterBrick = rasterDepthEIDW1, gridSn = gridSn, grid = grid5m, gridResolution = 5)

writeOGR(obj = eidw1, dsn = "/path/to/translatedToSnCoords/eidw1/shapefile/", layer = "poly.eidw1", driver = "ESRI Shapefile", overwrite_layer = TRUE)
