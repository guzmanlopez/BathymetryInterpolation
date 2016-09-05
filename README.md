# River bathymetry interpolation

This `R` script is a workflow to perform a river bathymetry interpolation following [Merwade et al. 2006](http://www.sciencedirect.com/science/article/pii/S0022169406003313 "ScienceDirect link to Merwade et al. 2006") work.

---
### Script under development!

![This script is under development!][underdevelopment]

[underdevelopment]: http://cdn.mysitemyway.com/etc-mysitemyway/icons/legacy-previews/icons-256/yellow-road-sign-icons-people-things/067798-yellow-road-sign-icon-people-things-people-worker.png "Under development"

---

This script uses the following `R` packages:

```r
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
```
