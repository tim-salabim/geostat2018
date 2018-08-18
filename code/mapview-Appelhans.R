library("sf")
library("mapview")
library("leaflet")
library("RColorBrewer")

# vwr = options("viewer")
# options(viewer = NULL)

### basic usage ===========================================================

### most basic call
mapview()

### with some data
m = mapview(breweries)
m

# mapview structure
str(m, 3)
m@object[[1]]
class(m@map)

mapview(trails)
mapview(franconia)
mapview(poppendorf[[5]])

### styling options & legends
mapview(franconia, color = "white", col.regions = "red")
mapview(breweries, color = "grey95", col.regions = "white")

mapview(breweries, zcol = "founded", layer.name = "Year of foundation")
mapview(breweries, zcol = "founded", at = seq(1300, 2200, 200))
mapview(franconia, zcol = "district", legend = FALSE, label = NULL, highlight = NULL)

clrs = colorRampPalette(brewer.pal(3, "Set1"))
mapview(franconia, zcol = "district", col.regions = clrs, alpha.regions = 1)

### multiple layers
mapview(breweries) + franconia
mapview(breweries, col.regions = "red") +
  mapview(franconia, col.regions = "grey") +
  trails
mapview(list(breweries, franconia, trails))

mapview(list(breweries, franconia),
        zcol = list("founded", "district"),
        legend = list(FALSE, TRUE),
        homebutton = list(TRUE, FALSE)) +
  trails

### burst
mapview(franconia, burst = TRUE)
mapview(franconia, burst = TRUE, hide = TRUE)
mapview(franconia, zcol = "district", burst = TRUE)




### view extents
viewExtent(breweries)
mapview(st_bbox(breweries))
viewExtent(breweries) + mapview(breweries, zcol = "founded")
viewExtent(poppendorf) + poppendorf[[5]]

### basemaps
mapview(gadmCHE, map.types = c("Stamen.Toner", "NASAGIBS.ViirsEarthAtNight2012"))

# see https://leaflet-extras.github.io/leaflet-providers/preview/ for more options

### popups ================================================================
### popupTable (the default for mapview - also works with leaflet)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = breweries,
                   popup = popupTable(breweries))

mapview(breweries, popup = popupTable(breweries,
                                      zcol = c("brewery",
                                               "village",
                                               "founded")))

### popuGraph (static)
library(lattice)
library(sp)

data(meuse)
coordinates(meuse) = ~x+y
proj4string(meuse) = CRS("+init=epsg:28992")

p = xyplot(copper ~ cadmium, data = meuse@data, col = "grey", pch = 20, cex = 2)
p = mget(rep("p", length(meuse)))

clr = rep("grey", length(meuse))
p = lapply(1:length(p), function(i) {
  clr[i] = "red"
  update(p[[i]], col = clr)
})

p[[1]]

mapview(meuse,
        zcol = "cadmium",
        popup = popupGraph(p, type = "png"))

# here, burst by row acturally is more sensible
mapview(meuse, burst = "soil")

### popupGraph (interactive)
brew1 = breweries[1, ]
pop_content = mapview(brew1, map.types = "Esri.WorldImagery")@map

pop_content

mapview(brew1,
        popup =  popupGraph(pop_content,
                            type = "html",
                            width = 400,
                            height = 400))

### popupImage (works for local or remote images for most common file types)
pnt = data.frame(x = 174.764474, y = -36.877245)
pnt = st_as_sf(pnt, coords = c("x", "y"), crs = 4326)

img = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Mount_Eden.jpg/640px-Mount_Eden.jpg"

mapview(pnt, map.types = "Esri.WorldImagery",
        popup = popupImage(img, src = "remote", width = 600))

### small multiples and map syncing ============================================
m1 = mapview(breweries, zcol = "village", map.types = "Esri.WorldImagery")
m2 = mapview(franconia, zcol = "district", col.regions = clrs)
m3 = mapview(breweries, zcol = "founded", legend = TRUE)
m4 = leaflet() %>% addTiles() %>% addCircleMarkers(data = breweries)

sync(m1, m2, m3, m4)

sync(list(m1, m2, m3, m4),
     sync = list(c(1, 4),
                 c(2, 3)))

sync(list(m1, m2, m3, m4),
     sync = list(c(1, 2),
                 c(3, 4)),
     ncol = 1)

latticeView(m1, m3)


### raster specific functions ==============================================
### viewRGB - view RGB composites of any combination of raster layers
viewRGB(poppendorf)
raster::nlayers(poppendorf)
viewRGB(poppendorf, 4, 3, 2)
viewRGB(poppendorf, 5, 4, 3)
viewRGB(poppendorf, 5, 4, 3, quantiles = c(0.75, 1))

### plainview
plainview(poppendorf[[1]])
plainview(poppendorf[[1]], at = seq(8000, 15000, 500))
plainview(poppendorf, 4, 3, 2)
plainview(poppendorf, 5, 4, 3, quantiles = c(0.5, 1))

 ### slideview
slideview(poppendorf[[1]], poppendorf[[5]])
slideview(poppendorf[[1]], poppendorf[[5]], legend = FALSE)

### cubeview
library(raster)
kili_data = system.file("extdata", "kiliNDVI.tif", package = "mapview")
kiliNDVI = stack(kili_data)

cubeView(kiliNDVI)

clr = viridisLite::viridis
cubeView(kiliNDVI, at = seq(-0.15, 1.2, 0.2), col.regions = clr)



### changing options globally =============================================
mapviewOptions(basemaps = c("CartoDB.DarkMatter", "Esri.OceanBasemap"),
               raster.palette = colorRampPalette(brewer.pal(9, "Greys")),
               vector.palette = colorRampPalette(brewer.pal(9, "YlGnBu")),
               na.color = "magenta",
               layers.control.pos = "topright")

mapview(breweries, zcol = "founded")
mapview(poppendorf[[5]])

mapviewOptions()
mapviewOptions(default = TRUE)

mapview(poppendorf[[5]])


### extra functionality and leaflet integration ================================
# leaflet integration
leaflet() %>%
  addMouseCoordinates()

mapview(breweries)@map %>% addPolygons(data = franconia)

# addLogo
logo = "https://www.r-project.org/logo/Rlogo.svg"

leaflet() %>%
  addTiles() %>%
  addLogo(logo, url = "http://www.r-project.org", alpha = 0.7)


### mapview scalability ===================================================
### mapview scales to biggish data sets
vwr = options("viewer")
options(viewer = NULL)

lu = st_read("/home/timpanse/tappelhans/talks/geostat2018/data/landuse.gpkg")

# mapview(lu_rgdal)
mapview(lu, zcol = "type", legend = FALSE)

dim(lu)
npts(lu)

options(viewer = vwr$viewer)

### dplyr integration =====================================================
library(dplyr)

franconia %>%
  st_union() %>%
  mapview()

franconia %>%
  mutate(n_brew = lengths(st_contains(., breweries)),
         area = st_area(.),
         brew_density = as.numeric(n_brew / area)) %>%
  mapview(zcol = "brew_density", legend = FALSE) +
  breweries

trails %>%
  mutate(length = st_length(.)) %>%
  mapview(zcol = "length", lwd = 4)

franconia %>%
  st_centroid() %>%
  mapview(cex = 5, col.regions = "black") +
  franconia

mapview(trails %>%
          mutate(orig_id = 1:nrow(trails)) %>%
          st_cast(., "LINESTRING") %>%
          mutate(length = st_length(.),
                 test = "blablabla"),
        zcol = "length", lwd = 3)

### saving maps ============================================================
m = mapview(franconia, zcol = "district") + breweries

mapshot(m, url = "/home/timpanse/Desktop/mymap.html")
mapshot(m, file = "/home/timpanse/Desktop/mymap.png")
