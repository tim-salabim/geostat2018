library("raster")
library("Rsenal")
library("latticeExtra")
library("fields")
library("viridisLite")

# downloaded from
url = "https://data.nodc.noaa.gov/nodc/archive/data/0077816/2009/geotiffs/2009_Pathv5.0_daynight_SST_meanc.tif"
tmpfl = tempfile(fileext = ".tif")
download.file(url, tmpfl)
wrld = raster(tmpfl)
wrld_smll = crop(wrld, extent(c(-180, 180, -80, 80)))

# define some of the palettes
clrs_spec = colorRampPalette(rev(brewer.pal(11, "Spectral")))
clrs_hcl = function(n) {
  hcl(h = seq(230, 0, length.out = n),
      c = 60, l = seq(10, 90, length.out = n),
      fixup = TRUE)
}

# helper function to plot land mask (incl. country borders)
panel.map = function(map.data, col = "transparent", ...) {
  mm = maps::map(map.data, plot = FALSE, fill = TRUE, col = col)
  pp = panel.polygon(mm$x, mm$y, col = col, ...)
  return(pp)
}


pals = c("clrs_hcl", "clrs_spec", "tim.colors", "viridis", "inferno", "plasma")


for (i in pals) {
  pal = match.fun(i)
  p = spplot(wrld_smll, col.regions = pal(1000), at = seq(-3, 33, 0.1)) +
    layer(panel.map("world", col = "grey30", fill = TRUE))

  fl_nm = paste0("/home/timpanse/tappelhans/talks/geostat2018/figures/wrld_sst_",
                 i, ".png")
  png(fl_nm, width = 30, height = 23, units = "cm", res = 300)
  print(p)
  dev.off()
}
