library(latticeExtra)
library(colorspace)
library(RColorBrewer)
library(viridis)
library(grid)
library(Orcs)
library(sf)
library(fields)

### function to plot a colour palette
pal = function(col, border = "transparent", name = "") {

  n = length(col)
  mat = matrix(1:(n-1), nrow = n-1)
  levelplot(mat, col.regions = col, scales = list(draw = FALSE),
            ylab = "", xlab = "", colorkey = FALSE,
            at = seq(n), asp = 0.2,
            panel = function(...) {
              panel.levelplot(...)
              grid.text(label = name, x = 0.95, y = 0.9,
                        just = c("right", "top"))
            })
}


clrs_spec = colorRampPalette(rev(brewer.pal(11, "Spectral")))
clrs_hcl = function(n) {
  hcl(h = seq(230, 0, length.out = n),
      c = 60, l = seq(15, 95, length.out = n),
      fixup = TRUE)
}

clrs = list(
  rainbow = rainbow(100),
  rainbow_grey = desaturate(rainbow(100)),
  tim.colors = tim.colors(100),
  tim.colors_grey = desaturate(tim.colors(100)),
  Spectral = clrs_spec(100),
  Spectral_grey = desaturate(clrs_spec(100)),
  hcl = clrs_hcl(100),
  hcl_grey = desaturate(clrs_hcl(100)),
  viridis = viridis(100),
  viridis_grey = desaturate(viridis(100)),
  inferno = inferno(100),
  inferno_grey = desaturate(inferno(100)),
  sf.colors = sf.colors(100),
  sf.colors_grey = desaturate(sf.colors(100))
)

out = lapply(seq(clrs), function(i) {
  pal(clrs[[i]], name = names(clrs)[i])
})

png("/home/timpanse/tappelhans/talks/geostat2018/figures/color_grid.png",
    width = 20, height = 15, units = "cm", res = 300)
print(Orcs::latticeCombineGrid(out))
dev.off()



