g = readRDS("/Users/mikejohnson/src-improvements/data/usgs_rc.rds") %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(st_crs(nlcd)) %>%
  filter(startDate <= "2001-01-01", endDate >= "2018-01-01")


col_lu <- data.frame(
  nlcd.code = c(-.1, 0, 11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95),
  nwm.code  = c(-.1, 0, 16, 23, NA, 1,  NA, NA, 19, 11, 14, 15, 22, 8,  7,  20, NA, NA, 2,  3,  18, 17),

  color = c("#000000",
            "#476BA0", "#D1DDF9",
            "#DDC9C9", "#D89382", "#ED0000", "#AA0000",
            "#B2ADA3",
            "#68AA63", "#1C6330", "#B5C98E",
            "#A58C30", "#CCBA7C",
            "#E2E2C1", "#C9C977", "#99C147", "#77AD93",
            "#DBD83D", "#AA7028",
            "#BAD8EA", "#70A3BA", NA) ,

  name = c(NA, "EMPTY", "Open Water", "Ice/Snow", "Developed (Open)", "Developed (Low)", 'Developed (Medium)', 'Developed (High)', "Barren",
           "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Dwarf Scrub", "Shurb", "Grassland", "Sedge", 'Lichens', "Moss",
           "Pasture", "Culitivated Crops", "Woody Wetlands", "Herbaceous Wetlands"),

  stringsAsFactors = FALSE)

library(resample)


rains = paste0('/Volumes/Transcend/PRISM_ppt_30yr_normal_sum_5070_q', 1:4, ".tif") %>% stack()
ll = list()

for(i in 1:nrow(f2)){
  go = f2[i,] %>% st_transform(st_crs(nlcd))

  output = prep_output(go)
  input = crop(nlcd, output, snap = "out")
  n = resampleData(input, output, 0, method = 'nn')
  a = resampleData(input, output, 0, method = 'area')
  ra = resampleData(input, output, 0, method = 'rawarea')
  m = resampleData(input, output, 0, method = 'maj')

s2 = mask(stack(a,m), go, snap = "out") %>%
  values() %>%
  na.omit()

ll[[i]] = data.frame(comid = go$comid, area_maj  = 100 * sum(s2[,1] != s2[,2]) / nrow(s2))

rainfall = crop(rains, output, snap = "out")

gages.here = st_intersection(g, go)

png(file = paste0("img3/", f2$class[i], "_comid-" , f2$comid[i], ".png"), width = 2400, height = 1800)
  par(mfrow = c(3,5), mar = c(1.5,1.5,1.5,1.5))
  image(input,breaks = c(col_lu$nlcd.code, 1000), col = col_lu$color, main = "NLCD", cex.main = 3, asp = 1, axes = F)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  plot(gages.here$geometry, pch = 25, cex = 2, add = TRUE)
  image(n,  breaks = c(col_lu$nlcd.code, 1000), col = col_lu$color, main = "NN", cex.main = 3, axes = F, asp = 1)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  plot(gages.here$geometry, pch = 25, cex = 2, add = TRUE)
  image(m,  breaks = c(col_lu$nlcd.code, 1000), col = col_lu$color, main = "MAJ", cex.main = 3 , axes = F, asp = 1)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  plot(gages.here$geometry, pch = 25, cex = 2, add = TRUE)
  image(a,  breaks = c(col_lu$nlcd.code, 1000), col = col_lu$color, main = "PPA", cex.main = 3 , axes = F, asp = 1)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  plot(gages.here$geometry, pch = 25, cex = 2, add = TRUE)
  image(ra,  breaks = c(col_lu$nlcd.code, 1000), col = col_lu$color, main = "RA", cex.main = 3 , axes = F, asp = 1)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  plot(gages.here$geometry, pch = 25, cex = 2, add = TRUE)

  image(rainfall[[1]], col = blues9, main = "Q1", cex.main = 3 , axes = F, asp = 1)
  plot(rainfall[[1]], legend.only=TRUE, col = blues9,horizontal = TRUE,
       axis.args=list(cex.axis=3), legend.width = 2)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  image(rainfall[[2]], col = blues9, main = "Q2", cex.main = 3 , axes = F, asp = 1)
  plot(rainfall[[2]], legend.only=TRUE, col = blues9,horizontal = TRUE,
       axis.args=list(cex.axis=3), legend.width = 2)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  image(rainfall[[3]], col = blues9, main = "Q3", cex.main = 3 , axes = F, asp = 1)
  plot(rainfall[[3]], legend.only=TRUE, col = blues9,horizontal = TRUE,
       axis.args=list(cex.axis=3), legend.width = 2)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  image(rainfall[[4]], col = blues9, main = "Q4", cex.main = 3 , axes = F, asp = 1)
  plot(rainfall[[4]], legend.only=TRUE, col = blues9,horizontal = TRUE,
       axis.args=list(cex.axis=3), legend.width = 2)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)
  srf = sum(rainfall)
  image(srf, col = blues9, main = "Annual", cex.main = 3 , axes = F, asp = 1)
  plot(srf, legend.only=TRUE, col = blues9,horizontal = TRUE,
       axis.args=list(cex.axis=3), legend.width = 2)
  plot(go$geometry, col = NA, add = TRUE, lwd = 5)

  x = crop(r, st_transform(go, st_crs(r)))
  image(crop(o, st_transform(go, st_crs(o))), main = "Soils", cex.main = 3 , axes = F, asp = 1)
  plot(st_transform(go, st_crs(o))$geometry, col = NA, add = TRUE, lwd = 5)
  image(x, main = "Elevation", col = terrain.colors(10), cex.main = 3 , axes = F, asp = 1)
  plot(x, legend.only=TRUE,horizontal = TRUE, col = terrain.colors(10),
       axis.args=list(cex.axis=3), legend.width = 2)
  plot(st_transform(go, st_crs(o))$geometry, col = NA, add = TRUE, lwd = 5)

  dev.off()



}

g2 = bind_rows(ll)

tt = left_join(f2, g2)
tt = tt %>%
  arrange(class, -area_maj) %>%
  st_drop_geometry() %>%
  mutate(urban <= 5, area_maj = round(area_maj, 2))

write.csv(tt, file = "data/test-basins-4.csv")
mike = filter(f2, comid %in% c(23762661, 1631587, 5894384, 19389766, 191739,14552525,
23988192, 5781369))
mapview::mapview(mike)
