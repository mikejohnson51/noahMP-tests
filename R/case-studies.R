library(raster)
library(dplyr)
library(sf)

####
pip = function(points, polygon, var){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    na.omit() %>%
    count(get(var)) %>%
    setNames(c(var, "n")) %>%
    right_join(polygon, by = var) %>%
    st_as_sf()
}

nlcd = raster::raster('/Volumes/Backup/NLCD/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img')
niddata = readxl::read_xlsx("/Users/mikejohnson/github/dams/data/NID2019.xlsx")

####
all_huc10 = st_read('/Volumes/Transcend/WBD_National_GDB/WBD_National_GDB.gdb', layer  = 'WBDHU10')

huc10 = filter(all_huc10, areasqkm >= 400, areasqkm <= 1000) %>%
  filter(hutype %in% c("C", "S")) %>%
  select(huc10, name, hutype, states, areasqkm) %>%
  st_transform(5070) %>%
  mutate(compact = as.numeric(st_area(fin) / st_area(st_convex_hull(fin)))) %>%
  filter(compact <= 0.5)

gages  = readRDS("/Users/mikejohnson/src-improvements/data/usgs_rc.rds") %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(st_crs(huc10)) %>%
  filter(startDate <= "2001-01-01", endDate >= "2018-01-01")

huc10 = pip(gages, huc10, "huc10") %>%
  rename(gages = n) %>%
  mutate(gages = ifelse(is.na(gages), 0, gages)) %>%
  filter(gages >= 2)

# F “Frontal” hydrologic unit—An area along the coastline of
# a lake, ocean, bay, etc., that has more than one outlet. These
# hydrologic units are predominantly land with some water areas
# at or near the outlet(s) (section 3.5.2).

# http://water.nv.gov/DamGuidelinesSize.aspx

niddata2 = niddata %>% select(NID_STORAGE, NID_HEIGHT, LONGITUDE, LATITUDE) %>%
  filter(NID_HEIGHT >= 50 | NID_STORAGE > 10000) %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4236) %>%
  st_transform(st_crs(huc10))

huc10 = pip(niddata2, huc10, "huc10") %>%
  rename(dams = n) %>%
  mutate(dams = ifelse(is.na(dams), 0, dams)) %>%
  filter(dams == 0)

tmp2 = exactextractr::exact_extract(nlcd, huc10, function(values, coverage_fraction) table(substr(values,1,1)))

out = dplyr::bind_rows(tmp2)
tt = 100* out / rowSums(out, na.rm = TRUE)
ttt = select(tt, ends_with('Freq'))
ttt[is.na(ttt)] <- 0
ttt = round(ttt,2)

ttt = setNames(ttt, c("water", "urban", "barren", "forest",
                      "shrub", "grassland", "agriculture", "wetland", "missing"))

fin = bind_cols(huc10, ttt)
fin = fin %>% filter(missing <= 1, water <= 5, barren < 5, wetland < 5)

fin$lc10    = rowSums(st_drop_geometry(fin[,8:16]) > 10, na.rm = T)
fin$lcMax   = apply(st_drop_geometry(fin[,8:16]), 1, function(x) max(x))

f2 = filter(fin, lc10 >= 3, lcMax < 50)
f2$class = NA

for(i in 1:nrow(f2)){
  f2$class[i] = paste(
    sort(names(f2)[order(st_drop_geometry(f2[i,8:16]), decreasing = TRUE)[1:3] + 7]),
    collapse = "-")
}

f2 = arrange(f2, class, -compact)
table(f2$class)
mapview::mapview(f2, zcol = "class", col.regions = RColorBrewer::brewer.pal(8, "Dark2"))

write.csv(st_drop_geometry(f2), file = "data/test-basins.csv")


