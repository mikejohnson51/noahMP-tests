pacman::p_load(sf, dplyr)

pip = function(points, polygon, var){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    na.omit() %>%
    count(get(var)) %>%
    setNames(c(var, "n")) %>%
    right_join(polygon, by = var) %>%
    st_as_sf()
}


gages  = readRDS("data/usgs_rc.rds") %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(5070) %>%
  filter(startDate <= "2001-01-01", endDate >= "2018-01-01") %>%
  left_join(nhdplusTools::get_vaa(c("totdasqkm")), by = c("COMID" = "comid")) %>%
  filter(between(totdasqkm, 400, 1000))

basins = list()

for(i in 1:nrow(gages)){
  out = dataRetrieval::findNLDI(comid = gages$COMID[i],
                          nav   = "UT",
                          find = c("basin", "nwis"))
  out = lapply(out, sf::st_as_sf)
  bas = out$basin
  bas$comid = gages$COMID[i]
  bas$siteID = gages$siteID[i]
  bas$DA = gages$totdasqkm[i]
  bas$gages = nrow(out$UT_nwissite)
  basins[[i]] = bas
  message(i)
}

bas2 = dplyr::bind_rows(basins) %>%
  mutate(area  = st_area(.)/1e6) %>%
  mutate(gages = gages - 1) %>%
  filter(gages > 0) %>%
  st_transform(5070)

niddata = read_sf('https://nid.usace.army.mil/api/nation/gpkg')

niddata2 = niddata %>%
  select(nidStorage, nidHeight) %>%
  filter(nidStorage >= 50 | nidStorage > 10000) %>%
  st_transform(st_crs(bas2))

bas2 = pip(niddata2, bas2, "comid") %>%
  rename(dams = n) %>%
  mutate(dams = ifelse(is.na(dams), 0, dams)) %>%
  filter(dams == 0)

nlcd = terra::rast('/Volumes/Backup/NLCD/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img')
tmp2 = exactextractr::exact_extract(nlcd,
                                    bas2,
                                    function(values, coverage_fraction) table(substr(values,1,1)))

out = bind_rows(tmp2)
tt = 100* out / rowSums(out, na.rm = TRUE)
ttt = select(tt, ends_with('Freq'))
ttt[is.na(ttt)] <- 0
ttt = round(ttt,2)
ttt = setNames(ttt, c("water", "urban", "barren", "forest",
                      "shrub", "grassland", "agriculture", "wetland"))

fin = bind_cols(bas2, ttt) %>%
  filter(water <= 5, barren < 5, wetland < 5)

fin$lc10    = rowSums(st_drop_geometry(fin[,7:14]) > 10, na.rm = T)
fin$lcMax   = apply(st_drop_geometry(fin[,8:16]), 1, function(x) max(x))

f2 = filter(fin, lc10 >= 3, lcMax < 55)
f2$class = NA

for(i in 1:nrow(f2)){
  f2$class[i] = paste(
    sort(names(f2)[order(st_drop_geometry(f2[i,8:16]), decreasing = TRUE)[1:3] + 7]),
    collapse = "-")
}


