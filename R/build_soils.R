library(RNetCDF)

nc = open.nc('/Volumes/Transcend/geo_em.d01_1km.nc')
var = var.get.nc(nc, "SOILCTOP")

rwrfhydro::ExportGeogrid('/Volumes/Transcend/geo_em.d01_1km.nc', 'HGT_M', '/Volumes/Transcend/hgt.tif')

r = raster('/Volumes/Transcend/hgt.tif')
plot(r)
ind = var[,,1]
4608 3840

start = matrix(0, nrow = 4608,3840)

for(i in 1:16){
  tmp = var[,,i]
  tmp[tmp != 0] = i
  start  = start + tmp
}

o = raster(apply(t(start),2,rev))
extent(o) = extent(r)
crs(o) = crs(r)
writeRaster(o, "/Volumes/Transcend/soils.tif")
plot(o)


as.vector(start) %>% unique
