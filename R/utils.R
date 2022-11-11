#' # Find Latest Version of NWM on NCEP
#' @return character
#' @export

latest_nwm_version = function(){
  ncep = 'https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/'
  ver = grep("nwm", readLines(ncep), value = TRUE)
  gsub('^.*href=\"\\s*|\\s*/.*$', '', ver)
}

#' @title Download National Water Model Domain Files
#' @description Download the domain files (CONUS) used in the most
#' current operational NWM. It will download
#' ~17GB of data so plan accordingly!
#' @details This function will download the follwing national (CONUS) domain
#'  files for the latest version of the National Water Model on NCEP.
#' \itemize{
##'  \item{"Fulldom_hires_netcdf_1km.nc"}
##'  \item{"geo_em.d01_1km.nc"}
##'  \item{"wrfinput_d01_1km.nc"}
##'  \item{"RouteLink_NHDPLUS.nc"}
##'  \item{"spatialweights_1km_all_basins.nc"}
##'  \item{"GWBUCKPARM_CONUS.nc"}
##'  \item{"soil_veg_properties_LongRange.nc"}
##'  \item{"HYDRO_TBL_2D.nc"}
##'  \item{"WRF_Hydro_NWM_geospatial_data_template_land_GIS.nc"}
##' }
##'
##' This function will NOT download:
##'\itemize{
##'  \item{"Fulldom_hires_netcdf_250m.nc"}("~13GB)
##'  \item{"GWBUCKPARM_CONUS_LongRange.nc"}("92B")
##'  \item{"HYDRO_TBL_2D_LongRange.nc"}{"338MB"}
##'  \item{"nudgingParams.nc"}{"2.4MB"}
##'  \item{"soil_veg_properties_ASM.nc"}{"3GB}
##'  \item{"soil_veg_properties_LongRange.nc"}{"3GB"}
##'  \item{"spatialweights_250m_all_basins.nc"}{"4.5GB"}
##' }
#' @param outDir where to write the files. Again ~17GB worth!
#' @return
#' @export

download_conus_nwm = function(outDir = NULL){

  nwmDir = paste0('https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/',
                  latest_nwm_version(),
                  '/parm/domain')

  needed_files = c("Fulldom_hires_netcdf_1km.nc",
                 "geo_em.d01_1km.nc",
                 "wrfinput_d01_1km.nc",
                 "RouteLink_NHDPLUS.nc",
                 "spatialweights_1km_all_basins.nc",
                 "GWBUCKPARM_CONUS.nc",
                 "soil_veg_properties_LongRange.nc",
                 "HYDRO_TBL_2D.nc",
                 "WRF_Hydro_NWM_geospatial_data_template_land_GIS.nc")

  localDir = paste0(outDir, "/nwmCONUS-", gsub("nwm", "", gsub("[.]", "",ver)))
  fs::dir_create(localDir)

  urls  = paste(nwmDir, needed_files, sep = "/")
  local = paste(localDir, needed_files, sep = "/")

  for( i in 1:length(urls)){
    if(!file.exists(local[i])){
      message('Downloading: ', basename(local[i]))
      httr::GET(urls[i], httr::write_disk(local[i]), httr::progress())
    } else {
      message(basename(local[i]),   ' already exists')
    }
  }

    message("\n\nAll files located: ", localDir)
}

#' Update a route link netcdf file based on an R dataframe.
#' @description See rwrfhydro::UpdateLinkFile
#' @details This function is a direct copy for rwrfhydro expect that the
#' diminison passed to ncks had to be changed from linkDim o feature_id to work.
#' @param linkFile 	Path to new route link file to update. This should be a
#' copy of an existing route link file, as it will be overwritten
#' @param linkDf    Dataframe of the new route link information
#' to write to the new file
#' @param subDim    Boolean whether the dimensions in the new file
#' need to be subsetted to match the new dataframe (DEFAULT=TRUE)
#' @return  NULL
#' @export
#'
#' @examples

ULF = function (linkFile, linkDf, subDim = TRUE)
{
  if (subDim) {
    cmdtxt <- paste0("ncks -O -d feature_id,1,", nrow(linkDf),
                     " ", linkFile, " ", linkFile)
    print(cmdtxt)
    system(cmdtxt)
  }
  ncid <- nc_open(linkFile, write = TRUE)
  for (i in names(ncid$var)) {
    print(i)
    if (i %in% names(linkDf))
      ncvar_put(ncid, i, linkDf[, i])
  }
  nc_close(ncid)
  return()
}

#' Get GeoGrid Projection
#' @description From a geogrid file, extract the proj4string.
#' @details Extracts projection data from global attributes
#' @param path a path to the local geogrid.nc file
#' @return
#' @export

geo_grid_proj = function(path)
{
  nc = RNetCDF::open.nc(path)
  map_proj <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "MAP_PROJ")
  cen_lat  <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "CEN_LAT")
  cen_lon  <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "STAND_LON")
  truelat1 <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "TRUELAT1")
  truelat2 <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "TRUELAT2")
  if (map_proj == 1) {
    crs.here <- paste0("+proj=lcc +lat_1=", truelat1,
                       " +lat_2=", truelat2,
                       " +lat_0=", cen_lat,
                       " +lon_0=", cen_lon,
                       " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
  }
  return(crs.here)
}


#' Return GeoGrid Raster Structure
#' @description Given a geogrid path, a raster object is returned with the
#' approariate structure,
#' but no values.
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples


make_empty_geogrid_raster = function(path, var = NULL)
{
  crs.here = geo_grid_proj(path)
  nc = RNetCDF::open.nc(path)
  x = RNetCDF::dim.inq.nc(nc, 'west_east')$length
  y = RNetCDF::dim.inq.nc(nc, 'south_north')$length
  dx = RNetCDF::att.get.nc(nc, "NC_GLOBAL", "DX")
  dy = RNetCDF::att.get.nc(nc, "NC_GLOBAL", "DY")
  lat_max = RNetCDF::var.get.nc(nc, "XLAT_M", start = c(1,y,1), count = c(1,1,1))
  lng_max = RNetCDF::var.get.nc(nc, "XLONG_M", start = c(1,y,1), count = c(1,1,1))

  pts = st_sfc(st_point(c(lng_max,lat_max)), crs = 4326) %>%
    st_transform(crs.here) %>%
    st_coordinates()

  xmin = pts[1] - res[1]/2
  ymax = pts[2] + res[2]/2
  xmax = xmin + x*dx
  ymin = ymax - y*dy

  r = raster(res = c(dx,dy), xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, crs = crs.here)


  if(!is.null(var)){
  var = RNetCDF::var.get.nc(nc, var)
  values(r) = apply(t(var),2,rev)
  }

  r
}

dim(lat)
