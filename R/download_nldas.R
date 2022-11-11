library(httr);library(dplyr)
startDate = "2000-01-01 00:00:00"
endDate   = "2001-12-31 23:00:00"
destdir   = '/Volumes/Transcend/testNLDAS/'



df = data.frame(date = seq.POSIXt(as.POSIXct(startDate),
                                  as.POSIXct(endDate), by = 'h')) %>%
  mutate(jul = format(date, "%j"),
         year = format(date, "%Y"),
         day = format(date, "%d"),
         month = format(date, "%m"),
         hour = format(date, "%H")) %>%
  mutate(urls = paste0('https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_H.002/',
                        year,
                        "/",
                        jul,
                        '/NLDAS_FORA0125_H.A',
                        year,
                        month,
                        day,
                        ".",
                        hour,
                        '00.002.grb'))

message(nrow(df), " files requested")

head(df)
for(i in 1:nrow(df)){

  dir = paste(destdir, df$year[i], df$month[i], df$day[i], sep = "/")

  fs::dir_create(dir, recurse = TRUE)

  here = paste0(dir, "/", basename(df$urls[i]))

  if(!file.exists(here)){
    httr::GET(df$urls[i], write_disk(here, overwrite = TRUE),
              config(netrc = TRUE, netrc_file = '/Users/mikejohnson/.netrc'),
              set_cookies("LC" = "cookies"))
  }

  message(i)
}



system(paste0("ncl 'srcFileName=\"NLDAS_FORA0125_H.*\"' 'dstGridName=\"geo_em.d01.nc\"' NLDAS2WRFHydro_regrid.ncl"))
system('source ~/.bash_profile && conda activate ncl_stable')
system('ncl -V')
