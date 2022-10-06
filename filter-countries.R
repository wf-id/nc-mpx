library(tidyverse)
if (!file.exists("counties-10m.json")) {
  download.file(
    url = "https://cdn.jsdelivr.net/npm/us-atlas@3/counties-10m.json", 
    destfile = "counties-10m.json",
    quiet = TRUE
  )
}

a <- sf::read_sf("counties-10m.json")
nc <- a[grepl(a$id, pattern = "37\\d{3}"),]

sf::st_write(
  obj = nc, 
  dsn = "nc.geojson",
  quiet = TRUE
)
unlink("counties-10m.json")
