library(rvest)
if(!dir.exists("data")) dir.create("data")

slugify_date <- function(x){
  x <- stringi::stri_replace_all_regex(x,"[^\\P{P}-]","")
  x <- gsub(x, pattern = " ", replacement = "-")
  x
}

ping_time <- slugify_date(Sys.time())

url <- "https://epi.dph.ncdhhs.gov/cd/diseases/monkeypox.html"

ses <- session(url)

html_text <- ses %>% 
  read_html()

cases <- html_text %>% 
  html_nodes(xpath = '//*[@id="blueWrapper"]/div[2]/div[3]') %>% 
  html_text()

date <- html_text %>% 
  html_nodes(xpath = '//*[@id="blueWrapper"]/div[2]') %>% 
  html_text()

dat <- stringr::str_remove(unlist(strsplit(date,split = "\r\n"))[5], "\\s+")


# write outputs -----------------------------------------------------------

dat_out <- data.frame(date = dat, cases = cases, ping_time = ping_time)

data.table::fwrite(dat_out, here::here("data", "mpx.csv"), append = TRUE)