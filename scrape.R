library(rvest)
if(!dir.exists("data")) dir.create("data")

slugify_date <- function(x){
  x <- stringi::stri_replace_all_regex(x,"[^\\P{P}-]","")
  x <- gsub(x, pattern = " ", replacement = "-")
  x
}

# On 10 August 2022, NCDHHS went from "https://epi.dph.ncdhhs.gov/cd/diseases/monkeypox.html"

ping_time <- slugify_date(Sys.time())

url <- "https://www.ncdhhs.gov/divisions/public-health/monkeypox"

ses <- session(url)

html_text <- ses %>% 
  read_html()

cases <- html_text %>% 
  #html_nodes(xpath = '//*[@id="block-block-block-nc-base-theme-nc-site-child-theme-system-main"]/div/article/div/div[2]/div/div[1]/div/section/section/div[1]/div/div/div/div/div/div/span') %>% 
  #html_nodes(xpath = '//*[@id="block-block-block-nc-base-theme-nc-site-child-theme-system-main"]/div/article/div/div[2]/div/div[2]/div/section/section/div[1]/div/div/div/div/div/div')  |>
html_nodes(xpath = '//*[@id="content"]/section/article/div/div/div[2]/div/section/section/div[1]/div/div[2]/div/div/strong') |>
html_text() |> 
  stringr::str_extract(pattern = "\\d+") |> 
  as.integer()

cat("Cases posted: ", cases, "\n")

date <- html_text %>% 
  #html_nodes(xpath = '//*[@id="block-block-block-nc-base-theme-nc-site-child-theme-system-main"]/div/article/div/div[2]/div/div[1]/div/section/section/div[1]/div/div/div/div/div/div/p') %>% 
  #html_node(xpath = '//*[@id="block-block-block-nc-base-theme-nc-site-child-theme-system-main"]/div/article/div/div[2]/div/div[2]/div/section/section/div[1]/div/div/div/div/div/div/p') |>
html_nodes(xpath = '//*[@id="content"]/section/article/div/div/div[2]/div/section/section/div[1]/div/div[2]/div/div/p[2]') |>  
html_text()

dat <- stringr::str_remove(unlist(strsplit(date,split = "\r\n"))[1], "\\s+")

vaccines <- html_text %>%
    #html_nodes(xpath = '//*[@id="block-block-block-nc-base-theme-nc-site-child-theme-system-main"]/div/article/div/div[2]/div/div[2]/div/section/section/div[3]/div/div/div/div/div/div/span/strong') %>%
    #html_nodes(xpath = '//*[@id="block-block-block-nc-base-theme-nc-site-child-theme-system-main"]/div/article/div/div[2]/div/div[3]/div/section/section/div[3]/div/div/div/div/div/div/span') %>% 
html_nodes(xpath = '//*[@id="content"]/section/article/div/div/div[3]/div/section/section/div[3]/div/div[2]/div/div/strong') |>    
html_text() |>
    stringr::str_remove(",") |>
    stringr::str_extract(pattern = "\\d+") |> 
    as.integer()


# write outputs -----------------------------------------------------------

dat_out <- data.frame(date = dat, cases = cases, ping_time = ping_time, vaccines = vaccines)

dat_out$clean_date <- data.table::as.IDate(with(dat_out, as.Date(stringr::str_extract(ping_time, "\\d{4}-\\d{2}-\\d{2}"))))

dat_old <-data.table::fread(here::here("data", "mpx.csv"))

dat_combined <- rbind(dat_old, dat_out, fill = TRUE)

dat_combined <- dat_combined[,tail(.SD,1), by = "clean_date"]

data.table::fwrite(dat_combined, here::here("data", "mpx.csv"))
