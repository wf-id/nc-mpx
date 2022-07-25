library(dplyr)
library(stringr)
library(pdftools)

url <- "https://epi.dph.ncdhhs.gov/cd/docs/2022MonkeypoxSurveillanceData.pdf"

tmp <- tempfile()

download.file(url, tmp)

dat_raw <- pdf_text(tmp)

dat_raw_line <- str_split(dat_raw, "\n")[[1]]


extract_field <- function(x){
  str_extract(str_trim(dat_raw_line[which(grepl(x,dat_raw_line))[1]]), "\\d+")
}

extract_update <- function(x){
  lubridate::mdy(str_remove(str_extract(str_trim(dat_raw_line[which(grepl("available as of ",dat_raw_line))[1]]),
              "\\D+ \\d{1,2}, \\d{4}"), "Includes data available as of "))
}


fields_of_interest <- c("Male", "Female", "0-17", "18-30", "31-50", "50+",
                        "Black", "White", "Asian","Hispanic", "Non-Hispanic")

dat_extracted <- purrr::map(fields_of_interest, extract_field)

names(dat_extracted) <- fields_of_interest

dat_extracted$update_date <- extract_update()

demos <- bind_rows(dat_extracted)

data.table::fwrite(demos, here::here("data", "demos.csv"), append = TRUE)
