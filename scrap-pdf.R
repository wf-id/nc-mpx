library(dplyr)
library(stringr)
library(pdftools)
library(data.table)

url <- "https://epi.dph.ncdhhs.gov/cd/docs/2022MonkeypoxSurveillanceData.pdf"

tmp <- tempfile()

download.file(url, tmp, mode = "wb" )

dat_raw <- pdf_text(tmp)

dat_raw_line <- str_split(dat_raw, "\n")[[1]]


extract_field <- function(x){
  str_extract(str_trim(dat_raw_line[which(grepl(x,dat_raw_line))[1]]), "\\d+")
}

extract_update <- function(x){
  lubridate::mdy(str_remove(str_extract(str_trim(dat_raw_line[which(grepl("available as of ",dat_raw_line))[1]]),
              "\\D+ \\d{1,2}, \\d{4}"), "Includes data available as of "))
}


# targets in the raw text file ----------------------------------------------------------------


fields_of_interest <- c("Male", "Female", "0-17", "18-30", "31-50", "50+",
                        "Black", "White", "Asian","Hispanic", "Non-Hispanic")

description_demographic <- c("Gender", "Gender", "Age", "Age", "Age", "Age",
                             "Race", "Race", "Race", "Ethnicity", "Ethnicity")


# go get them ---------------------------------------------------------------------------------


dat_extracted <- purrr::map(fields_of_interest, extract_field)

names(dat_extracted) <- fields_of_interest

dat_extracted$update_date <- extract_update()

demos <- bind_rows(dat_extracted)

# convert -------------------------------------------------------------------------------------

# Converting to a long format so it is more future-proof as the fields evolve

setDT(demos)

demos_long <- melt(demos,variable.name = "description", value.name = "count", id.vars = "update_date")


convert_key <- data.table(demographic = description_demographic,
                          description = fields_of_interest)

demos_long <- merge(demos_long,convert_key, by = "description", all.x = TRUE)

# write it out --------------------------------------------------------------------------------
# Note append to save space.

data.table::fwrite(demos_long, here::here("data", "demos.csv"), append = TRUE)
