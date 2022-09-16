library(dplyr)
library(stringr)
library(pdftools)
library(data.table)

url <- "https://epi.dph.ncdhhs.gov/cd/docs/2022MonkeypoxSurveillanceData.pdf"

tmp <- file.path("archive", paste0(format(Sys.time(), "%Y%m%d-%H%S"),".pdf"))

download.file(url, tmp, mode = "wb" )

dat_raw <- pdf_text(tmp)

dat_raw_line <- str_split(dat_raw, "\n")[[1]]

dat_raw_line <- str_replace_all(dat_raw_line, "\\+", "_")

extract_field <- function(x){
  x <- stringr::str_replace(x, "\\+", "_")
  str_extract(str_trim(dat_raw_line[which(grepl(x,dat_raw_line))[1]]), "\\d+(?= \\()")
}

extract_update <- function(x){
  z <- dat_raw_line[which(grepl("\\d{1,2}/\\d{1,2}/\\d{2}", dat_raw_line))]
  z <- str_trim(z)
  lubridate::mdy(str_extract(z,"\\d{1,2}/\\d{1,2}/\\d{2}"))
}

extract_update_2 <- function(x){
  pg2 <- str_split(dat_raw, "\n")[[2]]
  z <- pg2[which(grepl("\\d{1,2}, 2022", pg2))]
  z <- str_trim(str_extract(z,"(?<= as of )\\D+ \\d{1,2}, \\d{4}"))
  
  lubridate::mdy(z)
}

# targets in the raw text file ----------------------------------------------------------------


fields_of_interest <- c("Total","Male", "Female", "Other than sex assigned at",
                        "0-17", "18-30", "18-29", "30-49","31-50", "50+","51+",
                        "Black", "White","Multi-racial", "Asian", "American Indian/Alaska", "Other  ",
                        "Hispanic", "Non-Hispanic", "Unknown")

description_demographic <- c("Total","Gender", "Gender", "Gender",
                             "Age", "Age", "Age", "Age", "Age", "Age", "Age",
                             "Race", "Race", "Race","Race", "Race", "Race",
                             "Ethnicity", "Ethnicity", "Ethnicity")


# go get them ---------------------------------------------------------------------------------
extract_field("Other  ")

dat_extracted <- purrr::map(fields_of_interest, ~as.numeric(extract_field(.x)))

names(dat_extracted) <- fields_of_interest

dat_extracted$update_date <- if(length(extract_update()) ==0) extract_update_2() else extract_update()

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
