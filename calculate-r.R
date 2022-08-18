library(epinowcast)
library(dplyr)
dat_mpx <- data.table::fread("https://raw.githubusercontent.com/wf-id/nc-mpx/main/data/mpx.csv")

dat_mpx$clean_date <- with(dat_mpx, as.Date(stringr::str_extract(ping_time, "\\d{4}-\\d{2}-\\d{2}")))

dat_mpx$new_cases <- c(NA, diff(dat_mpx$cases))

plot(cases ~ clean_date, dat_mpx)
plot(new_cases ~ clean_date, dat_mpx)

library(EpiNow2)
library(data.table)
get_generation_time(disease = "SARS-CoV-2", source = "ganyani")

mpx_generation <- list()

mpx_generation$mean = 8.58
mpx_generation$mean_sd = 1.06
mpx_generation$sd = 4.16
mpx_generation$sd_sd = 1.01
mpx_generation$max <- 21

mpx_test_delay <- list()

mpx_test_delay$mean = 8.17
mpx_test_delay$mean_sd = 1.57
mpx_test_delay$sd = 2.11
mpx_test_delay$sd_sd = 0.534
mpx_test_delay$max <- 15


case_data <- dat_mpx %>% 
  select(date = clean_date, confirm = new_cases ) %>% 
  filter(!is.na(confirm)) %>%
  padr::pad() %>% 
  mutate(confirm = ifelse(is.na(confirm),0,confirm)) %>% 
  as.data.table()


glm(cases ~ as.numeric(clean_date), data = dat_mpx, family = quasipoisson())

EpiNow2::growth_to_R( 0.07 , 8.17, 2.11)

estimates <- epinow(reported_cases = case_data,
                    generation_time = mpx_generation,
                    delays = delay_opts(mpx_test_delay),
                    rt = rt_opts(prior = list(mean = 1, sd = 0.5)),
                    stan = stan_opts(cores = 4))
