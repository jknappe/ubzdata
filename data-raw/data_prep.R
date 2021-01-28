## code to prepare `DATASET` dataset goes here
###############################################################


### libraries

library(rdwd)
library(magrittr)
library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(padr)
library(lubridate)
library(stringr)


### rainfall data from DWD (Leipzig) for reference years

dwd_ref_rain =
    # create data file URL
    selectDWD(name = "Leipzig-Holzhausen",
              res = "10_minutes",
              var = "precipitation",
              per = "historical",
              remove_dupli = FALSE) %>%
    tibble(connection = .) %>%
    # download and parse data
    mutate(path = map(.x = connection,
                      .f = ~dataDWD(url = .x,
                                    read = FALSE)),
           file = map(.x = path,
                      .f = ~unzip(.x)),
           data = map(.x = file,
                      .f = ~read_csv2(.x,
                                      col_types = cols(STATIONS_ID = col_integer(),
                                                       MESS_DATUM = col_datetime(format = "%Y%m%d%H%M"),
                                                       QN = col_integer(),
                                                       RWS_DAU_10 = col_double(),
                                                       RWS_10 = col_character(),
                                                       RWS_IND_10 = col_double()))))  %>%
    select(data) %>%
    unnest(data) %>%
    # extract rain data and convert to tidy format
    rename(datetime = MESS_DATUM,
           rain_mm.10min = RWS_10) %>%
    mutate(year = year(datetime),
           rain_mm.10min = as.numeric(str_trim(rain_mm.10min))) %>%
    select(year, datetime, rain_mm.10min) %>%
    # select target years and add qualifiers for each year (wet, normal, dry)
    filter(year %in% c(2010, 2017, 2018)) %>%
    mutate(year_status = case_when(year == 2010 ~ "wet",
                                   year == 2017 ~ "average",
                                   year == 2018 ~ "dry"),
           year_status = factor(year_status),
           year = factor(year)) %>%
    # replace NAs
    mutate(rain_mm.10min = ifelse(rain_mm.10min == -999, 0, rain_mm.10min)) %>%
    # pad missing timestamps
    group_by(year) %>%
    pad(interval = "10 min") %>%
    ungroup() %>%
    # summarize by day
    thicken(interval = "day",
            rounding = "down",
            colname = "date") %>%
    group_by(year, date, year_status) %>%
    summarise(rain_mm.d = sum(rain_mm.10min)) %>%
    ungroup() %>%
    mutate(rain_mm.d = round(rain_mm.d, 2))

usethis::use_data(dwd_ref_rain, overwrite = TRUE)


### evapotranspiration data from DWD (Leipzig) for reference years
