require(lubridate)
require(tidyverse)

mm_walk<-read_csv("https://data.edinburghopendata.info/dataset/d4a86f41-5e78-48cd-adb8-2064acf93ef0/resource/ffbeb785-a19f-4788-bc27-892b024a6750/download/15middlemeadowwalk.csv") %>% mutate(
    newdate = dmy(date), 
    month = month(newdate,label = TRUE),
    year = year(newdate),
    day = day(newdate)
) %>% filter(year==2012) %>% 
    group_by(month, day, time) %>%
    summarise(cyclists = sum(channel_1)+sum(channel_2)) %>% ungroup %>%
    rename(hour = time)

set.seed(1345)
cycdat<-read_csv("https://data.edinburghopendata.info/dataset/d4a86f41-5e78-48cd-adb8-2064acf93ef0/resource/3944c195-947c-4e45-aa21-5a7fea264c52/download/32inverleithrow.csv") %>% mutate(
    newdate = dmy(date), 
    month = month(newdate,label = TRUE),
    year = year(newdate)
) %>% group_by(year, month) %>%
    summarise(cyclists = sum(channel_1)+sum(channel_2)) %>%
    ungroup %>%
    bind_rows(
        tibble(year = c(rep(2014,12),rep(2015,2)),
               month = month(c(1:12,1,2), label=T),
               cyclists = round(rnorm(14,4725,1500))
        ),
        .
    )

raindat <- read_csv("https://www2.sepa.org.uk/rainfall/api/Month/15201?csv=true") %>%
    mutate(
        date = dmy(paste0("01 ", Timestamp)),
        month = month(date, label = TRUE),
        year = year(date),
        rainfall_mm = Value
    ) %>% select(year,month,rainfall_mm)

inv_row <- left_join(cycdat, raindat)