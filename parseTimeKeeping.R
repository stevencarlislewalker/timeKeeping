library(knitr)
library(dplyr)
library(tidyr)
library(data.table)
dat <- readLines("timeKeeping")
dat <- dat[dat != ""]
dateLines <- grep("20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]", dat)
dateStartLines <- dateLines + 2
dateEndLines <- c(dateLines[-1] - 1, length(dat))
listOfDates <- mapply(function(start, end) dat[start:end],
                      dateStartLines, dateEndLines, SIMPLIFY = FALSE)
names(listOfDates) <- dat[dateLines]
listOfDataFrames <-
    lapply(listOfDates, function(xx) {
               xx %>%
                 strsplit(" -- ") %>%
                 simplify2array %>%
                 t %>%
                 as_data_frame %>%
                 setNames(c("Time Range", "Activity")) %>%
                 separate("Time Range", c("Start Time", "End Time"), sep = "-")
           })
dataFrame <-
    mapply(function(x, y) mutate(x, Date = y), listOfDataFrames, names(listOfDataFrames),
           SIMPLIFY = FALSE) %>%
      rbindlist %>%
      select(Date, `Start Time`, `End Time`, Activity) %>%
      mutate(Date2 = Date) %>%
      mutate(Date3 = Date) %>%
      unite("Start Time", Date2, `Start Time`, sep = " ") %>%
      unite("End Time", Date3, `End Time`, sep = " ") %>%
      mutate(`Start Time` = as.POSIXct(`Start Time`, format = "%Y-%m-%d %H:%M")) %>%
      mutate(`End Time` = as.POSIXct(`End Time`, format = "%Y-%m-%d %H:%M")) %>%
      mutate(Duration = `End Time` - `Start Time`)


hoursPerDayDF <-
    dataFrame %>%
      filter(!grepl("lunch", Activity)) %>%
      group_by(Date) %>%
      summarise(hoursPerDay = as.numeric(sum(Duration)) / 60) %>%
      ungroup()

