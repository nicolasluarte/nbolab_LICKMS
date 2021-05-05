##################################################################
##                             Libs                             ##
##################################################################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(zoo)

##########################################################################
##                           IMPORT FUNCTIONS                           ##
##########################################################################

source("functions.R")

##########################################################################
##                    LOAD FILES INTO DATAFRAME LIST                    ##
##########################################################################

path <- "/home/nicoluarte/phd_thesis/experimental_data/Resultados/FR Abril 2021"
# load all text files into memory
# ! make sure only relevant .txt files are present
files <- list.files(path, pattern = "*.csv")
# all text files are read as tibbles
tibbleListRaw <- files %>%
        map(~ read_csv(file.path(path, .)))
# add a column that indicates the filename
numbers <- seq(from = 1, to = length(tibbleListRaw), by = 1)
tibbleListRaw <- map2(tibbleListRaw, numbers, function(.x, .y) {
        .x %>% mutate(
                fileName = rep(files[.y], dim(.x)[1])
        )
})
# load look up table
# change this path to the correct one
lookUptablePath <- "/home/nicoluarte/phd_thesis/experimental_data/uncertainty_fr/lookUpTable.csv"
lookUpTable <- read_delim(lookUptablePath, col_names = TRUE, delim = ":")

##################################################################
##                    Initial pre-processing                    ##
##################################################################

# remove empty tibbles
tibbleListRaw <- purrr::keep(tibbleListRaw, ~ nrow(.x) > 0)
tibbleListRaw <- purrr::keep(tibbleListRaw, ~ all(.$fileName %in% lookUpTable$fileName))
tibbleListRaw <- map(tibbleListRaw, ~ drop_na(.))
# get animal names corresponding to each arduinoNumber
# WARNING!: code 999 means that no corresponding value was found in the lookUpTable
tibbleListRaw <- map(tibbleListRaw, function(x) {
        r <- c()
        for (i in 1:dim(x)[1]) {
                if (length(compareReturn(
                        x$fileName[i], as.integer(as.character(x$arduinoNumber[i])),
                        lookUpTable$fileName, lookUpTable$arduino,
                        lookUpTable$animalCode
                )) == 0) {
                        r[i] <- 999
                }
                else {
                        r[i] <- compareReturn(
                                x$fileName[i], as.integer(as.character(x$arduinoNumber[i])),
                                lookUpTable$fileName, lookUpTable$arduino,
                                lookUpTable$animalCode
                        )
                }
        }
        x %>% mutate(
                animalCode = r
        )
})

##################################################################
##                      Feature extraction                      ##
##################################################################

# add basic features
tibbleList <- tibbleListRaw %>%
        map(function(x) {
                x %>%
                        group_by(arduinoNumber, spoutNumber) %>%
                        mutate(
                                ms = msFromStart - msFromStart[1],
                                isEvent = getIsEvent(eventsCum),
                                isLick = getIsLick(eventsCum),
                                msFromEvent = getmsFromEvent(isEvent, ms),
                                isTimeout = getIsTimeout(msFromEvent, 20000, eventsCum)
                        ) %>%
                        ungroup()
        })
# add main features
tibbleList <- tibbleList %>%
        map(function(x) {
                x %>%
                        group_by(arduinoNumber, spoutNumber, isLick) %>%
                        mutate(
                                ILI = getILI(ms),
                                burstClusters = getBursts(ILI, 100)
                        ) %>%
                        ungroup()
        })
tibbleList <- tibbleList %>%
        map(function(x) {
                x %>%
                        group_by(arduinoNumber, spoutNumber, isLick, burstClusters) %>%
                        mutate(
                                burstCount = n()
                        ) %>%
                        ungroup()
        })
tibbleList <- tibbleList %>%
        map(function(x) {
                x %>%
                        mutate(
                                clusterMS = binsMs(ms, 600000)$msBins
                        )
        })


##################################################################
##                      Data visualization                      ##
##################################################################

plotData <- bind_rows(tibbleList)
plotData %>%
        ggplot(aes(ms, eventsCum)) +
        geom_point() +
        facet_wrap(~animalCode)

plotData %>%
        filter(burstCount > 100) %>%
        ggplot(aes(burstCount, fill = as.factor(clusterMS))) +
        geom_density(alpha = 0.5)

plotData %>%
        ggplot(aes(burstCount, fill = as.factor(clusterMS))) +
        geom_density(alpha = 0.5) +
        facet_wrap(~ as.factor(clusterMS))

##################################################################
##                          Statistics                          ##
################################################################## s

