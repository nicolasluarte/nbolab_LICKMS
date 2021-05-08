##################################################################
##                             Libs                             ##
##################################################################

library(ggplot2)
library(ggsci)
library(glue)
library(dplyr)
library(tidyverse)
library(zoo)

##########################################################################
##                           IMPORT FUNCTIONS                           ##
##########################################################################

source("lickMicroStructureFunctions.R")

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
# build the hashtable
hashTable <- tibble(key = paste0(lookUpTable$fileName, lookUpTable$arduino),
value = lookUpTable$animalCode)
# get animal names corresponding to each arduinoNumber
# WARNING!: code 999 means that no corresponding value was found in the lookUpTable
tibbleListRaw <- map(tibbleListRaw, function(x) {
			     key <- paste0(x$fileName, x$arduinoNumber)
			     x %>%
				     mutate(animalCode = compareReturn(hashTable, key))
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
plotData%>%
	filter(ILI < 1000, msFromEvent < 60000) %>%
	ggplot(aes(msFromEvent, ILI)) +
	geom_point()

plotData <- bind_rows(tibbleList)
plotData%>%
	filter(ILI < 1000) %>%
	ggplot(aes(ILI, ..scaled..)) +
	geom_density() +
	facet_wrap(~clusterMS)

plotData <- bind_rows(tibbleList)
plotData%>%
	filter(ILI < 250, ms < 1800000) %>%
	ggplot(aes(ms, ILI)) +
	geom_line(aes(y = rollapplyr(ILI, 1000, sd, fill = NA)))

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

rolling <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(rewards = max(eventsCum)) %>%
	group_by(animalCode) %>%
	mutate(rollMeans = rollmean(rewards, 3, fill = NA)) %>%
	mutate(SD = rollapply(rewards, 3, sd, fill = NA))
rolling %>%
	ggplot(aes(as.factor(date), rollMeans, group = 1)) +
	geom_point() +
	geom_line() +
	geom_errorbar(aes(ymin = rollMeans - SD, ymax = rollMeans + SD)) +
	facet_wrap(~as.factor(animalCode)) +
	ggtitle("Rolling average(3) + rolling SD")

rolling <- plotData %>%
	group_by(date) %>%
	summarise(rewards = max(eventsCum)) %>%
	mutate(rollMeans = rollmean(rewards, 3, fill = NA)) %>%
	mutate(SD = rollapply(rewards, 3, sd, fill = NA))
rolling %>%
	ggplot(aes(as.factor(date), rollMeans, group = 1)) +
	geom_point() +
	geom_line() +
	geom_errorbar(aes(ymin = rollMeans - SD, ymax = rollMeans + SD)) +
	ggtitle("Rolling average(3) + rolling SD, total")

a <- plotData %>%
	group_by(animalCode) %>%
	summarise(MEAN = mean(eventsCum),
		  SD = sd(eventsCum)) %>%
	ggplot(aes(as.factor(animalCode), MEAN)) +
	geom_point() +
	geom_errorbar(aes(ymin = MEAN - SD, ymax = MEAN + SD)) +
	ggtitle("Mean events")
b <- plotData %>%
	group_by(animalCode) %>%
	summarise(MEAN = mean(licksCum),
		  SD = sd(licksCum)) %>%
	ggplot(aes(as.factor(animalCode), MEAN)) +
	geom_point() +
	geom_errorbar(aes(ymin = MEAN - SD, ymax = MEAN + SD)) +
	ggtitle("Mean licks")

plotData %>%
	group_by(animalCode, spoutNumber) %>%
	summarise(MEAN = mean(licksCum),
		  SD = sd(licksCum)
		  ) %>%
	ggplot(aes(as.factor(animalCode), MEAN)) +
	geom_point() +
	geom_errorbar(aes(ymin = MEAN - SD, ymax = MEAN + SD)) +
	facet_wrap(~spoutNumber) +
	ggtitle("Mean licks")

plots <- list(a, b)
pdf("plots.pdf")
plots
dev.off()

plotData %>%
	group_by(fileName, animalCode) %>%
	summarise(maxEvents = max(eventsCum),
		  maxLicks = max(licksCum)) %>%
	View()

##################################################################
##                          Statistics                          ##
##################################################################

# mean, sd, coefficient of variation

# cutoff ILI larger than 1000ms and smaller than 60ms
tibbleList <- tibbleList %>%
	map_dfr(function(x) x %>%
		filter(ILI < 1000, ILI > 60) %>%
		group_by(animalCode) %>%
		mutate(meanILI = mean(ILI),
			sdILI = sd(ILI),
			coeffVariationILI = meanILI/sdILI)
		)

# histograms
nestTibble <- tibbleList %>%
	filter(ILI < 1000, ILI > 60, eventsCum > 30) %>%
	group_by(animalCode) %>%
	nest() %>%
	mutate(plotDensity = map2(data, animalCode, ~ ggplot(data = .x,
							     aes(ILI, ..scaled..)) +
						ggtitle(glue("Mice: {.y}")) +
						geom_density() +
						theme_bw()),
		density = map(data, ~ density(.$ILI)),
		densityMode = map(density, function(x) x$x[which.max(x$y)]))
nestTibble$plotDensity[[1]]


acf(nestTibble$data[[3]]$ILI, plot = TRUE, lag.max = 1000)

