##################################################################
##                             Libs                             ##
##################################################################

library(ggplot2)
library(ggrepel)
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

path <- "~/nbolab_LICKMS/testFolder"
# load all text files into memory
# ! make sure only relevant .txt files are present
files <- list.files(path, pattern = "*.csv")
# all text files are read as tibbles
tibbleListRaw <- files %>%
        map(~ readLines(file.path(path, .)))

tibbleListRaw <- map(tibbleListRaw, function(x){
  unParsed <- x
  unParsed <- gsub(pattern = '(-[0-9]*)( -)', replace = '\\1\\20,', x = unParsed)
  unParsed <- gsub(pattern = "-", replace = ",", x = unParsed)
  dataRaw <- as_tibble(do.call(rbind, strsplit(unParsed, split=",")))
})


##################################################################
##                    Initial pre-processing                    ##
##################################################################

tibbleListRaw <- map(tibbleListRaw, function(x){
colnames(x) <- c("date", "pc_time",
	     "seconds_from_start", "ms_from_start",
	     "number_of_arduino", "spout",
	     "licks_cum", "events_cum")
return(x)
}
)
# data types
tibbleListRaw <- map(tibbleListRaw, function(x){
			     x %>% mutate(date = lubridate::ymd(date),
			     pc_time = as.numeric(pc_time),
			     seconds_from_start = as.numeric(seconds_from_start),
			     ms_from_start = as.numeric(ms_from_start),
			     number_of_arduino = as.numeric(number_of_arduino),
			     spout = as.numeric(spout),
			     licks_cum = as.numeric(licks_cum),
			     events_cum = as.numeric(events_cum),
			     ms = (seconds_from_start * 1000) + ms_from_start)
})
tibbleListRaw

# add features
tibbleListRaw <- tibbleListRaw %>%
	map(function(x)
	    {
		    x %>%
		    group_by(number_of_arduino, spout) %>%
		    mutate(
			   isEvent = getIsEvent(events_cum),
			   isLick = getIsLick(licks_cum)
			   )
	    })
tibbleListRaw %>% View()

# first view event x time
event_time <- tibbleListRaw %>%
	map(
	    function(x)
	    {
		    x %>%
			    filter(isEvent == 1)
	    }
	    )

# second view licks x time
licks_time <- tibbleListRaw %>%
	map(
	    function(x)
	    {
		    x %>%
			    filter(isLick ==1)
	    }
	    )

# third view relative time from event
relativeTimeCol <- function(eventIndex, df){
	v <- eventIndex
	down <- df[v[1]:dim(df)[1],]$ms - df[v[1],]$ms
	up <- df[1:v[1],]$ms - df[v[1],]$ms
	up <- up[-length(up)]
	r <- tibble(c(up, down))
	colnames(r) <- c(paste0("relative_time"))
	return(r)
}
relative_time <- tibbleListRaw %>%
	map(function(x){
		    v <- which(x$isEvent == 1)
		    v %>% map_dfc(~relativeTimeCol(., x))
	    })

relativeTimeRow <- function(eventIndex, df){
	v <- eventIndex
	down <- df[v[1]:dim(df)[1],]$ms - df[v[1],]$ms
	up <- df[1:v[1],]$ms - df[v[1],]$ms
	up <- up[-length(up)]
	r <- tibble(c(up, down))
	colnames(r) <- c(paste0("relative_time"))
	d <- bind_cols(df, r)
	return(d)
}
relative_time1 <- tibbleListRaw %>%
	map(function(x){
		    v <- which(x$isEvent == 1)
		    v %>% map_dfr(~relativeTimeRow(., x))
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

# recurrent plots
plotData <- bind_rows(tibbleList)
p1 <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(licksCum)) %>%
	ggplot(aes(as.factor(date), licks, group = 1)) +
	geom_line() +
	facet_wrap(~animalCode) +
	ggtitle("licks per session, per mice") +
	xlab("Days") +
	ylab("Number of licks per session")
p2 <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(eventsCum)) %>%
	ggplot(aes(as.factor(date), licks, group = 1)) +
	geom_line() +
	facet_wrap(~animalCode) +
	ggtitle("events per session, per mice") +
	xlab("Days") +
	ylab("Number of events per session")
p3 <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(licksCum)) %>%
	ungroup() %>%
	group_by(animalCode) %>%
	mutate(rollLicks = rollmean(licks, 3, fill = NA, align = "left"),
	       rollSd = rollapply(licks, 3, sd, fill = NA, align = "left")) %>%
	ggplot(aes(as.factor(date), rollLicks, group = 1)) +
	geom_line() +
	geom_errorbar(aes(ymin = rollLicks - rollSd, ymax = rollLicks + rollSd)) +
	facet_wrap(~animalCode, scale = "free") +
	ggtitle("Roll mean licks (3)") +
	xlab("Days") +
	ylab("Rolling mean of licks")
p4 <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(eventsCum)) %>%
	ungroup() %>%
	group_by(animalCode) %>%
	mutate(rollLicks = rollmean(licks, 3, fill = NA, align = "left"),
	       rollSd = rollapply(licks, 3, sd, fill = NA, align = "left")) %>%
	ggplot(aes(as.factor(date), rollLicks, group = 1)) +
	geom_line() +
	geom_errorbar(aes(ymin = rollLicks - rollSd, ymax = rollLicks + rollSd)) +
	facet_wrap(~animalCode, scale = "free") +
	ggtitle("Roll mean events (3)") +
	xlab("Days") +
	ylab("Rolling mean of events")
means <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(licksCum),
		  events = max(eventsCum)) %>%
	ungroup() %>%
	group_by(date) %>%
	summarise(meanLicks = mean(licks),
	sdLicks = sd(licks),
	meanEvents = mean(events),
	sdEvents = sd(events))
means <- means %>%
	mutate(M = cummean(meanLicks),
	S = cummean(sdLicks),
	ME = cummean(meanEvents),
	SE = cummean(sdEvents))
p5 <- means %>%
	ggplot(aes(as.factor(date), M, group = 1)) +
	geom_point() +
	geom_errorbar(aes(ymin = M - S, ymax = M + S)) +
	geom_line() +
	ggtitle("Cumulative licks average") +
	xlab("Cumulative up to day") +
	ylab("Mean number of licks")
p6 <- means %>%
	ggplot(aes(as.factor(date), ME, group = 1)) +
	geom_point() +
	geom_errorbar(aes(ymin = ME - SE, ymax = ME + SE)) +
	geom_line() +
	ggtitle("Cumulative events average") +
	xlab("Cumulative up to day") +
	ylab("Mean number of events")
p7 <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(pref = mean(spoutNumber == 0)) %>%
	ggplot(aes(as.factor(date), pref, group = 1)) +
	geom_point() +
	geom_line() +
	facet_wrap(~animalCode) +
	ggtitle("preference for spout 0")

p8 <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(licksCum),
		  events = max(eventsCum)) %>%
	mutate(meanLicks = mean(licks),
	sdLicks = sd(licks),
	meanEvents = mean(events),
	sdEvents = sd(events)) %>%
	mutate(S = cummean(sdLicks),
	M = cummean(meanLicks),
	ME = cummean(meanEvents),
	SE = cummean(sdEvents)) %>%
	ggplot(aes(as.factor(date), M, group = 1)) +
	geom_line(aes(y = licks, group = as.factor(animalCode) )) +
	geom_label(aes(y = licks, label = as.factor(animalCode))) +
	geom_point(color = "red") +
	geom_line(color = "red") +
	geom_errorbar(aes(ymin = M - S, ymax = M + S)) +
	theme(legend.position = "bottom") +
	xlab("Days") +
	ylab("Cumulative licks")

p9 <- plotData %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(licksCum),
		  events = max(eventsCum)) %>%
	mutate(meanLicks = mean(licks),
	sdLicks = sd(licks),
	meanEvents = mean(events),
	sdEvents = sd(events)) %>%
	mutate(S = cummean(sdLicks),
	M = cummean(meanLicks),
	ME = cummean(meanEvents),
	SE = cummean(sdEvents)) %>%
	ggplot(aes(as.factor(date), ME, group = 1)) +
	geom_point() +
	geom_line() +
	geom_point(aes(y = events, color = as.factor(animalCode))) +
	theme(legend.position = "bottom") +
	xlab("Days") +
	ylab("Cumulative events")

plots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)
pdf("plotsR.pdf")
plots
dev.off()


## ranks
f2 <- plotData %>%
	mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(licksCum),
		  events = max(eventsCum)) %>%
	mutate(meanLicks = mean(licks),
	sdLicks = sd(licks),
	meanEvents = mean(events),
	sdEvents = sd(events)) %>%
	mutate(S = cummean(sdLicks),
	M = cummean(meanLicks),
	ME = cummean(meanEvents),
	SE = cummean(sdEvents)) %>%
	ggplot(aes(as.factor(date), M, group = 1)) +
	geom_line(aes(y = licks, group = as.factor(animalCode) )) +
	geom_label(aes(y = licks, label = as.factor(animalCode))) +
	geom_point(color = "red") +
	geom_line(color = "red") +
	geom_errorbar(aes(ymin = M - S, ymax = M + S)) +
	theme(legend.position = "bottom") +
	xlab("Days") +
	ylab("Cumulative licks") +
	ggtitle("Convergencia en la cantidad de licks por sesion")
## ranks
f1 <- plotData %>%
	mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
	group_by(date, animalCode) %>%
	summarise(licks = max(licksCum),
		  events = max(eventsCum)) %>%
	mutate(meanLicks = mean(licks),
	sdLicks = sd(licks),
	meanEvents = mean(events),
	sdEvents = sd(events)) %>%
	mutate(S = cummean(sdLicks),
	M = cummean(meanLicks),
	ME = cummean(meanEvents),
	SE = cummean(sdEvents)) %>%
	ggplot(aes(as.factor(date), ME, group = 1)) +
	geom_line(aes(y = events, group = as.factor(animalCode) )) +
	geom_label(aes(y = events, label = as.factor(animalCode))) +
	geom_point(color = "red") +
	geom_line(color = "red") +
	geom_errorbar(aes(ymin = ME - SE, ymax = ME + SE)) +
	theme(legend.position = "bottom") +
	xlab("Days") +
	ylab("Cumulative events") +
	ggtitle("Convergencia en la cantidad de eventos por sesion")
plots <- list(f1)
pdf("plotsGroup.pdf")
plots
dev.off()


# ranks


plotData %>%
	group_by(animalCode, date) %>%
	summarise(E = max(eventsCum)) %>%
	mutate(roll = rollmean(E, 3, fill = NA, align = "right"),
	       date = as.Date(as.character(date), format = "%Y%m%d")) %>%
	filter(date == "2021-05-10") %>%
	arrange(desc(E))

A <- plotData %>%
	filter(animalCode == c(218, 220, 222, 221), date >= "2021-05-09") %>%
	mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
	group_by(animalCode, date) %>%
	summarise(E = mean(max(eventsCum))) %>%
	arrange(desc(lubridate::ymd(date))) %>%
	head(n = 6) %>%
	mutate(group = 1) 
A <- A[c(1,2,3,6),]
B <- plotData %>%
	filter(animalCode == c(224, 223, 219, 217), date >= "2021-05-09") %>%
	mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
	group_by(animalCode, date) %>%
	summarise(E = mean(max(eventsCum))) %>%
	arrange(desc(lubridate::ymd(date))) %>%
	head(n = 4) %>%
	mutate(group = 2)

ff <- bind_rows(A, B) %>%
	group_by(group) %>%
	mutate(MEAN = mean(E), SD = sd(E)) %>%
	ggplot(aes(as.factor(group), MEAN)) +
	geom_point(aes(as.factor(group), E)) +
	geom_label_repel(aes(y = E, label = animalCode)) +
	geom_errorbar(aes(ymin = MEAN - SD, ymax = MEAN + SD)) +
	geom_point(color = "red") +
	xlab("Groups") +
	ylab("Mean of last 3 days events") +
	ggtitle("Stratified sampling for events, t-test p-val = 0.24")
plots <- list(ff)
pdf("plotsGroup.pdf")
plots
dev.off()

te <- bind_rows(A, B)
t.test(te$E ~ as.factor(te$group))

