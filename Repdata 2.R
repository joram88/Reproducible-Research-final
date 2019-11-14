library(downloader)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(lubridate)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" 
download(url, dest="repdata_data_StormData.csv", mode="wb")
storm <- read.csv("repdata_data_StormData.csv")

#The Event type category is a bit messy, so I make some adjustments
#to the largest categories to clean up based on the supp docs

storm$EVTYPE[storm$EVTYPE %in% "TSTM WIND"] <- "THUNDERSTORM WIND"
storm$EVTYPE[storm$EVTYPE %in% "THUNDERSTORM WINDS"] <- "THUNDERSTORM WIND"
storm$EVTYPE[storm$EVTYPE %in% "MARINE TSTM WIND"] <- "MARINE THUNDERSTORM WIND"


#First we look at what the top 20 most common event is in the data

count <- head(arrange(storm %>% count(EVTYPE), desc(n)), n=20)
count$rank <- seq(20)
count$top <- "X"


#Next we see which caused the most damage

storm$dmg <- storm$PROPDMG+storm$CROPDMG

propdmg <- head(arrange(storm %>% 
                                group_by(EVTYPE) %>% 
                                summarize(dmg = sum(dmg)),
                        desc(dmg)), n = 10)

propdmg <- arrange(merge(propdmg, count))

#Next a graph of cost per incident

ggplot(propdmg, aes(x = dmg, y = n))+
        geom_point()+
        geom_label_repel(size = 2.5, label=propdmg$EVTYPE)+
        labs(x = "Property and Crop Damage (USD)",
             y = "Number of Events",
             title = "Damage per Event (TOP 10)*",
             caption = "*In terms of damage in dollars")+
        theme_economist()


################################################

#INJURIES AND FATALITIES

inj <- head(arrange(storm %>% 
                            group_by(EVTYPE) %>% 
                            summarize(INJURIES = sum(INJURIES)),
                    desc(INJURIES)), n = 20)

ftl <- head(arrange(storm %>% 
                            group_by(EVTYPE) %>% 
                            summarize(FATALITIES = sum(FATALITIES)),
                    desc(FATALITIES)), n = 20)

hurt <- head(merge(inj, ftl), n = 10)
hurt$totals <- (hurt$INJURIES+hurt$FATALITIES)

hurt2 <- gather(hurt, key = Incident, value = totals, -EVTYPE, -totals)

ggplot(hurt2, aes(EVTYPE, totals))+
        geom_bar(aes(fill = Incident), position = position_stack(reverse = TRUE),
                 stat="identity")+coord_flip()+
        labs(y = "Total Individuals",
             x = NULL,
             title = "Fatalities and Injuries by Type of Event (TOP 10)*",
             caption = "*In terms of total injuries and fatalities")+
        theme_igray()

####Changes over time and the composition of these disasters

storm$date <- mdy_hms(storm$BGN_DATE)
storm$year <- year(storm$date)

top <- filter(merge(storm, count), top == "X" & rank < 11)

y <- ggplot(top, aes(year))
y+geom_bar(aes(fill=EVTYPE))+
        labs(x = "Year",
             y = "Number of Events",
             title = "Number of Events by Year (TOP 10)*",
             caption = "*By number of occurrences")+
             scale_fill_brewer(palette="RdYlBu")
