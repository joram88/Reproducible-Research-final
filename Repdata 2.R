url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" 
download(url, dest="repdata_data_StormData.csv", mode="wb")
storm <- read.csv("repdata_data_StormData.csv")

