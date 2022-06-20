library(dplyr)
library(stringr)
library(jsonlite)
library(lubridate)

slackToken = "xoxp-22405684500-1851025350677-2155904832692-219e90454444bbf70efcc74966c01374"

#### Functions ####

#### Get Channels ####

channelsDf = getChannels(slackToken)

channel.general = channelsDf$id[channelsDf$name == "general"]
channel.support = channelsDf$id[channelsDf$name == "support"]

#### Get General Text ####

oldest = convertDateTimeToTs(as.POSIXct("2019-07-01 00:00:01", tz = "America/Chicago"))

generalDf = getConversations(slackToken, channel.general, oldest = oldest)
#### Get Support Text ####

supportDf = getConversations(slackToken, channel.support, oldest = oldest)
