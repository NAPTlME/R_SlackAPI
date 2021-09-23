# Functions to interface with slack and perform commmon read tasks
# 

library(httr)
library(dplyr)
library(stringr)
library(jsonlite)
library(lubridate)

#### Urls ####
# https://api.slack.com/methods

# https://api.slack.com/methods/conversations.list
conversations.list.url = "https://slack.com/api/conversations.list" #lists all channels
# https://api.slack.com/methods/conversations.history
conversations.history.url = "https://slack.com/api/conversations.history" #lists conversations in channel
# https://api.slack.com/methods/users.list
users.list.url = "https://slack.com/api/users.list" # get users
# https://api.slack.com/methods/conversations.replies
conversations.replies.url = "https://slack.com/api/conversations.replies"

#### Functions ####

convertTsToDateTime = function(x, tz = "America/Chicago"){
  as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = tz)
}

convertDateTimeToTs = function(x){
  as.numeric(x) # not really needed as a function, but I end up looking it up every time if I don't do this.
}

#### Generic Slack call with Pagination ####

slackCall = function(url, body, sleepEveryXCalls = 0){
  contentList = list()
  body$cursor = NULL
  response = POST(url, body = body, encode = "form")
  response.content = httr::content(response)
  
  contentList[[length(contentList) + 1]] = response.content
  
  i = 1
  while(!is.null(response.content$response_metadata$next_cursor) && response.content$response_metadata$next_cursor != ""){
    i <<- i + 1
    if (sleepEveryXCalls != 0 && i %% sleepEveryXCalls == 0){
      Sys.sleep(60) # sleep for one minute before continuing to avoid rate limits
    }
    body$cursor = response.content$response_metadata$next_cursor
    response = POST(url, body = body, encode = "form")
    response.content = httr::content(response)
    
    contentList[[length(contentList) + 1]] = response.content
  }
  contentList
}

#### Get Channels ####

getChannels = function(token, sleepEveryXCalls = 5){
  body = list(token = token)
  
  results = slackCall(conversations.list.url, body, sleepEveryXCalls)
  response = POST(conversations.list.url, body = list(token = token), encode = "form")
  response.content = httr::content(response)
  
  do.call(rbind, lapply(results, function(response.content) {
    do.call(rbind, lapply(response.content$channels, function(x){
      data.frame(id = x$id, name = x$name)
    }))
  }))
}


#### Get Users From Each Channel

getUsers = function(token, sleepEveryXCalls = 5, returnRaw = F){
  body = list(token = token)
  results = slackCall(users.list.url, body = body)
  
  if(returnRaw){
    results
  } else {
    do.call(rbind, lapply(results, function(userInfoContent){
      do.call(rbind, lapply(userInfoContent$members, function(x){
        data.frame(id = x[["id"]], name = x[["name"]], is_bot = x[["is_bot"]], is_app_user = x[["is_app_user"]])
      }))
    }))
  }
}



#### Get Conversations ####

getConversations = function(token, channel, oldest = 0, latest = NULL, sleepEveryXCalls = 45, returnRaw = F){
  if(!is.null(oldest)){
    if (class(oldest)[1] == "character"){
      oldest = convertDateTimeToTs(as.POSIXct(oldest, tz = "America/Chicago"))
    } else if (class(oldest)[1] == "POSIXct"){
      oldest = convertDateTimeToTs(oldest)
    }
    if (is.na(oldest) || class(oldest)[1] != "numeric"){
      stop("Unable to parse `oldest`")
    }
  }
  
  if(!is.null(latest)){
    if (class(latest)[1] == "character"  && latest != "now"){
      latest = convertDateTimeToTs(as.POSIXct(latest, tz = "America/Chicago"))
    } else if (class(latest)[1] == "POSIXct"){
      latest = convertDateTimeToTs(latest)
    }
    if (is.na(latest) || latest != "now" && class(latest)[1] != "numeric"){
      stop("Unable to parse `latest`")
    }
  }
  
  
  userInfoDf = getUsers(token)
  
  body = list(token = token, 
              channel = channel,
              oldest = oldest,
              latest = latest)
  
  results = slackCall(conversations.history.url, body = body, sleepEveryXCalls = sleepEveryXCalls)
  
  if (returnRaw){
    results
  } else {
    do.call(rbind, lapply(results, function(response.content){
      do.call(rbind, lapply(response.content$messages, function(x){
        id = x[["user"]]
        ts = x[["ts"]]
        text = x[["text"]]
        replyCount = ifelse(!is.null(x[["reply_count"]]), x[["reply_count"]], 0)
        if(!any(is.null(id), is.null(ts), is.null(text)))
          data.frame(id = id, ts = ts, text = text, reply_count = replyCount)
      }))
    })) %>% 
      mutate(datetime = convertTsToDateTime(ts),
             user = as.character(sapply(id, function(x) ifelse(x %in% userInfoDf$id, userInfoDf$name[userInfoDf$id == x], NA))))
  }
}

#### Get Replies to a message (thread) ####

getReplies = function(token, channel, parentTs, sleepEveryXCalls = 0){
  body = list(token = token, channel = channel, ts = parentTs)
  
  results = slackCall(conversations.replies.url, body, sleepEveryXCalls)
  
  # todo, add parsing
  
  results
}
