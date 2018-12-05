library(stringr)
library(dplyr)
library(tibble)
#Check in what directory you're in
getwd()

#Set working directory where file is
setwd("/Users/jencruz/Downloads/UCSF_LibCal/Data/DataScience/BioinformaticsForBiologists")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

dir <- list.files("/Users/jencruz/Downloads/UCSF_LibCal/Data")
waitlist <- data.frame()
registration <- data.frame()
cancelled <- data.frame()
event <- data.frame()
for(i in 2:length(dir)){
  print(dir[i])
  folder <- trim(dir[i])
  path <- paste("/Users/jencruz/Downloads/UCSF_LibCal/Data/", dir[i])
  path <- gsub(" ", "", path, fixed = TRUE)
  dir2 <- list.files(path)
  print(dir2)
  for(j in 1:length(dir2)){
    subfolder <- dir2[j]
    path2 <- paste(path,'/',dir2[j])
    path2 <- gsub(" ", "", path2, fixed = TRUE)
    dir3 <- list.files(path2)
    #print(dir3)
    for(l in 1:length(dir3)){
      path3 <- paste(path2,'/',dir3[l])
      finalPath <- gsub(" ", "", path3, fixed = TRUE)
      print('here')
      finalPath <- trim(finalPath)
      print(finalPath)
      if(identical(subfolder,"cancelled")){
        for(x in 1:length(finalPath)){
          print("cancelled")
          df <- addCanc(finalPath,folder)
          print('done')
          if(!is.null(df)){
            cancelled <- rbind(cancelled, df)
          }
        }
      }else if(identical(subfolder,"event")){
        for(x in 1:length(finalPath)){
          print("event")
          df <- addEvent(finalPath,folder)
          print('done')
          if(!is.null(df)){
            event <- rbind(event, df)
          }
        }
      }else if(identical(subfolder,"registration")){
        print('registration')
        for(x in 1:length(finalPath)){
          df <- addReg(finalPath,folder)
          if(!is.null(df)){
            registration <- rbind(registration, df)
          }
        }
      }else{
        #waitlist
        print('waitlist')
        df <- addWL(finalPath,subfolder)
        if(!is.null(df)){
          waitlist <- rbind(waitlist, df)
        }
      }
    } 
  }
}

  
headerBasic <- c("list", "category", "event", "date", "time", "presenter", "location", "campus", "seats")
headerWL <- c("list", "category", "first", "last", "email", "booking", "status", "school", "dept", "question1","question2")
headerReg <- c("list", "category", "first", "last", "email", "booking",
               "attendance", "phoneNumber", "status", "school", "dept", "question1", "question2")
headerReg4 <- c("list", "category", "first", "last", "email", "booking",
               "attendance", "phoneNumber", "status", "school", "dept", "question1")
headerWL2 <- c("list", "category", "first", "last", "email", "booking")
headerReg3 <- c("list", "category", "first", "last", "email", "booking",
               "attendance", "status", "school", "dept", "question")
headerReg2 <- c("list", "category", "first", "last", "email", "booking", "attendance")
headerCanc <- c("list", "category", "first", "last", "email", "booking")
headerEvent <- c("list", "category", "date", "time", "presenter", "location", "campus",
                 "categories", "attendance","registration", "confirmed","cancelled", 
                 "firstRegistration", "lastRegistration")
headerEvent2 <- c("list", "category", "date", "time", "presenter", "location", "campus",
                 "categories", "registration", "confirmed","cancelled", 
                 "firstRegistration", "lastRegistration")
headerEvent3 <- c("list", "category", "date", "time", "location", "campus",
                  "categories")
headerEvent4 <- c("list", "category", "date", "time", "location", "campus",
                   "categories", "registration", "confirmed","cancelled", 
                   "firstRegistration", "lastRegistration")


getInfo <- function(path){
  headerBasic <- c("event", "date", "time", "presenter", "location", "campus", "seats")
  info <- read.csv(file = path, header = FALSE, nrows=8)
  col <- t(info)[2,]
  info_df <- data.frame()
  info_df <- rbind(info_df, col)
  info_df <- subset(info_df, select = -c(X..))
  colnames(info_df) <- headerBasic
  return(info_df)
}




#Waitlist
# /Waitlist/lc_wait_list_423622520180829074157.csv 
#pt 8 line then skip 9 line for pt2
waitlistInfo <- read.csv(file = "Waitlist/lc_wait_list_423622520180829074157.csv", header = FALSE, nrows=8)
waitlistPpl <- read.csv(file = "Waitlist/lc_wait_list_423622520180829074157.csv", header = TRUE, skip = 9)

#Basic Info
col <- t(waitlistInfo)[2,]
basicInfo_df <- data.frame()
basicInfo_df <- rbind(basicInfo_df, col)
basicInfo_df <- subset(basicInfo_df, select = -c(X..))
list <- c('waitlist')
category <- c('dsi')
basicInfo_df <- cbind(list, category, basicInfo_df)
colnames(basicInfo_df) <- headerBasic

#BASIC INFO

#Ppl df
ppl_df <- data.frame()
list <- rep('waitlist',time=length(waitlistPpl$First.Name))
category <- rep('dsi',time=length(waitlistPpl$First.Name))
ppl_df <- cbind(list, category, waitlistPpl)
colnames(ppl_df) <- headerWL

#WAITLIST FUNCTION
addWL <- function(pathStr, category){
  print('IN WAITLIST')
  wlInfo <- read.csv(file = pathStr, header = TRUE, skip = 9)
  wl_df <- data.frame()
  list <- rep('waitlist',time=length(wlInfo$First.Name))
  category <- rep(category,time=length(wlInfo$First.Name))
  wl_df <- cbind(list, category, wlInfo)
  if(ncol(wl_df) == 6){
    colnames(wl_df) <- headerWL2
    nas <- rep(NA, time=length(wlInfo$First.Name))
    wl_df <- cbind(wl_df, nas, nas, nas, nas, nas)
  }else if(ncol(wl_df) == 8){
    nas <- rep(NA, time=length(wlInfo$First.Name))
    wl_df <- cbind(wl_df, nas, nas,nas)
  } 
  colnames(wl_df) <- headerWL
  print(wl_df)
  print('---------------------')
  df <- getInfo(pathStr)
  print(df)
  if(nrow(wl_df) == 0){
    print('in here')
    return(NULL)
  }
  print(ncol(wl_df))
  wl_df <- cbind(wl_df,df)
  print(ncol(wl_df))
  print('here4')
  return(wl_df)
}
#"First Name","Last Name","Email","Booking made"
#"First Name","Last Name","Email","Booking made","Status","School","Department","Questions/Comments:"
test <- addWL("/Users/jencruz/Downloads/UCSF_LibCal/Data/MakersLab/waitlist/lc_wait_list_384810520180906110854.csv", "dsi")


#Registration
df <- getInfo("Registrations/lc_attendees_365604420180829073527.csv")
# Registrations/lc_attendees_365604420180829073527.csv
regInfo <- read.csv(file = "Registrations/lc_attendees_365604420180829073527.csv",header = TRUE, skip = 9)
reg_df <- data.frame()
list <- rep('registration',time=length(regInfo$First.Name))
category <- rep('dsi',time=length(regInfo$First.Name))
reg_df <- cbind(list, category, regInfo)
colnames(reg_df) <- headerReg

#REGISTRATION FUNCTION
addReg <- function(pathStr, category){
  print('hello')
  regInfo <- read.csv(file = pathStr, header = TRUE, skip = 9)
  reg_df <- data.frame()
  list <- rep('registration',time=length(regInfo$First.Name))
  category <- rep(category,time=length(regInfo$First.Name))
  reg_df <- cbind(list, category, regInfo)
  if(ncol(reg_df) == 7){
    print('hey')
    colnames(reg_df) <- headerReg2
    nas <- rep(NA, time=length(regInfo$First.Name))
    reg_df <- cbind(reg_df, nas, nas, nas, nas, nas)
  }else if(ncol(reg_df)==11){
    print('hey2')
    colnames(reg_df) <- headerReg3
    reg_df <- add_column(reg_df, phoneNumber = c(NA), .after = "attendance")
  }else if(ncol(reg_df)==9){
    print('hey2')
    nas <- rep(NA, time=length(reg_df$First.Name))
    reg_df <- cbind(reg_df, nas, nas)
    reg_df <- add_column(reg_df, phoneNumber = c(NA), .after = "Attendance")
  }
  print(ncol(reg_df))
  if(ncol(reg_df)!=13){
    print('hey3')
    print(reg_df)
    print(headerReg4)
    colnames(reg_df) <- headerReg4
    reg_df <- add_column(reg_df, question2 = c(NA), .after = "question1")
  }
  if(nrow(reg_df) == 0){
    print('in here')
    return(NULL)
  }
  df <- getInfo(pathStr)
  reg_df <- cbind(reg_df,df)
  print(reg_df)
  print(headerReg)
  colnames(reg_df) <- headerReg
  print(reg_df)
  print(ncol(reg_df))
  #print(nrow(reg_df))
  return(reg_df)
}
#"First Name","Last Name","Email","Booking made","Attendance"
#"attendance", "phoneNumber", "status", "school", "dept", "question"

test <- addReg("/Users/jencruz/Downloads/UCSF_LibCal/Data/MakersLab/registration/lc_attendees_384809620180906110835.csv", "dsi")

#Cancelled
# /Cancelled/lc_cancelled_396694020180829073959.csv
cancInfo <- read.csv(file = "Cancelled/lc_cancelled_396694020180829073959.csv",header = TRUE, skip = 9)
canc_df <- data.frame()
list <- rep('cancelled',time=length(cancInfo$First.Name))
category <- rep('dsi',time=length(cancInfo$First.Name))
canc_df <- cbind(list, category, cancInfo)
colnames(canc_df) <- headerCanc

#CANCELLED FUNCTION
addCanc <- function(pathStr, category){
  print("START___________________")
  cancInfo <- read.csv(file = pathStr, header = TRUE, skip = 9)
  print(cancInfo)
  if(nrow(cancInfo) != 0){
    canc_df <- data.frame()
    list <- rep('cancelled',time=length(cancInfo$First.Name))
    category <- rep(category,time=length(cancInfo$First.Name))
    canc_df <- cbind(list, category, cancInfo)
    #n <- nrow(canc_df)
    colnames(canc_df) <- headerCanc
    df <- getInfo(pathStr)
    print("num1------------")
    print(nrow(canc_df))
    canc_df <- cbind(canc_df,df)
    return(canc_df)
  }
  return(NULL)
}

test <- addCanc("Cancelled/lc_cancelled_396694020180829073959.csv", "dsi")


#Event
# /OverviewStats/lc_event_20180829073839.csv
eventInfo <- read.csv(file = "OverviewStats/lc_event_20180829073839.csv", header = FALSE)
col <- t(eventInfo)[2,]
event_df <- data.frame()
event_df <- rbind(event_df, col)
event_df <- subset(event_df, select = -c(X..))
list <- c('event')
category <- c('dsi')
event_df <- cbind(list, category, event_df)
colnames(event_df) <- headerEvent

#EVENT FUNCTION
addEvent <- function(pathStr, category){
  eventInfo <- read.csv(file = pathStr, header = FALSE)
  print(eventInfo)
  col <- t(eventInfo)[2,]
  event_df <- data.frame()
  event_df <- rbind(event_df, col)
  event_df <- subset(event_df, select = -c(X..))
  list <- c('event')
  category <- c('dsi')
  print(event_df)
  event_df <- cbind(list, category, event_df)
  print(ncol(event_df))
  if(ncol(event_df) == 13){
    colnames(event_df) <- headerEvent2
    event_df <- add_column(event_df, attendance = c(NA), .after = "categories")
  }else if(ncol(event_df) == 7){
    colnames(event_df) <- headerEvent3
    print('EVEVEVEVEVEENTTTT')
    print(event_df)
    nas <- rep(NA, time=length(event_df$date))
    print(nas)
    event_df <- add_column(event_df, presenter = c(NA), .after = "time")
    print(event_df)
    event_df <- cbind(event_df, nas, nas, nas, nas, nas, nas)
  }else if(ncol(event_df) == 12){
    colnames(event_df) <- headerEvent4
    event_df <- add_column(event_df, presenter = c(NA), .after = "time")
    event_df <- add_column(event_df, attendance = c(NA), .after = "categories")
  }else if(ncol(event_df) == 8){
    print('IN HERE')
    colnames(event_df) <- headerEvent3
    nas <- rep(NA, time=length(event_df$date))
    print(nas)
    event_df <- cbind(event_df, nas, nas, nas, nas, nas, nas)
  }
  if(nrow(event_df) == 0){
    print('in here')
    return(NULL)
  }
  colnames(event_df) <- headerEvent
  return(event_df)
}

test <- addEvent("/Users/jencruz/Downloads/UCSF_LibCal/Data/MakersLab/event/lc_event_20180906111556.csv", "dsi")
