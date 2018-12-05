library(stringr)
library(dplyr)
library(tibble)
#Check in what directory you're in
getwd()
#setwd("/Users/jencruz/Downloads/UCSF_LibCal/Data/Education&Research/")
setwd("/Users/jencruz/GoogleDrive")

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

checkColNames <- function(df1, df2){
  x <- colnames(df1)
  y <- colnames(df2)
  remove <- y[!(y %in% x)]
  rmVector <- c()
  if(length(remove) != 0){
    for(i in 1:length(remove)){
      idx <- which(remove[i] == y)
      rmVector <- append(rmVector, idx)
    }
    df2 <- df2[,-rmVector]
  }
  return(df2)
}


correctCols <- function(df1, df2){
  df3 <- data.frame(X=NA)
  df2 <- checkColNames(df1, df2)
  count <- 1
  for(i in 1:ncol(df1)){
    if(count != (ncol(df2)+1)){
      
      if(identical(colnames(df1[i]),colnames(df2[count]))){
        df3 <- cbind(df3,df2[count])
        count <- count + 1
      }else{
        if(colnames(df2[count]) %in% colnames(df1)){
          df3 <- cbind(df3, c(NA))
        }
      }
    }
  }
  df2 <- subset(df3, select=-c(X))
  if(ncol(df1)>ncol(df2)){
    x <- ncol(df2) + 1
    df2 <- cbind(df2, df1[,x:ncol(df1)])
  }
  colnames(df2) <- colnames(df1)
  return(df2)
}

addList <- function(pathStr, category, list, set_df){
  ppl <- read.csv(file = pathStr, header = TRUE, skip = 9)
  if(identical(list, "registration") && ncol(ppl) > 9){
    ppl <- ppl[,1:9]
  }else if(identical(list, "waitlist") && ncol(ppl) > 9){
    ppl <- ppl[,1:9]
  }
  if(nrow(ppl) == 0){
    return(NULL)
  }else{
    if(!identical(colnames(set_df), colnames(ppl))){
      ppl <- correctCols(set_df, ppl)
    }
    ppl <- cbind(category, list, ppl)
  }
  colnames(ppl) <- colnames(set_df)
  return(ppl)
}

canc_header <- c("list", "category", "first", "last", "email", "booking")
canc_df <- data.frame(First.Name = NA, Last.Name = NA, Email = NA, Booking.cancelled = NA)
reg_header <- c("list", "category", "first", "last", "email", "booking",
               "attendance", "phoneNumber", "status", "school", "dept", "event", 
               "date", "time", "presenter", "location", "campus", "seats") #deleted questions, useless on tableau?
reg_df <- data.frame(First.Name = NA, Last.Name = NA, Email = NA, Booking.made = NA,
                      Attendance = NA, Primary.Campus = NA, Status = NA, School = NA, Department = NA)
wl_header <- c("list", "category", "first", "last", "email", "booking", "primaryCampus", "phoneNumber", "status", "school", "dept")
wl_df <- data.frame(First.Name = NA, Last.Name = NA, Email = NA, Booking.made = NA, Primary.Campus = NA, Phone.Number = NA,
                   Status = NA, School = NA, Department = NA)
event_header <- c("list", "category", "date", "time", "presenter", "location", "campus",
                  "categories", "attendance","registration", "confirmed","cancelled", 
                  "firstRegistration", "lastRegistration")
event_df <- data.frame(Date = NA, Time = NA, Presenter = NA, Location = NA, Campus = NA, Categories = NA,
                       ActualAttendance = NA, Registrations = NA, ConfirmedAttendance = NA,
                       CancelledRegistrations = NA, FirstRegistration = NA, LastRegistration = NA)


trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  
rmSpaceColNames <- function(df){
  names <- colnames(df)
  newNames <- c()
  for(i in 1:ncol(df)){
    add <- gsub(" ", "", names[i], fixed = TRUE)
    newNames <- append(newNames, add)
  }
  df <- setNames(df, newNames)
  return(df)
}  

  
  #EVENT FUNCTION
addEvent <- function(pathStr, category, list, df1){
    eventInfo <- read.csv(file = pathStr, header = FALSE)
    col <- t(eventInfo)[2,]
    event_df <- data.frame()
    info <- t(eventInfo)[2,]
    info <- info[2:length(info)]
    newNames <- t(eventInfo)[1,]
    newNames <- newNames[2:length(newNames)]
    event_df <- rbind(event_df, info)
    event_df <- setNames(event_df, newNames)
    event_df <- rmSpaceColNames(event_df)
    list <- c(list)
    category <- c(category)
    event_df <- correctCols(df1, event_df)
    event_df <- cbind(list, category, event_df)
    if(nrow(event_df) == 0){
      return(NULL)
    }
    return(event_df)
  }
  





#dir <- list.files("/Users/jencruz/Downloads/UCSF_LibCal/Data")
dir <- list.files("/Users/jencruz/GoogleDrive/UCSF_Projects/UCSF_All/All/")
waitlist <- data.frame()
registration <- data.frame()
cancelled <- data.frame()
event <- data.frame()
for(i in 2:length(dir)){
  folder <- trim(dir[i])
  #path <- paste("/Users/jencruz/Downloads/UCSF_LibCal/Data/", dir[i])
  path <- paste("/Users/jencruz/GoogleDrive/UCSF_Projects/UCSF_All/All/", dir[i])
  path <- gsub(" ", "", path, fixed = TRUE)
  dir2 <- list.files(path)
  for(j in 1:length(dir2)){
    subfolder <- dir2[j]
    path2 <- paste(path,'/',dir2[j])
    path2 <- gsub(" ", "", path2, fixed = TRUE)
    dir3 <- list.files(path2)
    for(l in 1:length(dir3)){
      path3 <- paste(path2,'/',dir3[l])
      finalPath <- gsub(" ", "", path3, fixed = TRUE)
      finalPath <- trim(finalPath)
      if(identical(subfolder,"cancelled")){
        for(x in 1:length(finalPath)){
          df <- addList(finalPath,folder, subfolder, canc_df)
          if(!is.null(df)){
            colnames(df) <- canc_header
            cancelled <- rbind(cancelled, df)
          }
        }
      }else if(identical(subfolder,"registration")){
        for(x in 1:length(finalPath)){
          eInfo <- getInfo(finalPath)
          df <- addList(finalPath, folder, subfolder, reg_df)
          if(!is.null(df)){
            final_df <- cbind(df, eInfo)
            final_df <- final_df[,1:18]
            colnames(final_df) <- reg_header
            registration <- rbind(registration, final_df)
          }
        }
      }else if(identical(subfolder,"waitlist")){
        for(x in 1:length(finalPath)){
          df <- addList(finalPath,folder, subfolder, wl_df)
          if(!is.null(df)){
            df <- df[,1:11]
            colnames(df) <- wl_header
            waitlist <- rbind(waitlist, df)
          }
        }
      }else if(identical(subfolder,"event")){
        for(x in 1:length(finalPath)){
          df <- addEvent(finalPath,folder, subfolder, event_df)
          if(!is.null(df)){
            colnames(df) <- event_header
            event <- rbind(event, df)
          }
        }
      }  
    } 
  }
}  

library(stringr)

fixDate <- function(df){
  bookingTime <- c()
  bookingDate <- c()
  bookingDay <- c()
  for(i in 1:length(df$booking)){
    idx1 <- unlist(gregexpr(pattern ='m',df$booking[i]))[1]
    t <- substr(df$booking[i], 1, idx1)
    idx2 <- unlist(gregexpr(pattern =',', df$booking[i]))[1]+2
    day <- substr(df$booking[i], 1, idx2-3)
    str <- toString(df$booking[i])
    end <- nchar(str)
    d <- substr(str, idx2, end)
    d <- as.Date(d, format='%B %d, %Y')
    bookingTime <- append(bookingTime, t)
    bookingDate <- append(bookingDate, d)
    bookingDay <- append(bookingDay, day)
  }
  df2 <- subset(df, select = -c(booking))
  df2 <- cbind(df2, bookingTime, bookingDay, bookingDate)
  return(df2)
} 

fixDate2 <- function(df){
  startTime <- c()
  endTime <- c()
  bookingDate <- c()
  bookingDay <- c()
  for(i in 1:length(df$time)){
    strTime <- toString(df$time[i])
    if(str_detect(strTime, "All Day Event")){
      startTime <- append(startTime, "ADE")
      endTime <- append(endTime, "ADE")
    }else{
      time_nChar <- nchar(strTime)
      idx1 <- unlist(gregexpr(pattern ='m',df$time[i]))[1]
      start_t <- substr(strTime, 1, idx1)
      end_t <- substr(strTime, idx1+3, time_nChar)
      startTime <- append(startTime, start_t)
      endTime <- append(endTime, end_t)
    }
    
    idx2 <- unlist(gregexpr(pattern =',', df$date[i]))[1]+2
    day <- substr(df$date[i], 1, idx2-3)
    
    dateStr <- toString(df$date[i])
    date_nChar <- nchar(dateStr)
    d <- substr(dateStr, idx2, date_nChar)
    d <- as.Date(d, format='%B %d, %Y')
    
    bookingDate <- append(bookingDate, d)
    bookingDay <- append(bookingDay, day)
  }
  df2 <- subset(df, select = -c(time, date))
  date <- df$date
  time <- df$time
  df2 <- cbind(df2, startTime, endTime, bookingDay, bookingDate, date, time)
  return(df2)
}

registration <- fixDate(registration)
waitlist <- fixDate(waitlist)
cancelled <- fixDate(cancelled)
event2 <- fixDate2(event)

#time label 
labelTime <- function(df){
  timeLabel <- c()
  for(i in 1:length(df$list)){
    start <- grepl("am", df$startTime[i])
    end <- grepl("am", df$endTime[i])
    ade <- grepl("ADE", df$endTime[i])
    if(start == TRUE && end == TRUE && ade == FALSE){
      timeLabel <- append(timeLabel, "AM")
    }else if(start == FALSE && end == FALSE && ade == FALSE){
      timeLabel <- append(timeLabel, "PM")
    }else{
      timeLabel <- append(timeLabel, "BOTH")
    }
  }
  return(cbind(df,timeLabel))
}
event2 <- labelTime(event2)

#seperate by cateory
categories <- function(df){
  df2 <- data.frame()
  for(i in 1:length(df$list)){
    #print("**********NEW EVENT**********")
    row <- df[i,]
    #print(length(row))
    row$categories <- str_replace_all(row$categories, " ", "")
    #print(rCat)
    #print(charmatch(",",row$categories))
    if(grepl(",", row$categories)){
      #rcat <- str_replace_all(row$categories, " ", "")
      catList <- strsplit(row$categories, ",")
      #print(catList)
     # print(length(catList[[1]]))
      for(j in 1:length(catList[[1]])){
       # print("--------new row---------")
        row$categories <- catList[[1]][j]
        #print(row)
        df2 <- rbind(df2, row)
      }
    }else{
      df2 <- rbind(df2, row)
    }
  }
  return(df2)
}

getCategories <- function(df, df2){
  check <- c()
  categories <- c()
  #print("hello")
  for(i in 1:length(df$list)){
    p <- df[i,]
    #print("--------NEW---------")
    if(!is.na(p$presenter) && !is.na(p$location)){
      #print("if")
      r <- which( as.character(p$date) == as.character(df2$date)
                  &  as.character(p$presenter) == as.character(df2$presenter)
                  &  as.character(p$time) == as.character(df2$time) 
                  &  as.character(p$location) == as.character(df2$location))
      #print(r[1])
    }else if(is.na(p$location) && is.na(p$presenter)){
      #check <- append(check, i)
      r <- which( as.character(p$date) == as.character(df2$date)
                  #& identical(p$presenter, df2$presenter)
                  &  is.na(p$presenter) == is.na(df2$presenter)
                  &  as.character(p$time) == as.character(df2$time) 
                  &  is.na(p$location) == is.na(df2$location))
                  #&  identical(p$location, df2$location))
      #print(r[1])
    }else if(is.na(p$location)){
      
      r <- which( as.character(p$date) == as.character(df2$date)
                  &  as.character(p$presenter) == as.character(df2$presenter)
                  &  as.character(p$time) == as.character(df2$time) 
                  &  is.na(p$location) == is.na(df2$location))
                  #&  identical(p$location, df2$location))
    }else{
      check <- append(check, i)
      r <- which( as.character(p$date) == as.character(df2$date)
                  &  is.na(p$presenter) == is.na(df2$presenter)
                  #& identical(p$presenter, df2$presenter)
                  &  as.character(p$time) == as.character(df2$time)
                  &  as.character(p$location) == as.character(df2$location))
    }
    r <- r[1]
    if(identical(r, integer(0))){
      c <- NA
      categories <- append(categories, c)
    }else{
      c <- event$categories[r]
      categories <- append(categories, as.character(c))
    }
  }
  df <- cbind(df, categories)
  print(check)
  return(df)
}
# registration$presenter <- as.character(registration$presenter)
# registration$presenter[registration$presenter==""] <- NA
# registration$presenter <- as.factor(registration$presenter)
# 
# registration$attendance <- as.character(registration$attendance)
# registration$attendance[registration$attendance==" "] <- NA
# registration$attendance[registration$attendance=="-"] <- NA
# registration$attendance <- as.factor(registration$attendance)

cleanNAs <- function(col){
  col <- as.character(col)
  col[col==""] <- NA
  col[col=="-"] <- NA
  col <- as.factor(col)
  return(col)
}


regSample <- registration[,1:18]

regSample$presenter <- cleanNAs(regSample$presenter)
regSample$attendance <- cleanNAs(regSample$attendance)
regSample$status <- cleanNAs(regSample$status)
regSample$school <- cleanNAs(regSample$school)
regSample$dept <- cleanNAs(regSample$dept)
regSample$location <- cleanNAs(regSample$location)
event2$location <- cleanNAs(event2$location)
event2$firstRegistration <- cleanNAs(event2$firstRegistration)
event2$lastRegistration <- cleanNAs(event2$lastRegistration)

registration1 <- unique(regSample)

eventSample <- event2[,1:19]
event3 <- unique(eventSample)
rownames(event3) <- seq(length=nrow(event3))
rownames(registration1) <- seq(length=nrow(registration1))

registration2 <- getCategories(registration1, event3)

event4 <- categories(event3)
registration3 <- categories(registration2)

write.csv(registration2, file = "registration.csv")
write.csv(waitlist, file = "waitlist.csv")
write.csv(cancelled, file = "cancelled.csv")
write.csv(event3, file = "event.csv")
write.csv(registration3, file = "registrationPerCategory.csv")
write.csv(event4, file = "eventPerCategory.csv")

#get only classes that took atendance
regSubset <- filter(registration3, attendance == "Yes" | attendance == "No")
eventSubset <- filter(event4, !is.na(attendance))
write.csv(regSubset, file = "regSubset.csv")
write.csv(eventSubset, file = "eventSubset.csv")