library(stringr)
library(dplyr)
#Check in what directory you're in
getwd()

#Set working directory where file is
setwd("/Users/jencruz/GoogleDrive/UCSF_Projects/UCSF_All")

#Read in relevant file(s)
calStats <- read.delim(file = "lc_calendar_stats_20181018040150.csv", sep="\t", header = FALSE)
eventStats <- read.delim(file = "lc_events_stats_20181018040211.csv", sep="\t", header = FALSE)

#Function that creates dataframe from selected rows given
create_df <- function(header, title, numRows, rows){
  df <- data.frame()
  for(i in 2:numRows){
    split <- unlist(strsplit(as.character(rows[i]), ","))
   # print(split)
    splitRow <- c()
    for(j in 1:length(split)){
      item <- split[j]
      splitRow <- append(splitRow, item)
    }
    if(i == 1){
      df <- rbind(df, splitRow)
      colnames(df) <- header
    }else{
      df2 <- data.frame()
      df2 <- rbind(df2, splitRow)
      colnames(df2) <- header
      df <- rbind(df, df2)
    }
  }
  return(df)
}

create_df2 <- function(numRows, rows){
  header <- c("Distribution","Month", "Year")
  for(i in 2:numRows){
    split <- unlist(strsplit(as.character(rows[i]), ","))
    
    h <- gsub(" ","",split[1])
    h <- gsub("\\W", "",toString(h))
    header <- append(header,h)
    split <- c(split[-1])
    
    if(i == 2){
      #print(split)
      df <- data.frame(rep(rows[1],length(split)))
      # print(df)
      splitMY <- data.frame(strsplit(split, " "))
      #print(splitMY)
      df <- cbind(df,t(splitMY[1,]))
      df <- cbind(df,t(splitMY[2,]))
      #print(ncol(df))
      print(df)
   }else{
      df <- cbind(df,c(split))
   }
  }
  colnames(df) <- header[-4]
  #print(ncol(df))
  return(df)
}

create_df3 <- function(numRows, rows){
  header <- c("Distribution")
  for(i in 2:numRows){
    split <- unlist(strsplit(as.character(rows[i]), ","))
    
    h <- gsub(" ","",split[1])
    h <- gsub("\\W", "",toString(h))
    header <- append(header,h)
    split <- c(split[-1])
    
    if(i == 2){
      df <- data.frame(rep(rows[1],length(split)))
      df <- cbind(df,split)
    }else{
      df <- cbind(df,c(split))
    }
  }
  colnames(df) <- header
  return(df)
}

#Create Summary dataframe (df)
summary <- calStats[1:5,]
summaryHeader <- c("category","total")
summary_df <- create_df(summaryHeader, "summary18", length(summary), summary)
write.csv(summary_df, file = "summary1518.csv")

#Create Month/Year df
monthYearDist <- calStats[6:11,]
monthYearDist_df <- create_df2(length(monthYearDist), monthYearDist)
rownames(monthYearDist_df) <- seq(length=nrow(monthYearDist_df))

#Day of Week df
dayDist <- calStats[12:17,]
dayDist_df <- create_df3(length(dayDist), dayDist)
rownames(dayDist_df) <- seq(length=nrow(dayDist_df))
write.csv(dayDist_df, file = "dayDistribution.csv")

#Hour of Day df
hourDist <- calStats[18:23,]
hourDist_df <- create_df3(length(hourDist), hourDist)
rownames(hourDist_df) <- seq(length=nrow(hourDist_df))
write.csv(hourDist_df, file - "hourDistribution.csv")

#dailyHour df
dailyHourDist <- calStats[24:32,]
dailyHourtDist_df <- create_df3(length(dailyHourDist), dailyHourDist)
rownames(dailyHourtDist_df) <- seq(length=nrow(dailyHourtDist_df))
write.csv(dailyHourtDist_df, file = "dailyHourDistribution.csv")

#month/year df
monthYear_Campus <- calStats[33:40,]
monthYear_CampusDist_df <- create_df2(length(monthYear_Campus), monthYear_Campus)
rownames(monthYear_CampusDist_df) <- seq(length=nrow(monthYear_CampusDist_df))
write.csv(monthYear_Campus, file - "campusDistribution.csv")

#monthyear category*
monthYear_Cat <- calStats[41:79,]
monthYear_Cat_df <- create_df2(length(monthYear_Cat), monthYear_Cat)
rownames(monthYear_Cat_df) <- seq(length=nrow(monthYear_Cat_df))
write.csv(monthYear_Cat_df, file = "categoryDistribution.csv")

#monthYear Lib
myLib <- calStats[81:86,]
myLib_df <- create_df2(length(myLib), myLib)
rownames(myLib_df) <- seq(length=nrow(myLib_df))


#SFGH Classes Monthly Distribution
sfgh <- calStats[87:92,]
sfgh_df <- create_df2(length(sfgh), sfgh)
rownames(sfgh_df) <- seq(length=nrow(sfgh_df))

#coursera - diabetes lecture production monthly dist
coursera <- calStats[93:98,]
coursera_df <- create_df2(length(coursera), coursera)
rownames(coursera_df) <- seq(length=nrow(coursera_df))

#lib custom classes monthly dist
custom <- calStats[99:104,]
custom_df <- create_df2(length(custom), custom)
rownames(custom_df) <- seq(length=nrow(custom_df))

#Library Stats - Curriculum/Special/Orientation/Fairs monthly dist
libstat <- calStats[105:110,]
libstat_df <- create_df2(length(libstat), libstat)
rownames(libstat_df) <- seq(length=nrow(libstat_df))

#GME Diversity Upcoming Events Monthly dist
div <- calStats[111:116,]
div_df <- create_df2(length(div), div)
rownames(div_df) <- seq(length=nrow(div_df))

#UCSF Graduate Medical Education Calender Monthly dist
grad <- calStats[117:122,]
grad_df <- create_df2(length(grad), grad)
rownames(grad_df) <- seq(length=nrow(grad_df))

#Combining df
df <- rbind(grad_df, div_df, libstat_df, custom_df, coursera_df, sfgh_df, myLib_df, monthYearDist_df)
write.csv(df, file = "monthDistributions.csv")
