library(stringr)
library(dplyr)

data <- read.csv(file = "campaign_Education_Research_February_2018_New_Emails__Apr_3_2018.csv", header = FALSE)
data1 <- read.csv(file = "campaign_Education_Research_February_2018__Apr_3_2018.csv", header = FALSE)
allData <- read.csv(file= "~/Downloads/UCSF/May_11_2018_campaigns.csv", header = T)

getData <- function(df,str){
  data <- df %>% filter(str_detect(Title, str))
  return(data)
}


createNewDataFrame <- function(df){
  #Title (1), Date, Time, Clicks, Opens, Total Clicks, Total Opens, Top links
  id <- c()
  date <- c()
  time <- c()
  for(i in 1:length(df$Send.Date)){
    str_date <- df[i,4]
    d <- substring(str_date,1,12)
    date <- append(date, d)
    t <- substring(str_date,14,21)
    time <- append(time, t)
    # idStr <- substr(df$Title[i],1,4)
    # id <- append(id, idStr)
  }
  
  final.df <- data.frame(Title = df$Title, Date = date, Time = time, TotalRecipients = df$Total.Recipients,
                         UniqueOpens = df$Unique.Opens, TotalOpens = df$Total.Opens, OpenRate = df$Open.Rate,
                         UniqueClicks = df$Unique.Clicks, TotalClicks = df$Total.Clicks, ClickRate = df$Click.Rate)
  return(final.df)
  
}

cleanFile <- function(df){
  title <- df$V2[2]
  dateAndTime <- df$V2[4]
  dateStr <- substring(dateAndTime, 6, 17)
  timeStr <- substring(dateAndTime,19,26)
  cleanData <- data.frame()
  idx <- which(df$V1 == "Unique Clicks") + 1
  for(i in seq(idx, length(df$V1),2)){
      urlStr <- df$V1[i]
      totalNum <- df$V2[i]
      uniqNum <- df$V1[i+1]
      temp <- data.frame(Title = title, Date = dateStr, Time = timeStr,
                          URL = urlStr, totalUrlClicks = totalNum, 
                          uniqueUrlClicks = uniqNum)
      cleanData <- rbind(cleanData, temp)
  }
  return(cleanData)
}

getLinkData <- function(path){
  setwd(path)
  allData <- data.frame()
  file.names <- dir(path, pattern = ".csv")
  for(i in 1:length(file.names)){
    file <- read.csv(file.names[i], header = FALSE)
    y <- cleanFile(file)
    allData <- rbind(allData, y)
  }
  return(allData)
}

addId <- function(df,id){
  vect <- rep(id, length(df$Time))
  df <- cbind(Id = vect, df)
  return(df)
}

#main
#ALL DATA
#all data from csv 17-18
#data17to18 <- allData[1:189,]
#making data frame containing the Title, Date, Time, Unique Opens/Total Opens, Unique Clicks/Total Clicks
#df <- createNewDataFrame(data17to18)
df <- createNewDataFrame(allData)
#Library Notes emails
libraryNotesData <- getData(df, "Library Notes")
libraryNotesData <- addId(libraryNotesData, "LIBR")
#Education & Research emails
eduAndResearchData <- getData(df, "Education & Research")
eduAndResearchData <- addId(eduAndResearchData, "EDUC")
#Makers Lab News emails
makersLabData <- getData(df, "Makers Lab")
makersLabData <- addId(makersLabData, "MAKE")
#Data Science Events emails
dataScienceData <- getData(df, "Data Science")
dataScienceData <- addId(dataScienceData, "DATA")

#INDIVIDUAL FILES (getting links data)
#Library Notes links data
#libraryNotesLinkData <- getLinkData("~/Downloads/LibraryNotesReports/")
libraryNotesLinkData <- getLinkData("~/Downloads/UCSF/UCSF_LibNotes/")
libraryNotesLinkData <- addId(libraryNotesLinkData, "LIBR")
#Education & Research links data
#eduAndResearchLinkData <- getLinkData("~/Downloads/EduAndResearchReports/")
eduAndResearchLinkData <- getLinkData("~/Downloads/UCSF/UCSF_EduAndResearch/")
eduAndResearchLinkData <- addId(eduAndResearchLinkData, "EDUC")
#Makers Lab News links data
#makersLabLinkData <- getLinkData("~/Downloads/MakersLabReports/")
makersLabLinkData <- getLinkData("~/Downloads/UCSF/UCSF_MakersLab/")
makersLabLinkData <- addId(makersLabLinkData, "MAKE")
#Data Science Events links data
#dataScienceLinkData <- getLinkData("~/Downloads/DataScienceReports/")
dataScienceLinkData <- getLinkData("~/Downloads/UCSF/UCSF_DataScience/")
dataScienceLinkData <- addId(dataScienceLinkData, "DATA")

# write.csv(libraryNotesData, "~/Downloads/emailData/libraryNotesData.csv")
# write.csv(eduAndResearchData, "~/Downloads/emailData/eduAndResearchDataa.csv")
# write.csv(makersLabData, "~/Downloads/emailData/makersLabData.csv")
# write.csv(dataScienceData, "~/Downloads/emailData/dataScienceData.csv")

write.csv(libraryNotesData, "~/Downloads/UCSF/UCSF_data/libraryNotesData.csv")
write.csv(eduAndResearchData, "~/Downloads/UCSF/UCSF_data/eduAndResearchDataa.csv")
write.csv(makersLabData, "~/Downloads/UCSF/UCSF_data/makersLabData.csv")
write.csv(dataScienceData, "~/Downloads/UCSF/UCSF_data/dataScienceData.csv")

# write.csv(libraryNotesLinkData, "~/Downloads/emailData/libraryNotesLinkData.csv")
# write.csv(eduAndResearchLinkData, "~/Downloads/emailData/eduAndResearchLinkData.csv")
# write.csv(makersLabLinkData, "~/Downloads/emailData/makersLabLinkData.csv")
# write.csv(dataScienceLinkData, "~/Downloads/emailData/dataScienceLinkData.csv")

write.csv(libraryNotesLinkData, "~/Downloads/UCSF/UCSF_data/libraryNotesLinkData.csv")
write.csv(eduAndResearchLinkData, "~/Downloads/UCSF/UCSF_data/eduAndResearchLinkData.csv")
write.csv(makersLabLinkData, "~/Downloads/UCSF/UCSF_data/makersLabLinkData.csv")
write.csv(dataScienceLinkData, "~/Downloads/UCSF/UCSF_data/dataScienceLinkData.csv")

finalData <- rbind(dataScienceData, eduAndResearchData, libraryNotesData, makersLabData)
write.csv(finalData, "~/Downloads/UCSF/UCSF_data/finalData.csv")
finalLinkData <- rbind(dataScienceLinkData, eduAndResearchLinkData, libraryNotesLinkData, makersLabLinkData)
zwrite.csv(finalLinkData, "~/Downloads/UCSF/UCSF_data/finalLinkData.csv")
