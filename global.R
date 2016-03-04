library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(data.table)
library(xts)
library(shinydashboard)
library(scales)
library(devtools)
library(dygraphs)
library(readxl)

### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

# install missing packages.
list.of.packages <- c("shiny","ggplot2","reshape2","plyr","lubridate","data.table","dygraphs","xts","devtools","shinydashboard","scales",'dygraphs','readxl')
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

#global functions
alt.diff <- function (x, n = 1, na.pad = TRUE) {
  NAs <- NULL
  if (na.pad) {NAs <- rep(NA, n)}
  diffs <- c(NAs, diff(x, n))
}

round.minutes <- function(x, noOfMinutes=5){
	tz <- tz(x[1])
	class <- class(x[1])
	structure((noOfMinutes*60) * (as.numeric(x + (noOfMinutes*60*0.5)) %/% (noOfMinutes*60)), class=class,tz=tz)
}

read.sum <- function(file, fname,tzone="GMT"){
	fileCheck <- file.info(file)$size>0
	if(fileCheck){
	sums <- fread(file)
	names(sums)[1:3]  <- c('datetime','temp','serial')
	sums[,datetime:=ymd_hms(datetime, tz="Africa/Accra")]
	}else{warning(paste("File", file, "does not contain valid iButton data", sep=" "))}
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#check the OS
OS <- Sys.info()[['sysname']]
if(OS == 'Windows'){path_to_dropbox <- paste(Sys.getenv('USERPROFILE'),'\\Dropbox',sep="")} else
if(OS =='Darwin'){path_to_dropbox <- paste("~/Dropbox")}else(warning("Not Windows or Mac"))


#create the data
files <- list.files(paste(path_to_dropbox, "/LPG_Study/archive", sep=""), full.names=T, recursive=T)
files <- grep('attributes', files, value=T, invert=T)

#data prep
log.sheet <- read_excel(paste(path_to_dropbox, '/LPG_Study/logsheets/USAID_iSUMS_logsheet.xlsx', sep=""))[,1:5]
log.sheet <- as.data.table(log.sheet)
setnames(log.sheet, c('community','device_id','mid','location','deploy_dt'))
log.location <- read_excel(paste(path_to_dropbox, '/Ghana_adoption_data_SHARED/Stove_use_protocol/SUMS_logsheet_draft_2015-07-2015.xlsx', sep=""), sheet=2)[,4:5]
log.location <- as.data.table(log.location)
setnames(log.location, 1:2, c('location', 'description'))
log.location[,description:=gsub(" - ", "_",description)]
log.location[,description:=gsub(" #", "_",description)]
log.location[,description:=gsub(" ", "_",description)]
log.location[,description:=tolower(description)]
log.location <- log.location[!is.na(location)]
log.sheet