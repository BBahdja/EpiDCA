#Get library

library(tidyverse)
library(dplyr)
library(lubridate)
library(geosphere)
library(MLmetrics)
library(caret)

#Get Functions to clean data
#Avec cette fonction on appelle le script (cleaningfunction)
#tu peux aussi le faire tourner ligne par ligne

source("C:/Users/el-bahdja.boudoua/Desktop/CleaningFunctionsforHM.R")

######################
# import data
######################
EBSdatabase <- read.csv2("C:/Users/el-bahdja.boudoua/Desktop/HealthMap_PADIWeb.csv")


################################
#1- Cleaning data
################################


#Sort dataframe from least recent publication to most recent
EBSdatabase$issue_date <- format(dmy(EBSdatabase$issue_date ),"%d-%m-%y")
EBSdatabase$issue_date  <- as.Date(EBSdatabase$issue_date , format="%d-%m-%y")

EBSdatabase<-EBSdatabase[order(as.Date(EBSdatabase$issue_date, format="%d/%m/%Y")),]



#creating unique Id culumn 

EBSdatabase <- cbind(ID = 1:nrow(EBSdatabase), EBSdatabase) 

#Rename id to ref_article 
#name is an r function I use this instead of dplyr bc of an update problem
names(EBSdatabase)[names(EBSdatabase) == "alert_id"] <- "Ref_article"


#we subset variables we need for danger signals

datasignals<-EBSdatabase[, c("ID","Ref_article","place_name","lat","lon","issue_date","disease_name","species_name","source","Safesignal")]


#classify species in three groups
#################################
datasignals<-CategorizeSpecies(datasignals)

#classify sources in two groups
###############################
datasignals<-CategorizeMediaSource(datasignals)

#Classify subtypes
##########################################
datasignals<-CategorizeSubtypes(datasignals)

#add new column for Dangersingals

datasignals['Dangersignal'] <- NA





####################################
#2- Set parameters for Dangersignals 
####################################

for (i in (1:nrow(datasignals))){
  if (datasignals[i,]$SourceType == 'official') {datasignals[i,]$Dangersignal=30}
  else if (datasignals[i,]$SourceType == 'unofficial') {datasignals[i,]$Dangersignal=20}}

for (i in (1:nrow(datasignals))){
  if (datasignals[i,]$SpeciesType == 'domestic birds') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 30}
  else if (datasignals[i,]$SpeciesType == 'wild birds') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 20}
  if (datasignals[i,]$SpeciesType == 'unspecified bird') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 10}
  if (datasignals[i,]$SpeciesType == 'human') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 5}
  if (datasignals[i,]$SpeciesType == 'else') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 0}}

for (i in (1:nrow(datasignals))){
  if (datasignals[i,]$Subtype == 'HPAI') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 40}
  else if (datasignals[i,]$Subtype == 'LPAI') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 30}
  if (datasignals[i,]$Subtype == 'unspecified') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 10}
  if (datasignals[i,]$Subtype == 'else') {datasignals[i,]$Dangersignal=datasignals[i,]$Dangersignal + 0}}

############################################
#Get table with coordinates, dates and signals
############################################

Antigens<-datasignals[, c("ID","Ref_article","place_name","issue_date","lat","lon","Dangersignal","Safesignal")]

#in case we cant to test without the ASF cases
#Antigens<-datasignals[datasignals$SpeciesType!="else", c("ID","Ref_article","place_name","issue_date","lat","lon","Dangersignal","Safesignal")]

#in case we want to test without the safesignals
Antigens$Safesignal<-0


#Calculate Output for each Antigen
Antigens$CSM <-((Antigens$Dangersignal*2)-Antigens$Safesignal)

#Antigens$CSM <- sapply(1:nrow(Antigens), function(i) if(Antigens[i,"Safesignal"]>0) exp(mean(log(c(Antigens[i,"Dangersignal"],Antigens[i,"Safesignal"])))) else Antigens[i,"Dangersignal"])

#We take a small sample to check if it works
#don't run this line if you want to work on the whole antigens dataset

#test on 30 first antigens

#Antigens<-Antigens[(1:30),]

#We convert lat and lon to numbers
Antigens$lat<-as.numeric(Antigens$lat)
Antigens$lon<-as.numeric(Antigens$lon)

#We add column to calculate the distance and the distance coefficient
Antigens$DistCellAnt<-NA
Antigens$Delta_dist<-NA

#We add column to classify cells according to their maturity it is set to 0
Antigens$maturite<-0

#We add column to calculate nb of expositions

Antigens$NbExp<-1

#chaque ag est logiqument exposé à sa cellule ? puisqu'il la crée

Antigens$ExposedCells = Antigens$ID



#The first Ag generate the first cell
#Cells<-Antigens[1,-14]
Cells<-Antigens[1, -which(names(Antigens) == "ExposedCells")]

#cette fonction t'affiche le chemin qui mene au script
getwd()

