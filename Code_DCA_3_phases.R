
##################################################################
#Here we set Disease.time_limit and Disease.space_limit parameters
##################################################################


#Cell stops sampling and migrate if date between two antigens exceed 30 days
Disease.time_limit=22

#We choose a max distance parameter, Cells samples antigens in this radius
#Distance max is 1000km (maybe too much but it works better with this dataset)
Disease.space_limit=20000


#############################
#Procedure_1: Detection Phase
#############################


for (i in c(2:nrow(Antigens))){
  for (j in c(1:nrow(Cells))){
    
    #distm is a function which calculates distance and returns the result in meters
    #here we calculate the distance between i line in Table complete and all the lines in Cells table
    #fun = distGeo = distance on an ellipsoid, Highly accurate estimate of the shortest distance between two points on an ellipsoid
    
    Cells[j,]$DistCellAnt<-(distm(c(Antigens[i,"lon"], Antigens[i,"lat"]), c(Cells[j,"lon"], Cells[j,"lat"]), fun = distGeo))
    
    print(paste(i, "-", j, Cells[j,]$DistCellAnt)) #Cells[j,]$coef_dist))
    
    #We suppose the distance max = 1000km
    if  (abs(julian(as.Date(Antigens[i, "issue_date"]), as.Date(Cells[j,"issue_date"])))<Disease.time_limit && abs((Cells[j,]$DistCellAnt<Disease.space_limit))) {
      
      #if distance < 1000 km AND the diff of publication dates is <30 days 
      #we calculate the coef distance and we update the outputs and nb of expositions     
      Cells[j,]$Delta_dist= ((Disease.space_limit-Cells[j,]$DistCellAnt)/Disease.space_limit)
      
      
      
      
      print(paste("****",i, "-", j, Cells[j,]$DistCellAnt, Cells[j,]$Delta_dist))
      #La je vois que le calcul se fait comme il faut !!
      
      
      Cells[j, "CSM"] = ((Antigens[i, "CSM"])*Cells[j,]$Delta_dist) + Cells[j, "CSM"]
      
      #We can use the coef_distance to weight the nb of exposures too
      Cells[j, "NbExp"] = ((Antigens[i, "NbExp"])*Cells[j,]$Delta_dist) + Cells[j, "NbExp"]
      
      #Update which Ags were sampled by Cells      
      Antigens[i,"ExposedCells"]= paste(Antigens[i,"ExposedCells"],",",as.character(Cells[j,"ID"])) 
      
    }
    
######################################
#Procedure_2: Context Assessment Phase
######################################
    
    #The Threshold is (the mean of output signals/ the mean of Nb of exposition)    
    #We calculate threshold to classify cells (mature or not)
    meanCSM<-mean(Cells$CSM)
    meanNbexp<-mean(Cells$NbExp)
    
    #Threshold = mean of output signals/ mean of exposition
    RatioExp<-meanCSM/meanNbexp
    #Threshold<-80
    if ((Cells[j,]$CSM/Cells[j,]$NbExp)>RatioExp){
      Cells[j,]$maturite=1
    }
    
    #print(paste(i, "-", j))
    #to check if the loop is working on both tables
  }
  #At the end the ag generates a NewCell
  
  Cells[i,]<-Antigens[i, -which(names(Antigens) == "ExposedCells")]
  #Copier ligne sans dernière colonne 

  
  
}

##################################
#Procedure_3: Classification Phase
##################################


nb_cells = nrow(Cells)
rownames(Cells) = paste0("Cell",as.character(Cells$ID)) # cell ids are used as index to access a specific cell in an instant way

#Coef is the anomaly coefficient, for each Ag:
#Coef = nb of exposition in mature cells/total number of expositions
#The column Coeff is added to the Antigens table and set to 0

Antigens$Coeff = 0

for (i in c(1:nrow(Antigens))){
  exposed.cells.str = Antigens[i,"ExposedCells"] # string, e.g. "1,3"
  exposed.cell.id.list = as.integer(unlist(strsplit(exposed.cells.str, ","))) # list of ids, e.g. [1,3]
  exposed.cell.id.list.as.index = paste0("Cell", as.character(exposed.cell.id.list))  # list of ids are converted as in the rownames of the matrix Cells for index purposes, e.g. ["Cell1","Cell3"]
  
  maturity.status.list = Cells[exposed.cell.id.list.as.index,"maturite"]
  Coeff = length(which(maturity.status.list == 1))/length(maturity.status.list)
  Antigens[i,"Coeff"] = Coeff
}



# If we want to export Antigens dataframe
# write_xlsx(Dataframe name,"C:\\PATH\\Name.xlsx")

#write_xlsx(Antigens,"C:\\Users\\el-bahdja.boudoua\\Desktop\\Antigens210.xlsx")
#write_csv2(Antigens,"C:\\Users\\el-bahdja.boudoua\\Desktop\\Antigens1010.csv")

#Add column for the DCA Classification

Antigens$DCAclassification<-NA


#classification = 0 means that Ag is irrelevant, 1 means relevant
#For the first exp we set the coeff threshold to 0.5

for (i in (1:nrow(Antigens))){
  if (Antigens[i,]$Coeff>0.5) {Antigens[i,]$DCAclassification=1}
  else {Antigens[i,]$DCAclassification=0}}

#Take a look at the Antigens table,column DCA classification
#1 means relevant 0 means irrelevant


##############################
#Here we evaluate the method
#############################

Refmanual<-EBSdatabase[, c("Ref_article","Manualclass","ID")]

df<-inner_join(Antigens,Refmanual, by="ID")

df<-df[,c("ID","DCAclassification","Manualclass")]


for (i in (1:nrow(df))){
  if (df[i,]$Manualclass == 'Relevant') {df[i,]$Manualclass=1}
  else {df[i,]$Manualclass = 0}}

#Here we specify that DCAclassification is our prediction
#the manual annotation is our reference 

pred<- factor(df$DCAclassification)
ref<-factor(df$Manualclass)
tab <- table(pred,ref)




#Create the confusion matrix
#We create a confusion Matrics for the relevant class (positive = "1")
confusionMatrix(tab)

confusionMatrix(tab, positive = "1",mode = "prec_recall")



#We create a confusionmatrix for the irrelevant class (positive = "0")
confusionMatrix(tab, positive = "0",mode = "prec_recall")



rm(Cells)
###################################################################
#Avant d'utiliser Weka il faut convertir les fichier en format arff
###################################################################
#Get library

library(RWeka)
######################
# import data
######################

NewBd2 <- read.csv2("C:/Users/el-bahdja.boudoua/Desktop/HealthMap_PADIWeb_AIOnly.csv")


write.arff(Healthmapdata1, file = "Healthmapdata1.arff")
#Ou alors exporter comme ceci 
write.arff(NewBd2,"C:\\Users\\el-bahdja.boudoua\\Desktop\\NewBdForWeka2.arff")

#Cette fonction est très utile, elle te montre le chemin vers tes fichiers
getwd()
####
df<-Antigens[Antigens$Coeff == '0',]
df<-inner_join(df,EBSdatabase, by="ID")
