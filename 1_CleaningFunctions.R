###########################################
#Fonctions pour le prétraitement des données
############################################

CategorizeMediaSource<-function(datasignals){
  #Liste des sources off
  off_source_names = c('oie','fao','empresi')
  
  off_source_index <- which(datasignals$source %in% off_source_names)
  
  #Which is not official
  unoff_source_index <- which(!(datasignals$source %in% off_source_names))
  
  datasignals['SourceType'] <- NA
  datasignals$SourceType[off_source_index] <- 'official'
  datasignals$SourceType[unoff_source_index] <- 'unofficial'
  return(datasignals)
}


#pour les espèces

CategorizeSpecies<-function(datasignals){
  
  dom_B_index <- which(datasignals$species_name%in%c('Ducks (domestic)','Backyard bird','Chicken','Poultry (domestic)','quail','Turkey','Poultry','chicken'))
  
  wild_B_index<- which(datasignals$species_name%in%c('wild birds', 'Crows','crow','falcon', 'fowl','goose','swan','ostrich', 'crane','Penguins','waterbird','Peacocks','Seals'))
  
  unsp_index <- which(datasignals$species_name%in%c('Birds','bird'))
  
  human_index<-which(datasignals$species_name%in%c('Humans'))
  
  else_index<-which(datasignals$species_name%in%c('Pigs','Boar','Meat'))
  
  datasignals['SpeciesType'] <- NA
  
  datasignals$SpeciesType[dom_B_index] <- 'domestic birds'
  datasignals$SpeciesType[wild_B_index] <- 'wild birds'
  datasignals$SpeciesType[unsp_index] <- 'unspecified bird'
  datasignals$SpeciesType[human_index] <- 'human'
  datasignals$SpeciesType[else_index]<-'else'
  return(datasignals)
  
}

#pour les sous_types viraux

CategorizeSubtypes<-function(datasignals){
  
  unspecified_index <- which(datasignals$disease_name%in%c('Avian Influenza'))
  
  HPAI_index<- which(datasignals$disease_name%in%c('Avian Influenza H5N1','Avian Influenza H5N2','Avian Influenza H5N6','Avian Influenza H5N8','H5N1','H5N6','Avian Influenza H5','Avian Influenza H1N1'))
  
  LPAI_index <- which(datasignals$disease_name%in%c('LPAI','Avian Influenza H7N9'))
  
  else_index <- which(datasignals$disease_name%in%c('African Swine Fever'))

  
  datasignals['Subtype'] <- NA
  
  datasignals$Subtype[unspecified_index] <- 'unspecified'
  datasignals$Subtype[HPAI_index] <- 'HPAI'
  datasignals$Subtype[LPAI_index] <- 'LPAI'
  datasignals$Subtype[else_index] <- 'else'
  return(datasignals)
  
}

#cette commande te permet de voir le chemin qui mène à ton fichier
getwd()




