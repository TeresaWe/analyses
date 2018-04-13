###################################### AEFT + results
#make same Code-Vectors in each results table
#e.g. within function to create table: colnames(AEFTresults)<-substr(file_list_AEFT, start=1, stop=10)

# match different experiment results tables together
index_online<-match(x=Cognition05.03$code_online, table=results$code)
#find NA by hand
#index[c(2,6,9,10,12,13,20,23)]<-c(198,230,9,206,180,214,241,199) # fill in additional cases!

#take rows of index vector of results table in a new table
resultsindex<-rbind(results[index_online,])

#cbind tables together
allresults<-cbind(Cognition05.03,resultsindex) 
#CAVE: factor levels included!
#e.g.
#allresults[,32]<-as.numeric(as.character(allresults[,32]))

###################################### NAVON


index2<-match(x=allresults$VP_Code, table=rownames(Navonresults))
#find NA by hand


#take rows of index vector of results table in a new table
resultsindex2<-rbind(Navonresults[index2,])

#cbind tables together
allresults<-cbind(allresults,resultsindex2) 
#CAVE: factor levels included!
#e.g.
#allresults[,32]<-as.numeric(as.character(allresults[,32]))

###################################### AGLT
index3<-match(x=allresults$VP_Code, table=rownames(AGLTresults))
#find NA by hand


#take rows of index vector of results table in a new table
resultsindex3<-rbind(AGLTresults[index3,])

#cbind tables together
allresults<-cbind(allresults,resultsindex3) 
#CAVE: factor levels included!
#e.g.
#allresults[,32]<-as.numeric(as.character(allresults[,32]))

####################################### COGNITION
index4<-match(x=allresults$VP_Code, table=rownames(AEFTresults))
#find NA by hand


#take rows of index vector of results table in a new table
resultsindex4<-rbind(AEFTresults[index4,])

#cbind tables together
allresults<-cbind(allresults,resultsindex4) 
#CAVE: factor levels included!
#e.g.
#allresults[,32]<-as.numeric(as.character(allresults[,32]))

######################################### AP_measures



index5<-match(x=allresults$VP_Code, table=rownames(AP_measures))
#find NA by hand


#take rows of index vector of results table in a new table
resultsindex5<-rbind(AP_measures[index5,])

#cbind tables together
allresults<-cbind(allresults,resultsindex5)
rownames(allresults)<-allresults$VP_Code
#CAVE: factor levels included!
#e.g.
#allresults[,32]<-as.numeric(as.character(allresults[,32]))

##### select online raw data of participants
ind<-match(x=allresults$code_online, table=raw180301$dupl1_Code)
online_data_institute<-raw180301[ind,]
online_data_institute<-cbind(allresults$VP_Code,allresults$code_online,online_data_institute)
rownames(online_data_institute)<-allresults$VP_Code

####further manipulations
ind_allresults<-match(x="RI15SON966", table=rownames(allresults))
allresults[ind_allresults,c("APdef","AP_sums")]<-c(1,NA)
allresults[ind_allresults,c("comments")]<-c("no answers in online PIT, maybe did not work on device")
##match with musical hours
allresults<-cbind(allresults,hours)
##### export tables
#setwd("~/files")
write.csv(allresults,"allresults28.03.csv")
write.csv(online_data_institute,"online_data_institute_13.03.csv")
