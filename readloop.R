list.dirs(path=getwd()) # to list subfolders/subdirs and use this info to go into the subfolders within a loop
# e.g. Sub-folders
#sub.folders <- list.dirs(parent.folder, recursive=TRUE)[-1]

filenames <- list.files(path=getwd()) # to list all files in a folder, ggf. define type of the file

for (i in filenames) {  
  name <- gsub("-",".",i) # gsub(pattern, replacement, x)
  name <- gsub(".csv","",name)  
  i <- paste(".\\",i,sep="")
  assign(name,read.csv(i, header=FALSE)) #read in the table and name as "name"
  }