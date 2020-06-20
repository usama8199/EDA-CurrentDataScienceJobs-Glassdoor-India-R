setwd("C:\\Users\\Lenovo\\Desktop\\Refactored_Py_DS_ML_Bootcamp-master\\Python-board-infinity\\R project")
library('dplyr')
df<-read.csv("jobs_dataset_1.csv",stringsAsFactors=FALSE)
head(df)


#there are no null values
df[!complete.cases(df),]


# removed outliers of job title

df[grep('Data Scien|Data scien|DATA SCIEN|data scientist|DAta SCIENCE|Data Modeler|
        ',df$Job.Title),'Job.Title']<-'Data Scientist'
df[grep('data engineer',tolower(df$Job.Title)),'Job.Title']<-'Data Engineer'
df[grep('data analy',tolower(df$Job.Title)),'Job.Title']<-'Data Analyst'
df[grep('applied scientist|research scientist|research',tolower(df$Job.Title)),'Job.Title']<-'Data Scientist'
df[grep('Data Scien',df$Job.Title),'Job.Title']<-'Data Scientist'
df[grep('machine learning|ai scien',tolower(df$Job.Title)),'Job.Title']<-'Machine Learning Engineer'
df[grep('analytic',tolower(df$Job.Title)),'Job.Title']<-'Data Scientist'
distinct(select(df,Job.Title))

df[df$Job.Title=='Quantitative Analyst ? iRageCapital'|df$Job.Title=='Sr. Business Intelligence Analyst'|df$Job.Title=='Scientist- Fermentation'|
   df$Job.Title=='Senior Scientist I'| df$Job.Title=='Data Test Engineer'| df$Job.Title=='AI/NLP Scientist (P.hd / MS must)'|
   df$Job.Title=='Breeding Design Implementation Scientist'|df$Job.Title=='Customer Scientist'|df$Job.Title=='Decision Scientist - Malayalam Speaker'|
     df$Job.Title=='SERM Scientist',]<-NA
df<- na.omit(df)


#remove salaries estimates as in india glassdoor dosen't has salary estimates
df$Salary.Estimate<-NULL


#Extract important info from description

#df$Job.Description<-gsub("\n",' ',df$Job.Description)
#as.logical(grep('python',tolower('ahjahsjah')))

#df[df$Job.Description==logical(0),'Job.Title']
j=1
for( i in df[,'Job.Description']){
  if(grepl("python",tolower(i),fixed=TRUE))
  {df[j,'python']=1}else
  {df[j,'python']=0}
  j=j+1
} #extract python word from description
colnames(df)[which(names(df) == "python")] <- "Python"

j=1
for( i in df[,'Job.Description']){
  if(grepl("r programming",tolower(i),fixed=TRUE))
  {df[j,'R Prog']=1}else
  {df[j,'R Prog']=0}
  j=j+1
} #extract R prog word from description

j=1
for( i in df[,'Job.Description']){
  if(grepl("excel",tolower(i),fixed=TRUE))
  {df[j,'Excel']=1}else
  {df[j,'Excel']=0}
  j=j+1
}  #extract excel word from description

j=1
for( i in df[,'Job.Description']){
  if(grepl("hadoop",tolower(i),fixed=TRUE))
  {df[j,'Hadoop']=1}else
  {df[j,'Hadoop']=0}
  j=j+1
}  #extract hadoop word from description

j=1
for( i in df[,'Job.Description']){
  if(grepl("sql",tolower(i),fixed=TRUE))
  {df[j,'SQL']=1}else
  {df[j,'SQL']=0}
  j=j+1
}  #extract sql word from description

j=1
for( i in df[,'Job.Description']){
  if(grepl("sas",tolower(i),fixed=TRUE))
  {df[j,'SAS']=1}else
  {df[j,'SAS']=0}
  j=j+1
}  #extract SAS word from description


#remove ratings from column name
df$Company.Name=sapply(strsplit(as.character(df$Company.Name), "\n"), function(x) x[[1]])

#check for validity of ratings
#-1 means not present
min(df$Rating) #-1
max(df$Rating) #5

#check for location
distinct(select(df,Location))
#Bengaluru
df$Location<-gsub("Bengaluru, India",'Bengaluru',df$Location)
df$Location<-gsub("Andheri",'Mumbai',df$Location)
df$Location<-gsub("India, India",'India',df$Location)
df$Location<-gsub("Hiranandani Gardens",'Mumbai',df$Location)
df[df$Location=='Khar'|df$Location=='SAS Nagar',]<-NA
df<- na.omit(df)
distinct(select(df,Location))
df[df$Location=='Karnataka' & df$Headquarters=='Gurgaon, India','Location']<-'Gurgaon'
df[df$Location=='Andhra Pradesh'|df$Location=='Maharashtra'|df$Location=='Karnataka','Location']<- '-1'
df$Location<-gsub("Vikhroli",'Mumbai',df$Location)
distinct(select(df,Location))

#check type of ownership
distinct(select(df,Type.of.ownership))
df[df$Type.of.ownership=='Unknown'|df$Type.of.ownership=='Other Organisation'|df$Type.of.ownership=='College / University'|
   df$Type.of.ownership=='Private Practice / Firm'|
   df$Type.of.ownership=='Subsidiary or Business Segment'|df$Type.of.ownership=='Self-employed'|
   df$Type.of.ownership=='Self-employed','Type.of.ownership']<-'-1'

#change revenue unknown to -1
distinct(select(df,Revenue))
df[df$Revenue=='Unknown / Non-Applicable','Revenue']<-'-1'

#change size of unknown to -1
df[df$Size=='Unknown','Size']<-'-1'
head(df)

write.csv(df,file = 'cleaned_job_dataset',row.names = F)
