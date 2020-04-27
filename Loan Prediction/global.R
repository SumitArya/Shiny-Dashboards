library(dplyr)

## read the data from the input csv
data_Jay<-read.csv('inputData.csv',stringsAsFactors = FALSE,na.strings = "n/a")
#data_Jay<-read.csv('data.csv',stringsAsFactors = FALSE)

## merge few categories in emp_length
## replace all na in the column as Others
data_Jay[is.na(data_Jay$emp_length),"emp_length"]<-"Others"

## create new caetgories as desired for emp_length data
emp_categories=c("Others"="Others","< 1 year"="Less than 1 Year", "1 year"="1 to 5 Years","2 years"="1 to 5 Years","3 years"="1 to 5 Years","4 years"="1 to 5 Years", "5 years"="5 to 10 Years","6 years"="5 to 10 Years","7 years"="5 to 10 Years","8 years"="5 to 10 Years","9 years"="5 to 10 Years","10 years"="5 to 10 Years", "10+ years"="More than 10 Years")
## inicialize the data
data_Jay$empCategory=rep("1",length.out=nrow(data_Jay))
## start the count
i<-1
for(x in data_Jay[,"emp_length"]){
  data_Jay[i,"empCategory"]=emp_categories[[x]]
  i<-i+1
}

## keep tham in desired order
data_Jay$empCategory <- factor(data_Jay$empCategory, levels = c("Less than 1 Year","1 to 5 Years", "5 to 10 Years", "More than 10 Years","Others"))

## divide the dti data into few categories
data_Jay$dtiCategory<-rep("1",length.out=nrow(data_Jay))

data_Jay$dtiCategory[data_Jay$dti < 20] = "0 to 20"
data_Jay$dtiCategory[data_Jay$dti >= 20 & data_Jay$dti < 40] = "20 to 40"
data_Jay$dtiCategory[data_Jay$dti >= 40 & data_Jay$dti < 50] = "40 to 50"
data_Jay$dtiCategory[data_Jay$dti >= 50 & data_Jay$dti < 60] = "50 to 60"
data_Jay$dtiCategory[data_Jay$dti >= 60 & data_Jay$dti < 100] = "60 to 100"

## keep tham in desired order
data_Jay$dtiCategory <- factor(data_Jay$dtiCategory, levels = c("0 to 20","20 to 40", "40 to 50", "50 to 60","60 to 100"))


## divide the revol_util data into few categories
data_Jay$revol_util<-as.numeric(gsub("%","",data_Jay$revol_util))  ## remove % from the values
data_Jay$rvlCategory<-rep("1",length.out=nrow(data_Jay))

data_Jay$rvlCategory[data_Jay$revol_util < 20] = "0 to 20"
data_Jay$rvlCategory[data_Jay$revol_util >= 20 & data_Jay$revol_util < 40] = "20 to 40"
data_Jay$rvlCategory[data_Jay$revol_util >= 40 & data_Jay$revol_util < 60] = "40 to 60"
data_Jay$rvlCategory[data_Jay$revol_util >= 60 & data_Jay$revol_util < 80] = "60 to 80"
data_Jay$rvlCategory[data_Jay$revol_util >= 80 & data_Jay$revol_util < 100] = "80 to 100"

## arrange in desired order
data_Jay$rvlCategory <- factor(data_Jay$rvlCategory, levels = c("0 to 20","20 to 40", "40 to 60", "60 to 80","80 to 100"))


