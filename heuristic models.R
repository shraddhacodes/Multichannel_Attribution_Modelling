#Code for heuristic models

#install the packages
install.packages("ChannelAttribution")
install.packages("reshape")
install.packages("ggplot2")

#run libraries
library(ChannelAttribution)
library(reshape)
library(ggplot2)
library("RColorBrewer")
library(tidyverse)
library("readxl")

#Load and see the data
data_for_rcode <- read_excel("C:/Users/hp/OneDrive/Desktop/Shraddha laptop backup/Masters ASA/DDA/FINAL/excel files (22-09 to 22-10)/data_for_rcode.xlsx")
Data <- as.data.frame(data_for_rcode)
View(Data)
head(Data)
summary(Data)

#Get column names
Data[1,]

#regular models> first touch , last touch, linear
H <- heuristic_models(Data, 'MCF Channel Grouping Path', 'Conversion','Conversion Value')
View(H)

#retrieve the conversion columns
H1<-H[,c(1,2,4,6)]
View(H1)

#Plotting for conversions
channels<-rep(c("Direct","Organic Search","Referral","Paid Search","Social Network"),3)
touch<-c(rep("first_touch_conversions",5),rep("last_touch_conversions",5),rep("linear_touch_conversions",5))
value1<-H1[['first_touch_conversions']]
value2<-H1[['last_touch_conversions']]
value3<-H1[['linear_touch_conversions']]
values<-c(value1,value2,value3)
values
diag<-data.frame(channels,touch,values)
View(diag)


rbind(diag)



#choosing colors and plotting

brewer.pal(n = 8, name = "Pastel1")
ggplot(diag, aes(fill=touch, y=values, x=channels)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Heuristic Models (Conversion)")+
  scale_fill_manual(values=c("#FBB4AE", "#B3CDE3" ,"#E5D8BD"))+theme_classic()+geom_text(aes(label = round(values,3)), position = position_dodge(0.9))

#retrieve the values columns
H2<-H[,c(1,3,5,7)]
View(H2)


#Plotting for values
channels<-rep(c("Direct","Organic Search","Referral","Paid Search","Social Network"),3)
touch<-c(rep("first_touch_value",5),rep("last_touch_value",5),rep("linear_touch_value",5))
value1<-H2[['first_touch_value']]
value2<-H2[['last_touch_value']]
value3<-H2[['linear_touch_value']]
values<-c(value1,value2,value3)
values
diag1<-data.frame(channels,touch,values)
View(diag1)

#choosing colors and plotting

brewer.pal(n = 8, name = "Pastel1")
ggplot(diag1, aes(fill=touch, y=values, x=channels)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Heuristic Models (Values)")+
  scale_fill_manual(values=c("#FBB4AE", "#B3CDE3" ,"#E5D8BD"))+theme_classic()+geom_text(aes(label = round(values,3)), position = position_dodge(0.9))

m1<-markov_model(mar, "MCF Channel Grouping Path", "Conversion",
                 var_value="Conversion Value",var_null="Null_Con",order=1,out_more=TRUE)
res<-m1$result
res<-res[,c(1,2)]
res<-as.data.frame(res)
View(res)
res1<-data.frame(res$channel_name,c(rep("markov_conversions",5)),res$total_conversions)
View(res1)
colnames(res1)<-c('channels','touch','values')
all<-rbind(diag,res1)
View(all)
ggplot(all, aes(fill=touch, y=values, x=channels)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("All models")+
  scale_fill_manual(values=c('#e5f5e0','#a8ddb5' ,'#31a354',"#9ecae1"))+theme_classic()+geom_text(aes(label = round(values,3)), position = position_dodge(0.9))
