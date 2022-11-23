#Codes for Markov Modelling

#install the packages
install.packages("ChannelAttribution")
install.packages("reshape")
install.packages("ggplot2")
install.packages("reshape")

#run libraries
library(ChannelAttribution)
library(reshape)
library(ggplot2)
library("RColorBrewer")
library(readxl)

# Read the data ---------------------------------------------------------------------------------------------------------------------------------

mar <- read_excel("C:/Users/hp/OneDrive/Desktop/Shraddha laptop backup/Masters ASA/DDA/FINAL/excel files (22-09 to 22-10)/data_for_rcode.xlsx")
View(mar)
mar<-as.data.frame(mar)
mar[1,]

#transition probabilities (including loops)
tp<-transition_matrix(Data=mar,var_path = 'MCF Channel Grouping Path', var_conv = 'Conversion',var_null = 'Null_Con',order=1,sep=">")
tp
# Note: Create a tpm format here--------


# Order 1: -----------------------------------------------------------------------------------------------------------------------------

#Fitting a order 1 markov model 

#Estimate a Makov model returning transition matrix and removal effects
m1<-markov_model(mar, "MCF Channel Grouping Path", "Conversion",
             var_value="Conversion Value",var_null="Null_Con",order=1,out_more=TRUE)

#get tranistion probabilities (not including self loops)
m1$transition_matrix


#get removal effects 
m1$removal_effects

#get results
m1$result

#Plotting order 1 markov model results for conversions 
p<-m1$result
p


brewer.pal(n = 8, name = "Pastel1")
ggplot(p, aes(fill=channel_name, y=total_conversions, x=channel_name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Order 1 Markov Model (Conversions)")+
  scale_fill_manual(values=c("#9ecae1", "#9ecae1" ,"#9ecae1","#9ecae1", "#9ecae1"))+theme_classic()+geom_text(aes(label = round(total_conversions,3)), position = position_dodge(0.9))

#Plotting order 1 markov model results for values 
ggplot(p, aes(fill=channel_name, y=total_conversion_value, x=channel_name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Order 1 Markov Model (Values)")+
  scale_fill_manual(values=c("#9ecae1", "#9ecae1" ,"#9ecae1","#9ecae1", "#9ecae1"))+theme_classic()+geom_text(aes(label = round(total_conversion_value,3)), position = position_dodge(0.9))

#Find the best order for the markov model ---------------------------------------------------------------------------------------------------------------------------------

#Finding best order markov model
res<-choose_order(mar,"MCF Channel Grouping Path", "Conversion", var_null="Null_Con",max_order=10, sep=">", 
             ncore=1, roc_npt=100, plot=TRUE )
plot(res$auc$order,res$auc$auc,type="l",xlab="order",ylab="pauc",main="AUC",col="blue")
lines(res$auc$order,res$auc$pauc,col="red")
legend("topleft", legend=c("auc","penalized auc"),
       col=c("blue","red"),lty=1)

#Since k=6 is desirable, repeat the process again

# User k=6 and repeat the process ---------------------------------------------------------------------------------------------------------------------------------

#Fitting a order 6 markov model 

#transition probabilities (including loops)
tp6<-transition_matrix(Data=mar,var_path = 'MCF Channel Grouping Path', var_conv = 'Conversion',var_null = 'Null_Con',order=6,sep=">")
tp6

#Estimate a Makov model returning transition matrix and removal effects
m6<-markov_model(mar, "MCF Channel Grouping Path", "Conversion",
                 var_value="Conversion Value",var_null="Null_Con",order=6,out_more=TRUE)

#get tranistion probabilities (not including self loops)
m6$transition_matrix

#get removal effects 
m6$removal_effects

#get results
m6$result


#Plotting order 6 markov model results for conversions 
p6<-m6$result

brewer.pal(n = 8, name = "Pastel1")
ggplot(p6, aes(fill=channel_name, y=total_conversions, x=channel_name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Order 6 Markov Model (Conversions)")+
  scale_fill_manual(values=c("#FBB4AE", "#B3CDE3" ,"#E5D8BD","#DECBE4", "#FED9A6"))+theme_classic()+geom_text(aes(label = round(total_conversions,3)), position = position_dodge(0.9))

#Plotting order 6 markov model results for values
ggplot(p6, aes(fill=channel_name, y=total_conversion_value, x=channel_name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Order 6 Markov Model (Values)")+
  scale_fill_manual(values=c("#FBB4AE", "#B3CDE3" ,"#E5D8BD","#DECBE4", "#FED9A6"))+theme_classic()+geom_text(aes(label = round(total_conversion_value,3)), position = position_dodge(0.9))

#Comparative Plotting --------------------------------------------------------------------------------------------------------------------------------


#Compare the removal effects of m1 and m6
rem<-data.frame(m1$removal_effects,m6$removal_effects)
rem<-rem[,-4]
View(rem)
colnames(rem)<-c("Channel_Name","Conversion O1","Value O1" ,"Conversion O6", "Value O6")

#Conversions removal_effect
rem_new<-rem[,c(1,2,4)]
rem1<-melt(rem_new,id=c("Channel_Name"))
View(rem1)


ggplot(rem1, aes(fill=variable, y=value, x=Channel_Name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Removal Effects Order 1 Vs 6 (Conversions)")+
  scale_fill_manual(values=c("#FBB4AE", "#B3CDE3" ,"#E5D8BD","#DECBE4", "#FED9A6"))+theme_classic()+geom_text(aes(label = round(value,3)), position = position_dodge(0.9))

#Values removal_effect
rem_new1<-rem[,c(1,3,5)]
rem2<-melt(rem_new1,id=c("Channel_Name"))
View(rem2)

ggplot(rem2, aes(fill=variable, y=value, x=Channel_Name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Removal Effects Order 1 Vs 6 (Values)")+
  scale_fill_manual(values=c("#DECBE4", "#FED9A6"))+theme_classic()+geom_text(aes(label = round(value,3)), position = position_dodge(0.9))


#Compare the Conversions and Values of m1 and m6
con<-data.frame(m1$result,m6$result)
View(con)
con<-con[,-4]
View(con)
colnames(con)<-c("Channel_Name","Conversion O1","Value O1" ,"Conversion O6", "Value O6")

#Conversions removal_effect
con_new<-con[,c(1,2,4)]
con1<-melt(con_new,id=c("Channel_Name"))
View(con1)


ggplot(con1, aes(fill=variable, y=value, x=Channel_Name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Comparision of Order 1 Vs 6 (Conversions)")+
  scale_fill_manual(values=c("#FBB4AE", "#B3CDE3" ,"#E5D8BD","#DECBE4", "#FED9A6"))+theme_classic()+geom_text(aes(label = round(value,3)), position = position_dodge(0.9))

#Values comparision
con_new1<-con[,c(1,3,5)]
con2<-melt(con_new1,id=c("Channel_Name"))
View(con2)

ggplot(con2, aes(fill=variable, y=value, x=Channel_Name)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("Comparision of Order 1 Vs 6 (Values)")+
  scale_fill_manual(values=c("#DECBE4", "#FED9A6"))+theme_classic()+geom_text(aes(label = round(value,3)), position = position_dodge(0.9))


#####The end ####




