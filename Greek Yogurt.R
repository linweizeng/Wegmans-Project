setwd("D:/Simon Business School/Marketing Research/Greek Yogurt")

library(foreign)
filnm = "Wegmans Survey 1";
spssDataLab <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=TRUE,
                         trim_values=TRUE)
spssData <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE)
summary(spssData)


#test the goodness of fit
summary(spssDataLab$Q33)
PopSex<-c(0.13,0.87)
SampleSex<-c(length(spssDataLab$Q33[spssDataLab$Q33=='Male']),
             length(spssDataLab$Q33[spssDataLab$Q33=='Female']))
chisq.test(as.table(SampleSex),p=PopSex)
w = PopSex/prop.table(as.table(SampleSex))
wSamp = w[as.numeric(spssData$Question33Areyou)]


#Average importance rating
library(data.table)
Attribute<-c('Question6Allnatural','Question6Blended','Question6Calorielevel','Question6Consistency',
             'Question6Fatlevel','Question6Fruitonthebottom','Question6Organic','Question6Price',
             'Question6Proteinlevel','Question6rbSTfree','Question6Sidebysidecup','Question6Taste',
             'Question6Texture')

#Purchase==yes
spssData1<-spssData[spssData$Question1HaveyoupurchasedGreekYogurtinthepastmonth == 'Yes',]
#Remove unsure
DFAttribute<-spssData1[,Attribute]
for(col in names(DFAttribute)) set(DFAttribute, i=which(DFAttribute[[col]]==5), j=col, value=NA)
AttributeName<-c('Allnatural','Blended','Calorielevel','Consistency','Fatlevel','Fruitonthebottom',
                 'Organic','Price','Proteinlevel','rbSTfree','Sidebysidecup','Taste','Texture')
AttributeMeans<-colMeans(DFAttribute[,Attribute],na.rm = TRUE)
AttributeSE<-apply(DFAttribute[,Attribute],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(DFAttribute[,Attribute])))

AttributeDF = data.frame(Attribute,AttributeName,AttributeMeans,AttributeSE)


library(ggplot2)
dodge<-position_dodge(width = 0.5)
gp<-ggplot(AttributeDF,aes(x= reorder(AttributeName,-AttributeMeans), y = AttributeMeans, 
                           ymax = AttributeMeans + AttributeSE, ymin = AttributeMeans - AttributeSE))
gp+geom_bar(position = dodge, stat = 'identity', col = 'darkolivegreen3', 
            fill = 'darkolivegreen3', width = 0.75) +
  geom_errorbar(position = dodge, width = 1) + labs(x = 'Attributes',y = 'Ratings')


#yogurt sales
Yogurt<-c('Question7DannonGreek','Question7Chobani','Question7Fage','Question7StonyfieldOikos',
          'Question7TraderJoes','Question7ThreeGreekGods','Question7YoplaitGreek')
YogurtName<-c('DannonGreek','Chobani','Fage','StonyfieldOikos',
              'TraderJoes','ThreeGreekGods','YoplaitGreek')
YogurtSales<-colSums(spssData[,Yogurt],na.rm = TRUE)
YogurtDF<-data.frame(Yogurt,YogurtName,YogurtSales)


gp1<-ggplot(YogurtDF,aes(x = reorder(YogurtName,-YogurtSales), y = YogurtSales))
gp1+
  geom_bar(position = dodge, stat = 'identity', 
           col = ifelse(YogurtName %in% c("Chobani",'DannonGreek'),'violetred3','darkolivegreen3'), 
           fill = ifelse(YogurtName %in% c("Chobani",'DannonGreek'), 'violetred3','darkolivegreen3'), width = 0.75) +
  labs(x = 'Yogurt Name',y = 'Popularity')

#Fage vs. Oiko(Q24)
#Keep purchaser
FO<-spssData[spssData$Question21HaveyoupurchasedFageGreekYogurtinthepastmonth == 'Yes'& 
               spssData$Question28HaveyoupurchasedStonyfieldOikosGreekYogurtinthepastmon == 'Yes',]

#Fage
FageAttr<-c('Question24Allnatural','Question24Calorielevel','Question24Consistency',
            'Question24Fatlevel','Question24Price','Question24Proteinlevel','Question24Sidebysidecup',
            'Question24Taste','Question24Texture')
Fage<-FO[,FageAttr]
#remove unsure
for(col in names(Fage)) set(Fage, i=which(Fage[[col]]==6), j=col, value=NA)
FageName<-c('Allnatural','Calorielevel','Consistency','Fatlevel','Price','Proteinlevel',
            'Sidebysidecup','Taste','Texture')
FageMeans<-colMeans(Fage[,FageAttr],na.rm = TRUE)
FageSE<-apply(Fage[,FageAttr],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(Fage[,FageAttr])))

FageDF = data.frame(FageAttr,FageName,FageMeans,FageSE)
gp2<-ggplot(FageDF,aes(x= reorder(FageName,-FageMeans), y = FageMeans, 
                           ymax = FageMeans + FageSE, ymin = FageMeans - FageSE))
gp2+geom_bar(position = dodge, stat = 'identity', col = 'darkolivegreen3', 
            fill = 'darkolivegreen3', width = 0.75)+ geom_errorbar(position = dodge, width = 1) +
  labs(x = 'Attributes',y = 'Ratings')


#Oiko
OikoAttr<-c('Question30Allnatural','Question30Calorielevel','Question30Consistency',
            'Question30Fatlevel','Question30Fruitonthebottom','Question30Organic','Question30Price',
            'Question30Proteinlevel','Question30Taste','Question30Texture')
Oiko<-FO[,OikoAttr]

#remove unsure
for(col in names(Oiko)) set(Oiko, i=which(Oiko[[col]]==6), j=col, value=NA)

OikoName<-c('Allnatural','Calorielevel','Consistency','Fatlevel','Fruitonthebottom','Organic',
            'Price','Proteinlevel','Taste','Texture')
OikoMeans<-colMeans(Oiko[,OikoAttr],na.rm = TRUE)
OikoSE<-apply(Oiko[,OikoAttr],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(Oiko[,OikoAttr])))

OikoDF = data.frame(OikoAttr,OikoName,OikoMeans,OikoSE)
gp2<-ggplot(OikoDF,aes(x= reorder(OikoName,-OikoMeans), y = OikoMeans, 
                       ymax = OikoMeans + OikoSE, ymin = OikoMeans - OikoSE))
gp2+geom_bar(position = dodge, stat = 'identity', col = 'darkolivegreen3', 
             fill = 'darkolivegreen3', width = 0.75) + geom_errorbar(position = dodge, width = 1) +
  labs(x = 'Attributes',y = 'Ratings')



#T-test(Paired)
t.test(Fage[,'Question24Allnatural'],Oiko[,'Question30Allnatural'],paired = TRUE)
t.test(Fage[,'Question24Price'],Oiko[,'Question30Price'],paired = TRUE)
t.test(Fage[,'Question24Taste'],Oiko[,'Question30Taste'],paired = TRUE)


#Fage vs. Oiko(Q24)
#Keep purchaser
Fage0<-spssData[spssData$Question21HaveyoupurchasedFageGreekYogurtinthepastmonth == 'Yes',]
Oiko0<-spssData[spssData$Question28HaveyoupurchasedStonyfieldOikosGreekYogurtinthepastmon == 'Yes',]

#Fage
FageAttr<-c('Question24Allnatural','Question24Calorielevel','Question24Consistency',
            'Question24Fatlevel','Question24Price','Question24Proteinlevel','Question24Sidebysidecup',
            'Question24Taste','Question24Texture')
Fage1<-Fage0[,FageAttr]
#remove unsure
for(col in names(Fage1)) set(Fage1, i=which(Fage1[[col]]==6), j=col, value=NA)
FageName<-c('Allnatural','Calorielevel','Consistency','Fatlevel','Price','Proteinlevel',
            'Sidebysidecup','Taste','Texture')
FageMeans<-colMeans(Fage1[,FageAttr],na.rm = TRUE)
FageSE<-apply(Fage1[,FageAttr],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(Fage1[,FageAttr])))

FageDF = data.frame(FageAttr,FageName,FageMeans,FageSE)
gp2<-ggplot(FageDF,aes(x= reorder(FageName,-FageMeans), y = FageMeans, 
                       ymax = FageMeans + FageSE, ymin = FageMeans - FageSE))
gp2+geom_bar(position = dodge, stat = 'identity', col = 'darkolivegreen3', 
             fill = 'darkolivegreen3', width = 0.75) + geom_errorbar(position = dodge, width = 1) + 
  labs(x = 'Attributes',y = 'Ratings')


#Oiko
OikoAttr<-c('Question30Allnatural','Question30Calorielevel','Question30Consistency',
            'Question30Fatlevel','Question30Fruitonthebottom','Question30Organic','Question30Price',
            'Question30Proteinlevel','Question30Taste','Question30Texture')
Oiko1<-Oiko0[,OikoAttr]

#remove unsure
for(col in names(Oiko1)) set(Oiko1, i=which(Oiko1[[col]]==6), j=col, value=NA)

OikoName<-c('Allnatural','Calorielevel','Consistency','Fatlevel','Fruitonthebottom','Organic',
            'Price','Proteinlevel','Taste','Texture')
OikoMeans<-colMeans(Oiko1[,OikoAttr],na.rm = TRUE)
OikoSE<-apply(Oiko1[,OikoAttr],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(Oiko1[,OikoAttr])))

OikoDF = data.frame(OikoAttr,OikoName,OikoMeans,OikoSE)
gp2<-ggplot(OikoDF,aes(x= reorder(OikoName,-OikoMeans), y = OikoMeans, 
                       ymax = OikoMeans + OikoSE, ymin = OikoMeans - OikoSE))
gp2+geom_bar(position = dodge, stat = 'identity', col = 'darkolivegreen3', 
             fill = 'darkolivegreen3', width = 0.75) + geom_errorbar(position = dodge, width = 1) +
  labs(x = 'Attributes',y = 'Ratings')



#T-test(independent)
t.test(Fage1[,'Question24Allnatural'],Oiko1[,'Question30Allnatural'])
t.test(Fage1[,'Question24Price'],Oiko1[,'Question30Price'])
t.test(Fage1[,'Question24Taste'],Oiko1[,'Question30Taste'])


#Cooking vs not cooking
Cooking<-spssData1[spssData1$Question12DoyouuseGreekYogurtforcooking=='Yes',Attribute]
Snacks<-spssData1[spssData1$Question12DoyouuseGreekYogurtforcooking!='Yes',Attribute]

#Cooking
CookingMeans<-colMeans(Cooking[,Attribute],na.rm = TRUE)
CookingSE<-apply(Cooking[,Attribute],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(Cooking[,Attribute])))
CookingDF = data.frame(Attribute,AttributeName,CookingMeans,CookingSE)

gp<-ggplot(CookingDF,aes(x= reorder(AttributeName,-CookingMeans), y = CookingMeans, 
                           ymax = CookingMeans + CookingSE, ymin = CookingMeans - CookingSE))
gp+geom_bar(position = dodge, stat = 'identity', col = 'darkolivegreen3', 
            fill = 'darkolivegreen3', width = 0.75) +
  geom_errorbar(position = dodge, width = 1) + labs(x = 'Attributes',y = 'Ratings')

#not cooking
SnacksMeans<-colMeans(Snacks[,Attribute],na.rm = TRUE)
SnacksSE<-apply(Snacks[,Attribute],2,sd,na.rm=TRUE)/
  sqrt(colSums(!is.na(Snacks[,Attribute])))
SnacksDF = data.frame(Attribute,AttributeName,SnacksMeans,SnacksSE)

gp<-ggplot(SnacksDF,aes(x= reorder(AttributeName,-SnacksMeans), y = SnacksMeans, 
                         ymax = SnacksMeans + SnacksSE, ymin = SnacksMeans - SnacksSE))
gp+geom_bar(position = dodge, stat = 'identity', col = 'darkolivegreen3', 
            fill = 'darkolivegreen3', width = 0.75) +
  geom_errorbar(position = dodge, width = 1) + labs(x = 'Attributes',y = 'Ratings')

#independent t-test
t.test(Cooking[,'Question6Allnatural'],Snacks[,'Question6Allnatural'])
t.test(Cooking[,'Question6Organic'],Snacks[,'Question6Organic'])
t.test(Cooking[,'Question6rbSTfree'],Snacks[,'Question6rbSTfree'])
t.test(Cooking[,'Question6Price'],Snacks[,'Question6Price'])

#brand-cooking vs.non-cooking
Cooking<-spssData1[spssData1$Question12DoyouuseGreekYogurtforcooking=='Yes',Yogurt]
Snacks<-spssData1[spssData1$Question12DoyouuseGreekYogurtforcooking!='Yes',Yogurt]
YogurtSales<-colSums(Cooking[,Yogurt],na.rm = TRUE)
YogurtDF<-data.frame(Yogurt,YogurtName,YogurtSales)
gp1<-ggplot(YogurtDF,aes(x = reorder(YogurtName,-YogurtSales), y = YogurtSales))
gp1+
  geom_bar(position = dodge, stat = 'identity', 
           col = 'darkolivegreen3', 
           fill = 'darkolivegreen3', width = 0.75) +
  labs(x = 'Yogurt Name',y = 'Popularity')

YogurtSales<-colSums(Snacks[,Yogurt],na.rm = TRUE)
YogurtDF<-data.frame(Yogurt,YogurtName,YogurtSales)
gp1<-ggplot(YogurtDF,aes(x = reorder(YogurtName,-YogurtSales), y = YogurtSales))
gp1+
  geom_bar(position = dodge, stat = 'identity', 
           col = 'darkolivegreen3', 
           fill ='darkolivegreen3', width = 0.75) +
  labs(x = 'Yogurt Name',y = 'Popularity')
