options(warn=-1)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(reshape)
library(reshape2)
library(viridis)
library(gridExtra)
library(hrbrthemes)
library(car)
library(lubridate)
library(RColorBrewer)
library(data.table)
library(ggrepel)
"%notin%"=Negate("%in%")

dataset = read_excel('C:\\Users\\Abdullah\\Desktop\\Hussain projects\\Heart Disease\\Statistical-Analysis-of-Heart-Disease-Dataset-master\\heart.xlsx')
dataset[,c(2,3,6,7,9,11:14)]=sapply(dataset[,c(2,3,6,7,9,11:14)],as.factor)
# Checking average age of the subjects in dataset:

options(repr.plot.width=15, repr.plot.height=7)

g1=ggplot(dataset,aes(x=age,fill=sex))+geom_histogram(binwidth=2)+
  labs(x='Age',y='Count')+scale_fill_discrete(name='Sex',labels=c('Female','Male'))

g2=ggplot(dataset,aes(x=age,fill=sex))+geom_density(alpha=0.3)+
  labs(x='Age',y='Density')+scale_fill_discrete(name='Sex',labels=c('Female','Male'))

grid.arrange(g1,g2,ncol=2)


# Let's see the distribution of age w.r.t heart disease among different sexes:
options(repr.plot.width=10, repr.plot.height=6)

ggplot(dataset, aes (x=target,y=age, fill=sex))+ 
         geom_boxplot( outlier.shape = 23, alpha=0.5)+
         labs(x='Target', y='Age')+
         scale_color_discrete(name='Sex', labels=c('Female','Male'))

glimpse(dataset)




# Feature Engineering, created a categorical age feature that gives ranges of different ages.
dataset=dataset %>% mutate(age_cat=ifelse(age<=15,'0-15',ifelse(age<=35,'16-35',ifelse(age<=59,'36-59','>60'))))

dataset=dataset %>% mutate(sex=ifelse(sex=='0','Female','Male'))

#Make a factor for new age category levels
dataset$age_cat=factor(dataset$age_cat,levels=c('16-35','36-59','>60'))

dataset=dataset %>% mutate(target=ifelse(target=='0','High_Risk','Low_Risk'))




#Verify applied function and creation of new column
sapply(dataset,class)

#High Risk patients w.r.t Sex
dataset %>% group_by(sex,target) %>% summarize(n=n()) %>% mutate(freq=round(100*n/sum(n),2)) %>%
  filter(target=='High_Risk') %>% select(sex,target,percent_high_risk=freq)

#Distribution of male and female subjects according to their ages.
male_prop=dataset %>% filter(sex=='Male') %>% group_by(age_cat) %>% summarize(count=n())
female_prop=dataset %>% filter(sex=='Female') %>% group_by(age_cat) %>% summarize(count=n())


options(repr.plot.width=10, repr.plot.height=5)

ggplot(male_prop, aes(x='',y = count, fill=c('16-35 yrs','35-59 yrs','>60 yrs')))+ geom_bar(stat='identity')+ coord_polar(theta ="y")+theme_void()+ 
  guides(fill = guide_legend(title = "Age Group"))

ggplot(female_prop, aes(x='',y = count, fill=c('16-35 yrs','35-59 yrs','>60 yrs')))+ geom_bar(stat='identity')+ coord_polar(theta ="y")+theme_void()+ 
  guides(fill = guide_legend(title = "Age Group"))

