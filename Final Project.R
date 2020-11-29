#Osob Abas
#final project
library(readxl)
library(tidyverse)
library(broom)
library(ggfortify)
oxy <- read_excel("Desktop/Biostats/Final Project/data.xlsx")
View(oxy)
#making qualification from categorical to numerical
oxy <- oxy%>%
  mutate(ID =  factor (ID),
         DogID = factor(DogID),
         qualification_num =  factor(qualification),
         qualification_num = as.numeric(qualification_num),
         qualification_num = (qualification_num-2)^2 )
#Rename column to genotype 
oxy <- oxy%>%
  rename(genotype = `OTR-SNP4`)
view(oxy)
#initial look at data 
#Amount of dogs with each oxytocin genotype 
oxy%>%
  group_by(genotype)%>%
  summarise(n = n())
#number of dogs that qualified
oxy%>%
  group_by(qualification)%>%      
  summarise(n = n())                          #more dogs were unqualified (237/340)
#looking at sex, qualification and genotype 
ggplot(oxy, aes(x =qualification, fill =Sex  ))+
  geom_bar(position = 'dodge')+
  labs( x = "Dog Training Qualification")
#genotype('OTR-SNP4) and qualification
ggplot(oxy, aes(x = qualification , y  = genotype, fill =genotype  ))+
  geom_bar(position = 'dodge')+
  labs( x = "Dog Training Qualification")

#looking at factor1 personality and qualification
ggplot(oxy, aes(x =qualification, y =Factor1 ))+
  geom_boxplot()+
  labs( x = "Dog Training Qualification", y = "Factor1(Training Focus)")
#Plot factor2 personality and qualification
ggplot(oxy, aes(x =qualification, y =Factor2 ))+
  geom_boxplot()+
  labs( x = "Dog Training Qualification", y = "Factor2(Tolerance)")
#plot the relationship between genotype(OTR-SNP4) and factor1 when it comes to qualification
ggplot(oxy, aes(x =qualification, y =Factor1 , fill = genotype ))+
  geom_boxplot()+
  labs( x = "Dog Training Qualification", y = "Factor1(Training Focus)")



#look at the relationship between genotype(OTR-SNP4) and factor1 when it comes to qualification
full_model <- glm(qualification_num ~  Sex + Factor1+ Factor2+ genotype , data = oxy, family = binomial)
tidy(full_model)     
  anova(test =  'Chisq')       #GTC/GTC is a reference point 
coef(full_model)
#make a PCA
myPr <- prcomp(oxy[, 8:14], center = TRUE, scale = TRUE)

#autoplot the PCA and look at qualification
autoplot(myPr, 
         data = oxy %>% 
           mutate(qualification= factor(qualification)), 
         colour = "qualification", loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE) 

#combine data pf the PCA and the orginal data(oxy)
combined_data <- bind_cols(oxy, as.tibble(myPr$x))
ggplot(combined_data,aes( x = PC1, y = Factor1))+
  geom_point()
ggplot(combined_data,aes( x = PC2, y = Factor2))+
  geom_point()

#glm on PC1 and PC2
glm(qualification_num~PC1, data = combined_data,family = binomial)
glm(qualification_num~PC2, data = combined_data,family = binomial)
glm(qualification_num~PC1*PC2, data = combined_data,family = binomial)%>% tidy()


#find the proportion variance using the PCA
tibble(sds = myPCA$sd) %>%
  mutate(var = sds^2,
         var_explained = var / sum(var))

#plot the relationship between PC1 and genotype on qualificatoin
ggplot(combined_data, aes(x = genotype, y = PC1, fill  = qualification))+
  geom_boxplot()

#Plot with PC1 and genotype effecting qualificaiton
ggplot(combined_data, aes(x = PC1, y = qualification_num, color =  genotype))+
  geom_smooth(method ="glm", method.args = "binomial")+
  labs(y = "Dog Training Qualification", x = "PC1(Training Focus)")
#looking at a running mean 
 combined_data%>%
  mutate(PC1 = cut(PC1,seq(-6,6,.25)))%>%
  group_by(PC1,genotype) %>%
  summarise(qualification_num =mean(qualification_num,na.rm=TRUE))
