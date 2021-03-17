library(dplyr)
library(pheatmap)
library(corrplot)
library(RColorBrewer)#required for wordcloud
library(wordcloud) #for word visulaization
library(arsenal)
library(ggplot2)

#Import data
#Do the Right Thing Millennial
rightThing <- read.csv("D:/Documents/NEU/Integrated Experenetial Project/Week 3/assignment/rightThing.csv")

#The tech Geek Milennial
techGeek <- read.csv("D:/Documents/NEU/Integrated Experenetial Project/Week 3/assignment/techGeek.csv")

#The Millennial Mom
millennialMom <- read.csv("D:/Documents/NEU/Integrated Experenetial Project/Week 3/assignment/millennialMom.csv")

#The environmental Millennial
environmental <- read.csv("D:/Documents/NEU/Integrated Experenetial Project/Week 3/assignment/environmental.csv")

#The Dont call me a Millennial
dontCallme <- read.csv("D:/Documents/NEU/Integrated Experenetial Project/Week 3/assignment/dontCallme.csv")

#The Millennial by name only
nameOnly <- read.csv("D:/Documents/NEU/Integrated Experenetial Project/Week 3/assignment/nameOnly.csv")

#hecking Data dimensions
dim(rightThing)
dim(techGeek)
dim(millennialMom)
dim(environmental)
dim(dontCallme)
dim(nameOnly)


#Check missing value
sapply(rightThing, function(x) sum(is.na(x)))
sapply(techGeek, function(x) sum(is.na(x)))
sapply(millennialMom, function(x) sum(is.na(x)))
sapply(environmental, function(x) sum(is.na(x)))
sapply(dontCallme, function(x) sum(is.na(x)))
sapply(nameOnly, function(x) sum(is.na(x)))

#Summary of the of Data set
summary(rightThing)
summary(techGeek)
summary(millennialMom)
summary(environmental)
summary(dontCallme)
summary(nameOnly)

#Determining the structure of dataset
str(rightThing)
str(techGeek)
str(millennialMom)
str(environmental)
str(dontCallme)
str(nameOnly)


#EDA Correlation Matrix
pheatmap(cor(Filter(is.numeric, rightThing)),main="Correlation Matrix- Do the Right Thing Millennial")
pheatmap(cor(Filter(is.numeric, techGeek)),main="Correlation Matrix- Tech Geek Millennial")
pheatmap(cor(Filter(is.numeric, millennialMom)),main="Correlation Matrix-Milennial Mom")
pheatmap(cor(Filter(is.numeric, environmental)),main="Correlation Matrix - Environmental Millenial")
pheatmap(cor(Filter(is.numeric, dontCallme)),main="Correlation Matrix - Dont Call Me a millennial")
pheatmap(cor(Filter(is.numeric, nameOnly)),main="Correlation Matrix - Millennial by name only")



#grouping key words related to food, personal-care items , entertainment and clothing under consumer-goods
consumer_goods = c("book","Restaurant", "Food","Chef","Dish","Eat","Coffee","Drink",
                   "Eat","Cup","Brew","Starbucks","Hotel","Menu")
consumer_goods_DF = data_frame(consumer_goods)

#grouping key words related to health and fitness
Health_Fitness = c("Exercise", "Weight","Fitness","Body","Disease","Diet")
health_fitness_DF = data_frame(Health_Fitness)

#grouping keywords related to family and life
Family_life = c("Family","Life","Retire","Worker","Job","Child",
                "School","Car","Retirement",'Home')
family_life_DF = data_frame(Family_life)

#grouping keywords related to finace and income
Finances = c("Tax","Income","Financial","Loan","Mortgage","Debt","Money",
"Pay","Credit Card","Credit","Pension","Affordable")
finances_DF =data_frame(Finances)

#grouping keywords related to government and politics
political = c("Trump","Republican","Democrat","Democratic","Party","President","Candidate",
              "American","Biden","Election","Voter","Senator","Senate", "Campaign","Warren"
              ,"Sanders","Political","Gop","Presidential","Representative"
              ,"House","Debate")
political_DF = data_frame(political)

#grouping keywords related to race and religion
race = c("Race","White","Black")
race_DF = data_frame(race)

#grouping keywords related to technology

technology = c("Game","Camera", "Playstation","Iphone","Apple","Sony","Console","Xbox",
                "Nintendo","Phone","Version","Technology","Microsoft","Computer","Sony",
                "Controller","Keyboard","Edition","Mouse","Wireless","Device","Laptop","Gamer"
                ,"Headset")
technology_DF = data_frame(technology)



#creating function to determine similarity rate between different personas
dataframe1 = data_frame()
dataframe2 = data_frame()

zero = 0


similarityscore = function (dataframe1, dataframe2)
{
 common_keyword = dataframe1 %in% dataframe2
filter_true = subset(common_keyword,common_keyword==TRUE)
if(length(filter_true)==0)
{
  result= zero
}
else{ score = table(filter_true)
score_DF = as.data.frame(score)
result = score_DF$Freq
}
return(result)
 } 
 


#comparing each Persona with categories created
#comparison of Do the right thing millennial

#Consumer Goods
rightThing_Keywords = data_frame(colnames(rightThing))

SS_righthing_CG = similarityscore(rightThing_Keywords$`colnames(rightThing)`,consumer_goods_DF$consumer_goods)
SS_righthing_CG
class(SS_righthing_CG)

#Health & Fitness
righthing_HF = similarityscore(rightThing_Keywords$`colnames(rightThing)`,health_fitness_DF$Health_Fitness)
righthing_HF

#Family and Lifestyle
righthing_FL = similarityscore(rightThing_Keywords$`colnames(rightThing)`,family_life_DF$Family_life)
righthing_FL

#Finances
righthing_Fin = similarityscore(rightThing_Keywords$`colnames(rightThing)`,finances_DF$Finances)
righthing_Fin

#Politics
rightThing_Pol = similarityscore(rightThing_Keywords$`colnames(rightThing)`,political_DF$political)
rightThing_Pol

#technology
righthing_Tech = similarityscore(rightThing_Keywords$`colnames(rightThing)`,technology_DF$technology)
righthing_Tech
 
#comparing tech Geek Millennials with diffferent categories
techGeek_Keywords = data_frame(colnames(techGeek))

techGeek_CG = similarityscore(techGeek_Keywords$`colnames(techGeek)`,consumer_goods_DF$consumer_goods)
techGeek_HF = similarityscore(techGeek_Keywords$`colnames(techGeek)`,health_fitness_DF$Health_Fitness)
techGeek_FL = similarityscore(techGeek_Keywords$`colnames(techGeek)`,family_life_DF$Family_life)
techGeek_Fin = similarityscore(techGeek_Keywords$`colnames(techGeek)`,finances_DF$Finances)
techGeek_Pol = similarityscore(techGeek_Keywords$`colnames(techGeek)`,political_DF$political)
techGeek_Tech = similarityscore(techGeek_Keywords$`colnames(techGeek)`,technology_DF$technology)

#comparing millennialMom with diffferent categories
millennialMom_Keywords = data_frame(colnames(millennialMom))

millennialMom_CG = similarityscore(millennialMom_Keywords$`colnames(millennialMom)`,consumer_goods_DF$consumer_goods)
millennialMom_HF = similarityscore(millennialMom_Keywords$`colnames(millennialMom)`,health_fitness_DF$Health_Fitness)
millennialMom_FL = similarityscore(millennialMom_Keywords$`colnames(millennialMom)`,family_life_DF$Family_life)
millennialMom_Fin = similarityscore(millennialMom_Keywords$`colnames(millennialMom)`,finances_DF$Finances)
millennialMom_Pol = similarityscore(millennialMom_Keywords$`colnames(millennialMom)`,political_DF$political)
millennialMom_Tech = similarityscore(millennialMom_Keywords$`colnames(millennialMom)`,technology_DF$technology)

#comparing environmental millennials with different categories
environmental_Keywords = data_frame(colnames(environmental))

environmental_CG = similarityscore(environmental_Keywords$`colnames(environmental)`,consumer_goods_DF$consumer_goods)
environmental_HF = similarityscore(environmental_Keywords$`colnames(environmental)`,health_fitness_DF$Health_Fitness)
environmental_FL = similarityscore(environmental_Keywords$`colnames(environmental)`,family_life_DF$Family_life)
environmental_Fin = similarityscore(environmental_Keywords$`colnames(environmental)`,finances_DF$Finances)
environmental_Pol = similarityscore(environmental_Keywords$`colnames(environmental)`,political_DF$political)
environmental_Tech = similarityscore(environmental_Keywords$`colnames(environmental)`,technology_DF$technology)

#comparing keywords from dont call me a millennial with different categories
dontCallme_Keywords = data_frame(colnames(dontCallme))

dontCallme_CG = similarityscore(dontCallme_Keywords$`colnames(dontCallme)`,consumer_goods_DF$consumer_goods)
dontCallme_HF = similarityscore(dontCallme_Keywords$`colnames(dontCallme)`,health_fitness_DF$Health_Fitness)
dontCallme_FL = similarityscore(dontCallme_Keywords$`colnames(dontCallme)`,family_life_DF$Family_life)
dontCallme_Fin = similarityscore(dontCallme_Keywords$`colnames(dontCallme)`,finances_DF$Finances)
dontCallme_Pol = similarityscore(dontCallme_Keywords$`colnames(dontCallme)`,political_DF$political)
dontCallme_Tech = similarityscore(dontCallme_Keywords$`colnames(dontCallme)`,technology_DF$technology)

#comapring keywords from millennials by name only with different categories
nameOnly_Keywords = data_frame(colnames(nameOnly))

nameOnly_CG = similarityscore(nameOnly_Keywords$`colnames(nameOnly)`,consumer_goods_DF$consumer_goods)
nameOnly_HF = similarityscore(nameOnly_Keywords$`colnames(nameOnly)`,health_fitness_DF$Health_Fitness)
nameOnly_FL = similarityscore(nameOnly_Keywords$`colnames(nameOnly)`,family_life_DF$Family_life)
nameOnly_Fin = similarityscore(nameOnly_Keywords$`colnames(nameOnly)`,finances_DF$Finances)
nameOnly_Pol = similarityscore(nameOnly_Keywords$`colnames(nameOnly)`,political_DF$political)
nameOnly_Tech = similarityscore(nameOnly_Keywords$`colnames(nameOnly)`,technology_DF$technology)


#creating vectors for each category
CG_compare = c(SS_righthing_CG,techGeek_CG,millennialMom_CG,environmental_CG,dontCallme_CG,nameOnly_CG)
HF_compare = c(righthing_HF,techGeek_HF,millennialMom_HF,environmental_HF,dontCallme_HF,nameOnly_HF)
FL_compare = c(righthing_FL,techGeek_FL,millennialMom_FL,environmental_FL,dontCallme_FL,nameOnly_FL)
Fin_compare = c(righthing_Fin,techGeek_Fin,millennialMom_Fin,environmental_Fin,dontCallme_Fin,nameOnly_Fin)
Pol_compare = c(rightThing_Pol,techGeek_Pol,millennialMom_Pol,environmental_Pol,dontCallme_Pol,nameOnly_Pol)
Tech_compare = c(righthing_Tech,techGeek_Tech,millennialMom_Tech,environmental_Tech,dontCallme_Tech,nameOnly_Tech)
 

#creating vector for each persona
rightthing_vector = c(SS_righthing_CG,righthing_HF,righthing_FL,righthing_Fin,rightThing_Pol,righthing_Tech)
techGeek_vector= c(techGeek_CG,techGeek_HF,techGeek_FL,techGeek_Fin,techGeek_Pol,techGeek_Tech)
millennialMom_vector= c(millennialMom_CG,millennialMom_HF,millennialMom_FL,millennialMom_Fin,millennialMom_Pol,millennialMom_Tech)
environemental_vector=c(environmental_CG,environmental_HF,environmental_FL,environmental_Fin,environmental_Pol,environmental_Tech)
dontCallme_vector=c(dontCallme_CG,dontCallme_HF,dontCallme_FL,dontCallme_Fin,dontCallme_Pol,dontCallme_Tech)
nameOnly_vector=c(nameOnly_CG,nameOnly_HF,nameOnly_FL,nameOnly_Fin,nameOnly_Pol,nameOnly_Tech)

categories = c("Consumer Goods",
               "Health & Fitness",
               "Family & Lifestyle",
               "Finance",
               "Political",
               "technology")

millennial_name = c("Do the Right Thing Millennial",
                      "Tech Geek Millennial",
                      "Millennial Mom",
                      "Environemntal Milennial",
                      "Dont Call me a Millennial",
                      "Millennial By name only") 

millennial_DF = data.frame(millennial_name,CG_compare,HF_compare,FL_compare,Fin_compare,Pol_compare,Tech_compare)

category_DF = data.frame(categories,rightthing_vector,techGeek_vector,millennialMom_vector,environemental_vector,dontCallme_vector,nameOnly_vector)
#plotting the results

ggplot(millennial_DF, aes(x = HF_compare, y = millennial_name,fill=millennial_name)) + 
  geom_col() +scale_fill_hue()+
  ggtitle("Health&Fitness across personas") + xlab("Similarity Score")

ggplot(millennial_DF, aes(x = FL_compare, y = millennial_name,fill=millennial_name)) + 
  geom_col() +scale_fill_hue()+
  ggtitle("Family&Lifestyle across personas") + xlab("Similarity Score")

ggplot(millennial_DF, aes(x = Fin_compare, y = millennial_name,fill=millennial_name)) + 
  geom_col() +scale_fill_hue()+
  ggtitle("Finance& monetary across personas") + xlab("Similarity Score")

ggplot(millennial_DF, aes(x = Pol_compare, y = millennial_name,fill=millennial_name)) + 
  geom_col() +scale_fill_hue()+
  ggtitle("Political across personas") + xlab("Similarity Score")

ggplot(millennial_DF, aes(x = Tech_compare, y = millennial_name,fill=millennial_name)) + 
  geom_col() +scale_fill_hue()+
  ggtitle("Technology across personas") + xlab("Similarity Score")

ggplot(millennial_DF, aes(x =CG_compare , y = millennial_name,fill=millennial_name)) + 
  geom_col() +scale_fill_hue()+
  ggtitle("Consumer Goods across personas") + xlab("Similarity Score")

#plots for categories
ggplot(category_DF, aes(x = rightthing_vector, y = categories,fill=categories)) + 
  geom_col() +scale_fill_hue(l=50,c=80)+
  ggtitle("Categories across Do the right Thing Millennial") + xlab("Categories")

ggplot(category_DF, aes(x = techGeek_vector, y = categories,fill=categories)) + 
  geom_col() +scale_fill_hue(l=50,c=80)+
  ggtitle("Categories across Tech Geek Millennial") + xlab("Categories")

ggplot(category_DF, aes(x = millennialMom_vector, y = categories,fill=categories)) + 
  geom_col() +scale_fill_hue(l=50,c=80)+
  ggtitle("Categories across Millennial Mom") + xlab("Categories")

ggplot(category_DF, aes(x = environemental_vector, y = categories,fill=categories)) + 
  geom_col() +scale_fill_hue(l=50,c=80)+
  ggtitle("Categories across Environmental Millennial") + xlab("Categories")

ggplot(category_DF, aes(x = dontCallme_vector, y = categories,fill=categories)) + 
  geom_col() +scale_fill_hue(l=50,c=80)+
  ggtitle("Categories Dont Call me a Millennial") + xlab("Categories")

ggplot(category_DF, aes(x = nameOnly_vector, y = categories,fill=categories)) + 
  geom_col() +scale_fill_hue(l=50,c=80)+
  ggtitle("Categories across Millennial by name only") + xlab("Categories")

compare_result = rightThing_Keywords$`colnames(rightThing)`%in% consumer_goods_DF$consumer_goods


count_compare_result = subset(compare_result, compare_result==TRUE)
a=table(count_compare_result)
b=as.data.frame(a)
b$Freq




  


# Common keywords across personas: 7 keywords
common <- intersect(colnames(rightThing), colnames(techGeek))
common
common <- intersect(common, colnames(millennialMom))
common
common <- intersect(common, colnames(environmental))
common
common <- intersect(common, colnames(dontCallme))
common
common <- intersect(common, colnames(nameOnly))
common

# Create a data frame with common keywords
p1 =rightThing %>% select(one_of(common)) %>% mutate(Persona = 1)
p2 =techGeek %>% select(one_of(common)) %>% mutate(Persona = 2)
p3 =millennialMom %>% select(one_of(common)) %>% mutate(Persona = 3)
p4 =environmental %>% select(one_of(common)) %>% mutate(Persona = 4)
p5 =dontCallme %>% select(one_of(common)) %>% mutate(Persona = 5)
p6 = nameOnly %>% select(one_of(common)) %>% mutate(Persona = 6)

f <- full_join(p1, p2)
f <- full_join(f, p3)
f <- full_join(f, p4)
f <- full_join(f, p5)
f <- full_join(f, p6)
f

# Convert new column to factor
f$Persona <- factor(f$Persona)

# 7 common keywords: House, Show, Trump, Democrat, Party, Republican, Woman
# Trend of House across personas
dev.off()
f %>% select(Date, Persona, House) %>%
  ggplot(aes(x=Date, y=House)) +
  geom_line(aes(color = Persona), size = 1) +
  ylab("Daily Observation % (House)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank())


# Trend of Show across personas
f %>% select(Date, Persona, Show) %>%
  ggplot(aes(x=Date, y=Show)) +
  geom_line(aes(color = Persona), size = 1) +
  ylab("Daily Observation % (Show)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank())


# Trend of Trump across personas
f %>% select(Date, Persona, Trump) %>%
  ggplot(aes(x=Date, y=Trump)) +
  geom_line(aes(color = Persona), size = 1) +
  ylab("Daily Observation % (Trump)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank())


# Trend of Democrat across personas
f %>% select(Date, Persona, Democrat) %>%
  ggplot(aes(x=Date, y=Democrat)) +
  geom_line(aes(color = Persona), size = 1) +
  ylab("Daily Observation % (Democrat)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank())


# Trend of Party across personas
f %>% select(Date, Persona, Party) %>%
  ggplot(aes(x=Date, y=Party)) +
  geom_line(aes(color = Persona), size = 1) +
  ylab("Daily Observation % (Party)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank())


# Trend of Republican across personas
f %>% select(Date, Persona, Republican) %>%
  ggplot(aes(x=Date, y=Republican)) +
  geom_line(aes(color = Persona), size = 1) +
  ylab("Daily Observation % (Republican)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank())


# Trend of Woman across personas
f %>% select(Date, Persona, Woman) %>%
  ggplot(aes(x=Date, y=Woman)) +
  geom_line(aes(color = Persona), size = 1) +
  ylab("Daily Observation % (Woman)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank())