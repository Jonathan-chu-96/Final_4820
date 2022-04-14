library(tidyverse)
library(naniar)

gg_miss_var(mental_health)
colSums(is.na(mental_health1))

mental_health1<-subset(mental_health, select = -c(state,Timestamp,comments))
mental_health1$self_employed<-mental_health1$self_employed%>%replace_na("No")

library(Hmisc)
mental_health1$work_interfere<-with(mental_health1,impute(work_interfere,"random"))

mental_health1[(mental_health1$Age<18)|(mental_health1$Age>99), "Age"] <- median(mental_health1$Age)

other  = c('A little about you', 'p', 'Nah', 'Enby', 'Trans-female','something kinda male?','queer/she/they','non-binary','All','fluid', 'Genderqueer','Androgyne', 'Agender','Guy (-ish) ^_^', 'male leaning androgynous','Trans woman','Neuter', 'Female (trans)','queer','ostensibly male, unsure what that really means','trans')
male   = c('male', 'Male','M', 'm', 'Male-ish', 'maile','Cis Male','Mal', 'Male (CIS)','Make','Male ', 'Man', 'msle','cis male', 'Cis Man','Malr','Mail')
female = c('Female', 'female','Cis Female', 'F','f','Femake', 'woman','Female ','cis-female/femme','Female (cis)','femail','Woman','female')

mental_health1[(mental_health1$Gender%in%other),"Gender"]<-"other"
mental_health1[(mental_health1$Gender%in%male),"Gender"]<-"male"
mental_health1[(mental_health1$Gender%in%female),"Gender"]<-"female"
unique(mental_health1$Gender)

mental_clean<-mental_health1
write.csv(mental_clean,"mental_health.csv")

library(ggplot2)
mental_health1%>%ggplot(aes(y=))