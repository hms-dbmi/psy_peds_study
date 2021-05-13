rm(list=ls())
library(readxl)

mentalCodes <- read_excel("pediatric psychiatric ICD-10 codes.xlsx")
mentalCodes$codeFormated <- paste0( "ICD10:", substr(mentalCodes$`ICD-10 Code`, 1,3), 
                                    ".", substr(mentalCodes$`ICD-10 Code`, 4,nchar(mentalCodes$`ICD-10 Code`))) 

groups <- unique(mentalCodes$`Mental Health Disorder Group`)

for( i in 1:length(groups)){
  print( groups[i])
  groupName <- substr( gsub(" ", "", groups[i]), 1, 10)
  icd10Codes <- unique(mentalCodes[ mentalCodes$`Mental Health Disorder Group` == groups[i], "codeFormated"])
  icd10Codes <- paste(icd10Codes$codeFormated, collapse = "','")
  query <- paste0( "create table ", groupName," as select obs.patient_num, obs.concept_cd, obs.start_date, pat.birth_date, extract(year from obs.start_date) - extract( year from pat.birth_date) as age from observation_fact obs, patient_dimension pat where obs.patient_num = pat.patient_num and extract( year from obs.start_date ) > 2019 and ( extract(year from obs.start_date) - extract( year from pat.birth_date)  >  6 and extract(year from obs.start_date) - extract( year from pat.birth_date) < 18 ) and concept_cd in ('", icd10Codes, "');" )
  print(query)
}

###
rm(list=ls())

files <- list.files("./")

for( i in 1:length(files)){
  if( i == 1){
    output <- read.csv(files[i])
    output$category <- sapply(strsplit( files[i], "[.]"), '[', 1)
  }else{
    int <- read.csv(files[i])
    int$category <- sapply(strsplit( files[i], "[.]"), '[', 1)
    output <- rbind( output, int )
  }
}

### time series filtering by 21st April 2020
## day of the month %d
## %y
## abbr. month %b or %h

output$date <- sapply(strsplit( output$START_DATE, " "), '[', 1)
output$date <- as.Date( output$date, "%d-%B-%y")
output <- output[ output$date > "2020-04-21",]
output$weeks <- cut(output[,"date"], breaks="week")
subset <- unique(output[ , c("PATIENT_NUM", "weeks")])
length(unique(subset$PATIENT_NUM))
summary( subset$weeks )

library(dplyr)
byWeek <- subset %>% group_by(weeks) %>% tally()
byWeek$year <-sapply(strsplit( as.character(byWeek$weeks), "-"), '[', 1) 
byWeek$period <- "post"
ggplot(data= byWeek, aes(x= as.Date(weeks), y= n, fill= period )) +
  geom_bar(stat="identity")+
  ylab("counts") + xlab("Week")+ theme_bw()+
  geom_smooth(method = "lm", se= TRUE, aes(colour=period), colour= "blue", size = 0.5)+ 
  scale_fill_manual(values=c('#00BFC4', '#F8766D'))

#### by group category
subsetByCategory <- unique(output[ , c("PATIENT_NUM", "weeks", "category")])

byWeekCategory <- subsetByCategory %>% group_by(weeks, category) %>% tally()
byWeekCategory$period <- "post"
ggplot(data= byWeekCategory, aes(x= as.Date(weeks), y= n, fill= period, group=category )) +
  geom_bar(stat="identity")+
  ylab("counts") + xlab("Week")+ theme_bw()+
  geom_smooth(method = "lm", se= TRUE, aes(colour=period), colour= "blue", size = 0.5)+ 
  scale_fill_manual(values=c('#00BFC4', '#F8766D'))+
  facet_grid(. ~ category)

selection <- byWeekCategory[ byWeekCategory$category %in% 
                               c("anxiety", "depressive","suicideSelfInfury", 
                                 "obsessive", "substance", "feedingEating"), ]

selection$category <- gsub("anxiety", "Anxiety Disorders", selection$category)
selection$category <- gsub("depressive", "Depressive Disorders", selection$category)
selection$category <- gsub("suicideSelfInfury", "Suicide or Self-Injury", selection$category)
selection$category <- gsub("obsessive", "Obsessive-Compulsive and Related Disorders", selection$category)
selection$category <- gsub("substance", "Substance-Related and Addictive Disorders", selection$category)
selection$category <- gsub("feedingEating", "Feeding and Eating Disorders", selection$category)

selection%>% ggplot() +
  geom_line(aes(x = as.Date(weeks), y = n,  group=category), color = "steelblue", size=0.5)+
  scale_x_date(name = "", 
               breaks = seq.Date(as.Date("2020-04-20"), 
                                 as.Date("2021-04-12"), by = "1 week"), 
               date_labels = "%b-%U")+ 
  scale_y_continuous(name = "number patients")+ 
  facet_wrap(facets = "category", scales = "free",
             labeller = label_wrap_gen(width=40)) +
  labs(x = "calendar weeks", y = "number patients consulting for condition")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 7), text = element_text(size=5), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### bars
selection %>% ggplot(aes(x = as.Date(weeks), y = n)) +
  geom_bar( stat= 'identity', fill= 'steelblue')+
  scale_x_date(name = "", 
               breaks = seq.Date(as.Date("2020-04-20"), 
                                 as.Date("2021-04-12"), by = "1 week"), 
               date_labels = "%b-%U")+ 
  scale_y_continuous(name = "number patients")+ 
  facet_wrap(facets = "category", scales = "free",
             labeller = label_wrap_gen(width=40)) +
  labs(x = "calendar weeks", y = "number patients consulting for condition")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 7), text = element_text(size=5), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



####
ukyData <- read_excel("UKY_PEDPSYCH_PRINCLDX_GRPWKNUM/PEDPSYCHE8_PRINCPL_ALLGRP.xlsx")

ukyAll <- ukyData %>% group_by(YR, WEEKNUMBEROFYEAR) %>% summarise(n= sum( DAILY_PATIENTS))
ukyAll$WEEKNUMBEROFYEAR <- ifelse( nchar( ukyAll$WEEKNUMBEROFYEAR) == 1, paste0("0", ukyAll$WEEKNUMBEROFYEAR), 
                          ukyAll$WEEKNUMBEROFYEAR)
#ukyAll$date <- paste(ukyAll$YR, ukyAll$WEEKNUMBEROFYEAR, sep="-")

ukyAll$date <- paste(ukyAll$YR, ukyAll$WEEKNUMBEROFYEAR, 1, sep="-")
ukyAll$date <- gsub("2017-2019", "2019", ukyAll$date)
ukyAll$date2 <- as.Date(ukyAll$date, "%Y-%W-%u")
ukyAll$period <- ifelse(  as.Date( ukyAll$date2) < "2020-03-01", "pre", "post")

ggplot(data= ukyAll, aes(x= as.Date( ukyAll$date2) , y= n, fill= period )) +
  geom_bar(stat="identity")+
  ylab("counts") + xlab("Week")+ theme_bw()+
  geom_smooth(method = "lm", se= TRUE, aes(colour=period), colour= "blue", size = 0.5)+ 
  scale_fill_manual(values=c('#00BFC4', '#F8766D'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data= ukyAll[ as.Date( ukyAll$date2) < "2021-03-01", ], aes(x= date2, y= n, fill= period )) +
  geom_bar(stat="identity")+
  ylab("counts") + xlab("Week")+ theme_bw()+
  geom_smooth(method = "lm", se= TRUE, aes(colour=period),colour= "blue", size = 0.5)+ 
  scale_fill_manual(values=c('#00BFC4', '#F8766D'))

