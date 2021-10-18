#assignment started on 14th october 2021
####loading different libraries
library(readxl)
library(here)
library(dplyr)

####Reading the different sheets on the excel file as different datasets
#PLEASE NOTE: In excel, moved the sheets into ascending order, where 1= 2012, 2=2014, 3=2016, 4=2018
rawdata_oh_12<- read_xlsx(here("data","nhanes_ohx_12_18.xlsx"),sheet= 1)
rawdata_oh_14<- read_xlsx(here("data","nhanes_ohx_12_18.xlsx"),sheet= 2)
rawdata_oh_16<- read_xlsx(here("data","nhanes_ohx_12_18.xlsx"),sheet= 3)
rawdata_oh_18<- read_xlsx(here("data","nhanes_ohx_12_18.xlsx"),sheet= 4)
#PLEASE NOTE: In excel, moved the sheets into ascending order, where 1= 2012, 2=2014, 3=2016, 4=2018
rawdata_demo_12<- read_xlsx(here("data","nhanes_demo_12_18.xlsx"),sheet= 1)
rawdata_demo_14<- read_xlsx(here("data","nhanes_demo_12_18.xlsx"),sheet= 2)
rawdata_demo_16<- read_xlsx(here("data","nhanes_demo_12_18.xlsx"),sheet= 3)
rawdata_demo_18<- read_xlsx(here("data","nhanes_demo_12_18.xlsx"),sheet= 4)


#### sanity check for oral health data
#checking if they are coded the same way
rawdata_oh_12 %>% select(ends_with("CTC")) %>% sapply(FUN=unique)
rawdata_oh_14 %>% select(ends_with("CTC")) %>% sapply(FUN=unique)
rawdata_oh_16 %>% select(ends_with("CTC")) %>% sapply(FUN=unique)
rawdata_oh_18 %>% select(ends_with("CTC")) %>% sapply(FUN=unique)
#they appear to be coded the same way
rawdata_oh_12 %>% select("OHDEXSTS") %>% sapply(FUN=unique)
rawdata_oh_14 %>% select("OHDEXSTS") %>% sapply(FUN=unique)
rawdata_oh_16 %>% select("OHDEXSTS") %>% sapply(FUN=unique)
rawdata_oh_18 %>% select("OHDEXSTS") %>% sapply(FUN=unique)
#they appear to be coded the same way

rawdata_oh_12 %>% select("SEQN") %>% sapply(FUN=unique)
rawdata_oh_14 %>% select("SEQN") %>% sapply(FUN=unique)
rawdata_oh_16 %>% select("SEQN") %>% sapply(FUN=unique)
rawdata_oh_18 %>% select("SEQN") %>% sapply(FUN=unique)
#they appear to be coded the same

####sanity check for dempographic data
rawdata_demo_12 %>% select ("RIDAGEYR") %>% sapply(FUN=unique)
rawdata_demo_14 %>% select ("RIDAGEYR") %>% sapply(FUN=unique)
rawdata_demo_16 %>% select ("RIDAGEYR") %>% sapply(FUN=unique)
rawdata_demo_18 %>% select ("RIDAGEYR") %>% sapply(FUN=unique)
#they appear to be coded the same way
rawdata_demo_12 %>% select ("SEQN") %>% sapply(FUN=unique)
rawdata_demo_14 %>% select ("SEQN") %>% sapply(FUN=unique)
rawdata_demo_16 %>% select ("SEQN") %>% sapply(FUN=unique)
rawdata_demo_18 %>% select ("SEQN") %>% sapply(FUN=unique)
#they appear to be coded the same way

####Checking how many complete oral examinations in 2012 data
rawdata_oh_12 %>%
  filter(OHDEXSTS %in% 1) %>% 
  tally(OHDEXSTS %in%1)
#there are 8073

#extracting the requested columns - completed an oral examination(OHDEXSTS -1) and 
#variables related to SEQN and
# Variables that end in CTC
extdata_oh_12<- rawdata_oh_12 %>%
  select("SEQN","OHDEXSTS", ends_with("CTC"))%>%
  filter(OHDEXSTS %in% 1)


####Checking how many complete oral examinations in 2014 data
rawdata_oh_14%>%
  filter(OHDEXSTS %in% 1) %>% 
  tally(OHDEXSTS %in%1)
#there are 8633

#extracting the requested columns - completed an oral examination(OHDEXSTS -1) and 
#variables related to SEQN and
# Variables that end in CTC
extdata_oh_14<- rawdata_oh_14 %>%
  select("SEQN","OHDEXSTS", ends_with("CTC"))%>%
  filter(OHDEXSTS %in% 1)

##Checking how many complete oral examinations in 2016 data
rawdata_oh_16 %>%
  filter(OHDEXSTS %in% 1) %>% 
  tally(OHDEXSTS %in%1)
#there are 8857

#extracting the requested columns - completed an oral examination(OHDEXSTS -1 and 
#variables related to SEQN and
# Variables that end in CTC
extdata_oh_16<- rawdata_oh_16 %>%
  select("SEQN","OHDEXSTS", ends_with("CTC"))%>%
  filter(OHDEXSTS %in% 1)


##Checking how many complete oral examinations in 2018 data
rawdata_oh_18 %>%
  filter(OHDEXSTS %in% 1) %>% 
  tally(OHDEXSTS %in%1)
#there are 8099
#extracting the requested columns - completed an oral examination(OHDEXSTS -1) and 
#variables related to SEQN and
# Variables that end in CTC
extdata_oh_18<- rawdata_oh_18 %>%
  select("SEQN","OHDEXSTS", ends_with("CTC"))%>%
  filter(OHDEXSTS %in% 1)

#merge the rows for oral health exams through the years 2012,14,16,18
oh_exam<- bind_rows(extdata_oh_12,extdata_oh_14,extdata_oh_16,extdata_oh_18)



#### To the demographic datasets, adding a new variable - year
rawdata_demo_12_year<-rawdata_demo_12 %>% mutate(year=2012)
rawdata_demo_14_year<-rawdata_demo_14 %>% mutate(year=2014)
rawdata_demo_16_year<-rawdata_demo_16 %>% mutate(year=2016)
rawdata_demo_18_year<-rawdata_demo_18 %>% mutate(year=2018)

#extracting the requested demographic variables related to SEQN , year, and age for 2012

extdata_demo_12<- rawdata_demo_12_year %>% 
  select("SEQN","year","RIDAGEYR")

#extracting the requested demographic variables related to SEQN , year, and age for 2014

extdata_demo_14<- rawdata_demo_14_year %>% 
  select("SEQN","year","RIDAGEYR")

#extracting the requested demographic variables related to SEQN , year, and age for 2016

extdata_demo_16<- rawdata_demo_16_year %>% 
  select("SEQN","year","RIDAGEYR")

#extracting the requested demographic variables related to SEQN , year, and age for 2018

extdata_demo_18<- rawdata_demo_18_year %>% 
  select("SEQN","year","RIDAGEYR")

#merging the extracted demographic data through the years 20112,14,16,18
demographic_data<- bind_rows(extdata_demo_12,extdata_demo_14,extdata_demo_16,extdata_demo_18)




####merging the two datasets created by us (oh exam and demographic) into a single data set, ignoring those participants who are not present in both the data sets merged.
# prior merge done on 16th of october had only merged but did not ignore participants who are not present in BOTH datasets, merge on 18th of october will be by their SEQN
merged_data <-merge(oh_exam,demographic_data, by= "SEQN")

# let us tally the number of participants by year
merged_data %>% group_by(year) %>% tally()
# there are : 
# 1  2012  8073
# 2  2014  8633
# 3  2016  8857
# 4  2018  8099


####saving into csv format in data folder
write.csv(merged_data, here("data", '18-10-2021_extracted-data-of-oral-exam-&-demographic.csv') )
