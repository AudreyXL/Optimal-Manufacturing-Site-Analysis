##Code to clean metropolitan statistical Area Data using R
#install.packages("tidyverse")
library(tidyverse)
library(readr)
#install.packages("janitor")
#install.packages("ggmap")
#install.packages("ggplot")
#install.packages("ggthemes")

library(janitor)
#citation("ggmap")
library(ggmap)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(ggmap)
#library(data.table)
##1
##Create msa column
final_2_dat<-read_csv("final_2.csv")
final_2_dat<-mutate(final_2_dat, msa=geo_name)
final_2_dat$msa <- gsub(" \\(Metropolitan Statistical Area\\)", "", final_2_dat$msa)

##2
##create cleaned_final_2_dat
cleaned_final_2_dat<-final_2_dat %>% select(msa, description, x2018) %>% rename("value"=x2018)

##3
##Edit Value Column
cleaned_final_2_dat[ cleaned_final_2_dat == "(D)" ] <- NA
##4
cleaned_final_2_dat$value<-gsub(" E", "", cleaned_final_2_dat$value)
cleaned_final_2_dat$value<-gsub(",", "", cleaned_final_2_dat$value)
##col<-cleaned_final_2_dat %>% select(value)
cleaned_final_2_dat$value<-as.numeric(cleaned_final_2_dat$value)
print(cleaned_final_2_dat)

##5
Find MSA with the lowest per capita personal income
usable_5<-cleaned_final_2_dat %>% group_by(msa) %>% spread(description, value)
usable_5<-usable_5 %>% rename("msa"=msa,"manufacturing"=Manufacturing,
                              "comp"="Computer and electronic product manufacturing",
                              "elect"="Electrical equipment, appliance, and component manufacturing",
                              "data"="Data processing, hosting, and related services",
                              "pers_inc"="Personal income (thousands of dollars)",
                              "pop"="Population (persons) 1/",
                              "pers_inc_per_cap"="Per capita personal income (dollars) 2/")
##6
##Find MSA with Lowest per Capita personal income
manu_wo_na <- usable_5 %>% drop_na(manufacturing)
manu<-manu_wo_na[order(-manu_wo_na$manufacturing),]
manu<-head(manu, round(nrow(manu)*0.25))
lowest<-manu[order(manu$pers_inc_per_cap),]
manuf_low_cost<-lowest[[1,1]]

##7
## Split MSA into State
cleaned_split<-usable_5 %>% separate(msa,into=c("msa", "state"),sep = ", ")
cols <- c("state", "msa", "manufacturing", "comp", "elect", "data", "pers_inc", "pop", "pers_inc_per_cap")
cleaned_split<-cleaned_split[,cols]

##8
##Convert cleaned_split to long
cleaned_split_long<-gather(cleaned_split,key="description", value="value",3:9)

##9
##Create msa_large function
msa_large<-function(x) {
  tbl <- cleaned_split_long %>% 
    filter(state==x) %>%
    group_by(description) %>%
    summarize(max_msa = msa[which.max(value)]) %>%
    spread(description, max_msa)
  return(tbl)
}

##10
##Create msa_large2 function
msa_large2<-function(x) {
  tbl <- cleaned_split_long %>% 
    filter(state==str_to_upper(x)) %>%
    group_by(description) %>%
    summarize(max_msa = msa[which.max(value)]) %>%
    spread(description, max_msa)
  return(tbl)
}
 



