# Make a change to test GitHub

rm(list=ls())

my.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.dir)

# Load some packages we might use
library(tidyverse)  
library(rvest)    
library(stringr)   
library(rebus)     
library(lubridate)
library(qdapRegex)
library (gsubfn)
library(dplyr)
library(xml2)

# Data skeleton for  presented and ordered printed 
data <- data.frame(session=factor(),
                   billnum=factor(),
                   billnum_abbrv=factor(),
                   desc=factor(),
                   #offered=factor(),
                   #prefiled=factor(),
                   type=factor(),
                   sponsors=factor(),
                   committees=factor(),
                   text=factor(),
                   stringsAsFactors=FALSE)

## First, only for last (top) bills - HB presented and ordered printed  
# First, loop through bill numbers (need to do this for Senate and JR etc. as well - loop later)

loopchams<-c("H", "S")

for(cham in loopchams){
  
for(i in 1:1500) {  
  i1 <- sprintf('%01d', i)
  url <- paste0("http://lis.virginia.gov/cgi-bin/legp604.exe?941+ful+",cham, "B", i1) # loop through S too
  page <- read_html(url) 
  dump <- page %>% html_node("#mainC") %>%
        html_text()
  session <- str_extract(dump, "[1-9]+ SESSION")
  if (length(session)==0){
    break 
  }
  if (is.na(session)==T){
    break 
  }
  billnum <- str_extract(dump, "\\w+ BILL NO. \\d+")
  billnum_abbrv <- paste0(cham, "B", i1)
  sponsors <- str_extract(dump, "Patron.*")
  committees <- str_extract(dump, "Referred to.*")
  dump <- str_replace_all(dump, "[\r\n]" , "")
  text <- str_extract(dump, "Be it.*")
  desc <- str_extract(dump, "A BILL.*\\.-")
  type <- "presented and ordered printed"
  t <- as.data.frame(cbind(session, billnum,billnum_abbrv, desc, type, sponsors, committees, text))
  try(data <- rbind(data,t), silent=T)
  Sys.sleep(2)
}
}

write.csv(data, file = "1994.csv")


#loop H and S around J and R
#loop through HJ as a class
#loop through HR as a class 
#loop through SJ as a class
#loop through SR as a class
