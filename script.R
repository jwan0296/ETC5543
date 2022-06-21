# Load tidyverse packages
library(tidyverse)
library(readr)
library(janitor)
library(stringr)
library(lubridate)
# Read files as tibbles
referrals <- read_csv(file="referrals.csv")

# Function to check if all values in a vector are equal
is_constant <- function(x) {
  isTRUE(all.equal(x, rep(x[1],length(x))))
}

# Function to remove columns containing only missing values or all values equal
remove_meaningless_columns <- function(object) {
  all_miss <- colSums(is.na(object)) == NROW(object)
  all_equal <- apply(object, 2, is_constant)
  return(object[,!all_miss & !all_equal])
}


# Create main tables as tibbles
referrals_clean1 <- referrals %>%
  janitor::clean_names() %>%
  remove_meaningless_columns() %>%
  arrange(date_received)%>%
  mutate(client_type = str_to_lower(client_type))

#check variables' type
glimpse(referrals_clean1)

#change variable name
referrals_clean1<-referrals_clean1%>%rename(post_code=we_suggest_you_do,region=postcode)

# combining
referrals_clean2<-referrals_clean1%>%
  mutate(client_type = recode(client_type, beareaved = "bereaved person", witnes= "witness","witnes on scene"="witness", .missing = "unknown"))%>%
  mutate(referred_by = recode(referred_by, .missing = "unknown"))
  
referrals_clean2 <- referrals_clean2 %>%
  mutate(
    client_type = if_else(str_detect(client_type, "bereaved"), "bereaved", client_type)
  ) %>%
  mutate(
    client_type = if_else(str_detect(client_type, "parent|fam|fr|mother|partner|work|employ"), "fam/fr of casualty", client_type)
  ) %>%
  mutate(
    client_type = if_else(str_detect(client_type, "driver"), "driver", client_type)
  ) %>%
  mutate(
    client_type = if_else(str_detect(client_type, "scooter|cyc|motor"), "rider", client_type)
  ) %>%
  mutate(
    client_type = if_else(str_detect(client_type, "injure|rage"), "other injured person", client_type)
  ) %>%
  mutate(
    client_type = if_else(str_detect(client_type, "witness|scene"), "witness", client_type)
  ) %>%
  mutate(
    client_type = if_else(str_detect(client_type, "pass|pedes"), "passenger", client_type)
  ) %>%
  mutate(
    client_type = if_else(str_detect(client_type, "ccccc|group|emerg|second|inap"), "other", client_type)
  ) %>%
  mutate(
    referred_by = if_else(str_detect(referred_by, "General|Hospital|Social|Internet|GP|Ambulance|RTSSV|Solicitor|After|Agencies|Counsellor|Media|Metro"), "Other", referred_by)
  )
  


referrals_clean3<-referrals_clean2%>%
  mutate(referred_by=str_to_lower(referred_by))
  select(-x1)%>%
  mutate(day_of_week = wday(date_received, label = TRUE)) #, post_code = as.numeric(post_code)  can not find the place 8344 gipsland

referrals_clean3%>%group_by(day_of_week)%>%summarise(n())

# check the results  
client_type_count<-referrals_clean2%>%
  count(client_type)
referredby_count<-referrals_clean2%>%
  count(referred_by)  
#1.lowercase the refer variable
#2.clean the rest column
referrals_clean2%>%group_by(date_received)%>%summarise(n())
#3.do analysis according to the requirement
#4.push to git in time.