# Load tidyverse packages
library(sf)
library(absmapsdata)
library(tidyverse)
library(readr)
library(lubridate)
library(ggpubr)
library(readxl)
# Read files as tibbles
counselling <- read_csv(file="counselling.csv")
current <- read_csv(file="current.csv")
au_postcode<-read_csv(file = "au_postcodes.csv",col_types = cols(postcode_2016 = col_character()))
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


# Hao
counselling <- counselling %>%
  janitor::clean_names() %>%
  remove_meaningless_columns() %>%
  select(-x1, -id) %>%
  arrange(client_id, date_seen)

current <- current %>%
  janitor::clean_names() %>%
  remove_meaningless_columns() %>%
  select(-x1, -id) %>%
  arrange(client_id, contact_date)


table_join<-counselling %>%
  left_join(current, by="client_id")
  
############################above code adopted from rtssv.R line1~40
############################
############################
############################
############################
library(ggiraph)


dat<-table_join %>% 
     filter(!is.na(client_id),!is.na(postcode))  %>%
     rename(postcode_2016=postcode) %>%
	 select(client_id,postcode_2016) %>%
	 distinct() %>%
	 group_by(postcode_2016) %>%
	 summarise(nclient=length(unique(client_id)))  ###tidy client data;

vic.code<-au_postcode %>% 
          filter(state_code=='VIC') %>%
          select(postcode_2016,place_name) %>%
		  group_by(postcode_2016) %>%
		  summarise(place=str_c(unique(place_name),collapse=","))


vic<-postcode2016 %>% 
     select(postcode_2016) %>%
     filter(postcode_2016 %in% vic.code$postcode_2016) %>%
	 left_join(vic.code,by='postcode_2016') %>%
     left_join(dat,by='postcode_2016') %>%
	 mutate(label=str_c('<b>Places</b>:',place,'<br/><b>PostCode</b>:',postcode_2016,'<br/><b>N.Clients</b>:',str_replace_na(nclient,'none')))
 

vic.plot<-ggplot(vic,aes(geometry = geometry,fill = nclient,data_id=postcode_2016,tooltip=label)) +
  geom_sf_interactive(size = 0.2) +
  scale_fill_viridis_c(direction = -1) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
		plot.title = element_text(face=2,hjust = 0.5,size=rel(1.5)),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0)) +
  labs(fill = "Client Number",
       title = 'Clients in Victoria of Australia') 


mapgi<-girafe(ggobj=vic.plot, width_svg=14,height_svg=9.6,options = list(
        opts_sizing(rescale = FALSE),
        opts_tooltip(use_fill= TRUE),
		opts_zoom(min = 1, max = 6),
	    opts_hover_inv(css = "opacity:0.5;"),
        opts_hover(css = "stroke:#1279BF;cursor:pointer;"),
	    opts_toolbar(saveaspng=FALSE),
	    opts_selection(css = "stroke:#1279BF;cursor:pointer;",type='multiple')
        ))

mapgi  ##shown in webbrowser of in rmarkdown html 


library(htmlwidgets)  ####save as stand-alone htmlfile
saveWidget(mapgi,'nclientMap.html')

