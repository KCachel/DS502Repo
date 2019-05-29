library(tidyverse)

TedRaw <- read.csv(file="/cloud/project/FinalProject/ted_main.csv", header=TRUE, sep=",")

#comments(as is)
#duration (as is)
#languages (as is)
#views (as is)

TedRaw$film_date <- NULL
TedRaw$name <- NULL
TedRaw$num_speaker <- NULL
TedRaw$published_date <- NULL
TedRaw$ratings <- NULL
TedRaw$related_talks <- NULL
TedRaw$url <- NULL

####################
## title
##loop through top 20 words and create boolean columns for them

for(i in 1:20){
  col_name <- paste("tags",d_title$word[i], sep = "_")
  
  TedRaw <-  TedRaw %>%
    mutate(!!col_name := 
             grepl(d_title$word[i], TedRaw$title, fixed = TRUE))
  
}

TedRaw$title <- NULL


####################
## tags
##loop through top 50 tags and create boolean columns for them

for(i in 1:50){
  col_name <- paste("tags",d_tags$word[i], sep = "_")
  
  TedRaw <-  TedRaw %>%
    mutate(!!col_name := 
             grepl(d_tags$word[i], TedRaw$tags, fixed = TRUE))
  
}

TedRaw$tags <- NULL



####################
## for speaker_occupation
##loop through top 15 occupations and create boolean columns for them

for(i in 1:15){
  col_name <- paste("speaker_occupation",speaker_occupationtop$speaker_occupation[i], sep = "_")
  
  TedRaw <-  TedRaw %>%
    mutate(!!col_name := 
             grepl(speaker_occupationtop$speaker_occupation[i], TedRaw$speaker_occupation, fixed = TRUE)
    )
  
}

TedRaw$speaker_occupation <- NULL


####################
## for main_speaker
##loop through top 15 speakers and create boolean columns for them

for(i in 1:15){
  col_name <- paste("speaker",main_speakertop$main_speaker[i], sep = "_")
  
  TedRaw <-  TedRaw %>%
    mutate(!!col_name := 
             grepl(main_speakertop$main_speaker[i], TedRaw$main_speaker, fixed = TRUE))
  
}

TedRaw$main_speaker <- NULL


###################
# for event 
## loop through the top 20 events and create boolean columns for them

for(i in 1:20){
  col_name <- paste("event",eventtop$event[i], sep = "_")
  
  TedRaw <-  TedRaw %>%
    mutate(!!col_name := 
             grepl(eventtop$event[i], TedRaw$event, fixed = TRUE))
  
}

TedRaw$event <- NULL



####################
## for description
##loop through top 30 description words and create boolean columns for them
for(i in 1:30){
  col_name <- paste("description",d_description$word[i], sep = "_")
  
  TedRaw <-  TedRaw %>%
    mutate(!!col_name := 
             grepl(d_description$word[i], TedRaw$description, fixed = TRUE))
  
}

TedRaw$description <- NULL

TedClean <- TedRaw %>% dplyr::rename_all(funs(make.names(.)))