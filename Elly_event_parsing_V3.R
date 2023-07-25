setwd("C:/Users/Axel/Desktop/Psych Stuff/Faulkner") #set wd
###load in packages
library(tidyverse) 
library(tidytext)
library(stringr)
library(strex)
library(xlsx)

all_events <- read.csv("events.csv") #load events data
LIA_events <- all_events[all_events$SourceTextCode=="ELY",] #subset to just ones from Light in August
remove(all_events) #remove from environment to save space
LIA_events <- LIA_events[order(LIA_events$OrderWithinPage),] #reorder by page order (i.e., order within the text itself)

###create begin and end word columns from snippet###
for (i in 1:nrow(LIA_events)) {
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], ' aint ', " ain't ") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], ' Aint ', " Ain't ") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], 'dont', "don't") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], 'Dont', "Don't") #convert '\n' into new line
  LIA_events$First.8.10.words.of.event[i] <- str_replace_all(LIA_events$First.8.10.words.of.event[i], 'Mrs', "Mrs.") #convert '\n' into new line
}
LIA_events$begin_word <- word(LIA_events$First.8.10.words.of.event, 1,3) #pull first 3 words to identify chunk

LIA_events$end_word <- "" #create end word chunk identifier
i <- 0 
for (i in 1:nrow(LIA_events)) {
  LIA_events$end_word[i] <- LIA_events$begin_word[i+1]
} #set the end word of row[i] to the begin word of the next row
LIA_events$end_word[nrow(LIA_events)] <- "THE END" #last row is missing, so set to 'THE END'
###


###
LIA <- as.data.frame(read_file("william-faulkner-elly.txt")) #load in Light in August text as dataframe
colnames(LIA) <- "text"
#clean up the text
LIA$text <- sub(".*\n4\n","",LIA$text) #remove edition information
LIA$text <- str_replace_all(LIA$text, "", "") #remove unknown characters
LIA$text <- str_replace_all(LIA$text, '\n[0-9]+\n', '') #remove page numbers
LIA$text <- str_replace_all(LIA$text, '\nfWilliam Faulkner  LIGHT IN AUGUST\n', '') #remove header
LIA$text <- str_replace_all(LIA$text, '\fWilliam Faulkner  LIGHT IN AUGUST', '') #remove broken header
LIA$text <- str_replace_all(LIA$text, '\n', ' ') #convert '\n' into new line
LIA$text <- str_replace_all(LIA$text, '\f', '') #remove page break notification [][][][][]
###
LIA2 <- LIA

LIA$text <- str_to_lower(LIA$text)
LIA_events$begin_word <- str_to_lower(LIA_events$begin_word)
LIA_events$end_word <- str_to_lower(LIA_events$end_word)
LIA_events$begin_word <- str_replace_all(LIA_events$begin_word, '[[:punct:]]', "")
LIA_events$end_word <- str_replace_all(LIA_events$end_word, '[[:punct:]]', "")
LIA$text <- str_replace_all(LIA$text, '[[:punct:]]', "")


LIA_events$excerpt <- ""
temp_locations <- data.frame(0,0)
colnames(temp_locations) <- c("start", "end")
temp_locations$start <- as.numeric(temp_locations$start)
temp_locations$end <- as.numeric(temp_locations$end)
for (i in 1:nrow(LIA_events)) {
  if (i==1) {
    temp_locations <- as.data.frame(str_locate(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep=""))) #save indices of this excerpt
    #LIA$text <- str_sub(LIA$text, temp_locations$end[i], -1)
  } else {
    temp_locations <- rbind(temp_locations, as.data.frame(str_locate(LIA$text, paste('(', LIA_events$begin_word[i], ')', '.\\s*(.*?)\\s*(?=', LIA_events$end_word[i],')', sep="")))) #save indices of this excerpt
    #LIA$text <- str_sub(LIA$text, temp_locations$end[i], -1)
  }
  if (is.na(temp_locations$start[i])) {
    LIA_events$excerpt[i] <- NA
  } else if (((temp_locations$end[i] - temp_locations$start[i]) > 7000)) {
    LIA_events$excerpt[i] <- NA
    temp_locations$start[i] <- NA
    #temp_locations$end[i] <- NA
  }
}
sum(is.na(temp_locations$start) == TRUE)
sum(is.na(temp_locations$end) == TRUE)

for (i in 1:nrow(temp_locations)) {
  if (i > 1) {
    if (!is.na(temp_locations$end[i]) & !is.na(temp_locations$end[i-1])) {
      if (temp_locations$end[i] < temp_locations$end[i-1]) {
        temp_locations$end[i] <- NA
      }
    }
    if (!is.na(temp_locations$start[i]) & !is.na(temp_locations$start[i-1])) {
      if (temp_locations$start[i] < temp_locations$start[i-1]) {
        temp_locations$start[i] <- temp_locations$end[i-1] + 1
      }
    }
    if (is.na(temp_locations$start[i]) & is.numeric(temp_locations$end[i]) & is.numeric(temp_locations$start[i-1])) {
      temp_locations$start[i] <- temp_locations$end[i-1] + 1
      #temp_locations[i,1] <- as.data.frame(str_locate_nth(LIA$text, paste('(', LIA_events$begin_word[i], ')'), 2))[,1]
    }
  }
}
temp_locations$end[nrow(temp_locations)] <- nchar(LIA$text) #let last line go until text ends
sum(is.na(temp_locations$start) == TRUE)
sum(is.na(temp_locations$end) == TRUE)

substr(LIA$text, 1, 500)
substr(LIA2$text, 1, 500)

for (i in 1:nrow(LIA_events)) {
  if (!is.na(temp_locations$start[i]) & !is.na(temp_locations$end[i])) {
    if (temp_locations$end[i] - temp_locations$start[i] > 32767) {
      LIA_events$excerpt[i] <- NA
      temp_locations$end[i] <- NA
    }
    LIA_events$excerpt[i] <- str_sub(LIA$text, temp_locations$start[i], temp_locations$end[i])
  }
}

LIA_events$startIndex <- temp_locations$start
LIA_events$endIndex <- temp_locations$end
write.xlsx2(LIA_events, "ELY_events.xlsx")
###


str_locate_all(LIA2$text, '[:punct:]')









#see which did better
sum(is.na(LIA_events$excerpt))
sum(nchar(LIA_event_sentences$event_sentence) < 25)



write.csv(LIA_event_sentences, "LIA_event_sentences.csv")
#install.packages("xlsx")
library(xlsx)
write.xlsx(LIA_event_sentences, "LIA_event_sentences.xlsx")
