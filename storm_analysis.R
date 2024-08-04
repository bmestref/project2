
dat <- read.csv('data/repdata_data_StormData.csv.bz2')

length_of_file <- length(unique(dat$EVTYPE))
print(length_of_file)

event_types <- dat$EVTYPE
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
#Update the data frame
dat$EVTYPE <- event_types


library(dplyr)
eventype_fatalities <- as.data.frame(dat %>% group_by(EVTYPE) %>% summarise(fatalities = sum(FATALITIES), injuries = sum(INJURIES)))

#We yield the top 10 most fatal event types
print(head(eventype_fatalities[order(eventype_fatalities$fatalities, decreasing = TRUE), c('EVTYPE', "fatalities")], 10))

#We yield the top 10 event types which caused more injuries
print(head(eventype_fatalities[order(eventype_fatalities$injuries, decreasing = TRUE), c("EVTYPE","injuries")], 10))

#Let's first create a function which will return the exponent digit given a letter indicating the exponent
exp_function <- function(x){
  if(x %in% c('k', 'K')){
    return(3)
  }
  else if(x %in% c('M', 'm')){
    return(6)
  }
  else if(x %in% c('B', 'b')){
    return(9)
  }
  else if(x %in% c('h','H')){
    return(2)
  }
  else if(!is.na(as.numeric(x))){
    return(as.numeric(x))
  }
  else if(x %in% c('', '-', '?', '+')){
    return(0)
  }
  else{
    stop('Invalid entry')
  }
}
#Now let's add this changes to our original data file
prop_dmg_exp <- sapply(dat$PROPDMGEXP, FUN = exp_function)
dat$prop_dmg <- dat$PROPDMG * (10**prop_dmg_exp)
crop_dmg_exp <- sapply(dat$CROPDMGEXP, FUN = exp_function)
dat$crop_dmg <- dat$CROPDMG * (10**crop_dmg_exp)


#Next, let's calculate the total economic loss per event type
econ_loss <- as.data.frame(dat %>% group_by(EVTYPE) %>% summarise(crop_loss = sum(crop_dmg), prop_loss = sum(prop_dmg)))

#We yield the top 10 event types that caused the highest crop losses
print(head(econ_loss[order(econ_loss$crop_loss, decreasing = TRUE),c('EVTYPE','crop_loss')], 10))

#We now yield the top 10 event types which caused the highest property losses
head(econ_loss[order(econ_loss$prop_loss, decreasing = TRUE),c('EVTYPE','prop_loss')], 10)

library(ggplot2)
library(gridExtra)
eventype_fatalities_filter <- eventype_fatalities[order(eventype_fatalities$fatalities, decreasing = TRUE),]
eventype_injuries_filter <- eventype_fatalities[order(eventype_fatalities$injuries, decreasing = TRUE),]

p1 <- ggplot(data = head(eventype_fatalities_filter,10), aes(x = EVTYPE, y = fatalities)) + geom_bar(stat = 'identity') + labs(x = 'Event type', y = 'Number of fatalities', title = 'Fatalities as a function of the Event type') + coord_flip()
p2 <- ggplot(data = head(eventype_injuries_filter,10), aes(x = EVTYPE, y = injuries)) + geom_bar(stat = 'identity') + labs(x = 'Event type', y = 'Number of injuries', title = 'Injuries as a function of the Event type') + coord_flip()
grid.arrange(p1, p2, top="Top deadly weather events in the US (1950-2011)")

econ_loss_prop_filter <- head(econ_loss[order(econ_loss$prop_loss, decreasing = TRUE),c('EVTYPE','prop_loss')], 10)
econ_loss_crop_filter <- head(econ_loss[order(econ_loss$crop_loss, decreasing = TRUE),c('EVTYPE','crop_loss')], 10)

p1 <- ggplot(data = econ_loss_prop_filter, aes(x = EVTYPE, y = prop_loss)) + geom_bar(stat = 'identity') + labs(x = 'Event type', y = 'Porperty losses', title = 'Property losses as a function of the Event type') + coord_flip()
p2 <- ggplot(data = head(econ_loss_crop_filter,10), aes(x = EVTYPE, y = crop_loss)) + geom_bar(stat = 'identity') + labs(x = 'Event type', y = 'Crop losses', title = 'Crop losses as a function of the Event type') + coord_flip()
grid.arrange(p1, p2, top="Weather costs to the US economy (1950-2011)")
