## Goal: To wrangle a dataset such that each classroom in a walkthrough is scored correctly using the guidelines
## given here: https://docs.google.com/document/d/1EjshjMoKBF6DJj2d7YTJoVd03-Kh7XT9iO57h9zHgfM/edit?usp=sharing

## Sample dataset - in /data, you will find a goodly portion of 
## last year's walkthroughs, scrubbed to have very little identifiable information

## Important columns in this dataset:
## eventId - every event is unique to a school, so while the schoolid, schoolname and districtId are important, the 
##           eventId is probably what you really want to hone in on
## status - this is an interesting variable that John is keeping track of - he is unsure if Dimensions keeps it into account
##           when going through scoring
## participantRole - this tells you what role the person scoring has - el coach is important as the first tie breaker
## classroomId - Dimensions treats each classroom within an event as a unique grouping, so we should do the same
## identifier - annoyingly, name and identifier are not unique to each question across walkthroughs, although the are internally
##              unique, so you can use it as a grouping variable as well
## value - this is where you get our three values, as well as an empty value

## so the challenge will be to figure out how to the following:
## 1) use group_by by eventId, identifier and classroomId to get down to where Dimensions makes its calculations
## 2) sum up the number of Evidents, Somewhat Evidents, and Not Evident (maybe do a second level of group_by?)
## 3) use the highest ranking observer as a tie breaker (so if is is 2-2-0 for E-SE-NE, then whichever one the el coach decided
## is the indicator to use)

## I wonder if factoring and slicing could be a way to get around this? That is - factor the participant role, arrange it, and then
## when you group_by and calculate, you could also slice so that the highest ranking participant is also reported

## I leave it up to you! It's a neat little problem to solve, and I look forward to hearing how you do it!

library(here)
library(tidyverse)

data <- SY23_24walkthroughs

# Walkthrough data cleaning function ----
# Tie-breaking guidelines: https://docs.google.com/document/d/1EjshjMoKBF6DJj2d7YTJoVd03-Kh7XT9iO57h9zHgfM/edit?usp=sharing
process_walkthrough_data <- function(data, cln_dataName, dataCheck=TRUE, setseed=2024, n=5){
  #data: is the original data frame.
  
  #cln_dataName is the name you want for the clean data frame. 
  #cln_dataName must be entered in quotations.
  
  #dataCheck will print out the rows that had ties from the original data set and the final selection from the cleaned data.
  #default is TRUE.
  
  #setseed is set but can be changed.
  
  #n is the number of ties you want to randomly sample from the original data set. 
  #If the number of ties is less than the inputted value for n, you will receive an error. 
  #If there are no ties, please set n to 0 or dataCheck = FALSE. 
  
  data <- data %>% 
    #change the order of columns
    select(eventId, classroomId, identifier, participantRole, value, status, districtId, schoolid, schoolname, eventname, eventId, walkthroughid, focusGroup, name, subtext, participantId) %>% 
    #arrange the data
    arrange(eventId, classroomId, identifier) %>% 
    #remove entries with no vote
    filter(value != "") %>% 
    #create a row id
    mutate(row_id = row_number() ) %>% 
    #move row_id to front of all columns
    select(row_id, everything())
  
  s0 <- data %>% 
    group_by(eventId, classroomId, identifier) %>% 
    #create group index for all groups
    mutate(group_index = cur_group_id() ) %>% 
    #move group index to the front
    select(group_index, everything())
  
    
  #count the number of votes for each group
  s1 <- s0 %>%   
    group_by(group_index, eventId, classroomId, identifier, value) %>% 
    #count how many votes there are for each grouping
    summarize(vote_count = n()) 
  
  
  s2 <- s1 %>% group_by(group_index, eventId, classroomId, identifier) %>% 
    #all rows with maximum will say TRUE. If there's a tie, there will be multiple TRUEs for the group_index.
    mutate(max_exists = vote_count == max(vote_count) ) %>% 
    #remove all rows that do not have a max.
    filter(max_exists != FALSE) 
  
  #rows with no duplicate group_index
  #count how many rows there are for each group_index. If there's only one, the group_index does NOT have a tie.
  s3 <- s2 %>%
    group_by(group_index) %>% 
    filter( n() == 1 ) 
  
  #rows with multiple entries for a group_index. rows with ties.
  #count how many rows there are for each group_index. If there are more than 1, the group_index does have a tie.
  s4 <- s2 %>%
    group_by(group_index) %>% 
    filter( n() > 1 ) 

  ## NOTE from John - I added "value" to the semi_join - correct me
  ## if I'm wrong, but without value, in a situation where the coach
  ## gave a minority rating while the tie was the other two ratings,
  ## this would prioritize their rating (saw they put Effective, while
  ## 2 others did Not, and 2 others did Somewhat). Adding in "value"
  ## to the semi_join filters out minority ratings, since they aren't in s4
    
  rows_w_ties <- semi_join(s0, s4, by=c("group_index","value")) %>% 
    #add column to prioritize participant based on participant hierarchy. 1 is the most important. 7 is the least important. 
    mutate(participant_priority = case_when(
      participantRole == "el coach" ~ 1,
      participantRole == "principal" ~ 2,
      participantRole == "school coach" ~ 3,
      participantRole == "asst principal" ~ 4,
      participantRole == "district leader" ~ 5,
      participantRole == "teacher" ~ 6,
      participantRole == "other" ~ 7,
    )) %>% 
    #add column to prioritize the most positive vote. 
    mutate(value_priority = case_when(
      value == "Evident" ~ 1, 
      value == "Somewhat Evident" ~ 2,
      value == "Not Evident" ~ 3, 
      value == "" ~ NA
    )) %>% 
    #arrange data
    arrange(group_index, eventId, classroomId, identifier, participant_priority, value_priority) %>% 
    #bring group_index to the front
    select(group_index, everything()) %>% 
    group_by(group_index) %>%
    arrange(eventId, classroomId, identifier, participant_priority, value_priority) 
  
  #break tie once data has been arranged to keep highest priority participant and most positive vote
  break_ties <- rows_w_ties %>% 
    group_by(group_index) %>%
    arrange(eventId, classroomId, identifier, participant_priority, value_priority) %>% 
    slice(1) #keep the first row 
  
  #tie has been broken. 
  s5 <- break_ties %>% 
    select(group_index, eventId, classroomId, identifier, value)
  
  
  #this lists the winning value for all groups (with and without ties) 
  group_value <- rbind(s3, s5) %>% 
    select(-max_exists, -vote_count) %>% 
    arrange(group_index, eventId, classroomId, identifier)
  
  #remove row_id and particpantId to combine all data columns
  s6 <- s0 %>% 
    select(-row_id, -participantId) 
  
  #combine all data 
  cln_data <- s6 %>% 
    semi_join(group_value) %>% 
    slice(1) #keeps only first matching row for each eventId, classroomId, identifier
  
  #Create a dataframe in the global environment.
  assign(cln_dataName, cln_data, envir = .GlobalEnv)
  
  
  print( paste("The data has",  max(s0$group_index), "entries when grouped by eventId, classroomId, and identifier."))  
  print(paste("The data had ", nrow(break_ties), "ties."))
  
  #----- Data Check Function -----
  dataCheck_fn <- function(dataCheck, setseed , n){
    
    if (dataCheck) {
      
      #Add message. dataCheck_fn will not run if the number of ties is less than the inputted value of `n`.
      if (n > nrow(break_ties)){
        stop("Error: There are ", nrow(break_ties), " ties in the dataset. Please provide an `n` that is less than or equal to ", nrow(break_ties), ". If there are no ties, you can also set `dataCheck = FALSE`.")
      }
      
      set.seed(setseed)
      check_sample <- rows_w_ties[sample(1:nrow(rows_w_ties), n) , c("eventId", "classroomId", "identifier")]
      
      for (i in 1:nrow(check_sample)){
        
        print(paste("--------------------- CHECK NUMBER:", i, "----------------------"))
        
        print(paste("----- ORIGINAL DATA ------"))
        print(
          data %>% 
            filter(value != "") %>% 
            select(eventId, classroomId, identifier, value, participantRole) %>% 
            filter(eventId %in% check_sample[i, 1]) %>% 
            filter(classroomId %in% check_sample[i, 2]) %>% 
            filter(identifier %in% check_sample[i, 3])
        )
        
        print(paste("----- CLEAN DATA ------"))
        print(
          cln_data %>% 
            select(eventId, classroomId, identifier, value, participantRole) %>% 
            filter(eventId %in% check_sample[i, 1]) %>% 
            filter(classroomId %in% check_sample[i, 2]) %>% 
            filter(identifier %in% check_sample[i, 3])
        )
      }
    }
  }
  
  #Run Data Check function if dataCheck = TRUE
  if (dataCheck){
    dataCheck_fn(TRUE, setseed, n)
  }
  
}

#Load data
load(here("data","Testwalkthroughs.RData")) 

#Note: function requires cln_dataName to be entered in quotations.
process_walkthrough_data(data = SY23_24walkthroughs, cln_dataName = "clean_SY23_24walkthroughs_test123", dataCheck = FALSE, setseed = 701, n = 5)

#write.csv(Clndata0, file="clean_SY23_24walkthroughs_test123.csv")
#save(Clndata0, file="clean_SY23_24walkthroughs_test123.RData")


#More checks:
testdata1 <- SY23_24walkthroughs %>% 
  filter(eventId != 10075) %>% 
  arrange(eventId, classroomId, identifier)

#This should return an error. 
process_walkthrough_data(testdata1, "Clndata1", dataCheck = TRUE, setseed = 701, n = 100000)
#This will not return an error. 
process_walkthrough_data(testdata1, "Clndata1", dataCheck = TRUE, setseed = 701, n = 4)


testdata2 <- testdata1[ sample(1:100, 100055, replace=TRUE), ]
process_walkthrough_data(testdata2, "Clndata2", dataCheck = FALSE)
