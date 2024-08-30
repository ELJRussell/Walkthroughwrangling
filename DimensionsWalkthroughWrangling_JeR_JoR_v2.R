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
    #remove entries with no vote
    filter(value != "") %>% 
    #change cols to factors with levels
    mutate(participantRole = factor(participantRole, levels= c("el coach", "principal", 'school coach', "asst principal", "district leader", "teacher", "el admin") ) ) %>% 
    mutate(value = factor(value, levels= c("Evident", "Somewhat Evident", "Not Evident") ) ) %>% 
    #arrange the data
    arrange(eventId, classroomId, identifier, participantRole, value) %>% 
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
  
  
  #count the number of votes for each group index
  s1 <- s0 %>%   
    group_by(group_index, eventId, classroomId, identifier, value) %>% 
    #count how many votes there are for each grouping
    summarize(vote_count = n()) 
  
  #label rows that are either the winning vote or are involved in ties as "TRUE."
  #rows not involved in ties and that are not the winning vote are labeled as "FALSE" 
  s2 <- s1 %>% 
    group_by(group_index, eventId, classroomId, identifier) %>% 
    #all rows with maximum will say TRUE. If there's a tie, there will be multiple TRUEs for the group_index.
    mutate(max_exists = vote_count == max(vote_count) ) 
  

  #extract rows with clear winners (no ties) by extracting rows / group-indices that only showed up once 
  #rows that showed up only once were those of winning votes (and their max_exists column is automatically TRUE)
  #count how many rows there are for each group_index. 
  #If there's only one, the group_index does NOT have a tie.
  s3 <- s2 %>%
    # remove all rows that do not have a max. these rows are not winners and are not involved in ties. 
    filter(max_exists != FALSE) %>% 
    group_by(group_index) %>% 
    filter( n() == 1 ) 
  
  #do a semi-join to extract participantRole info on winning vote
  s3_temp <- semi_join(s0, s3, by=c("group_index","value")) 
  
  s3 <- s3_temp %>%
    select(group_index, eventId, classroomId, identifier, value, participantRole)
  
  #select(-max_exists, -vote_count) %>% 
  
  #extract rows with ties. these rows have have max_exists = TRUE and more than 1 entry for a group_index
  #count how many rows there are for each group_index. 
  #If a group_index shows up more than once, the group_index does have a tie.
  s4 <- s2 %>%
    filter(max_exists != FALSE) %>% 
    group_by(group_index) %>% 
    filter( n() > 1 ) 
    
  num_of_ties = n_distinct(s4$group_index)  
  
  ## NOTE from John - I added "value" to the semi_join - correct me
  ## if I'm wrong, but without value, in a situation where the coach
  ## gave a minority rating while the tie was the other two ratings,
  ## this would prioritize their rating (saw they put Effective, while
  ## 2 others did Not, and 2 others did Somewhat). Adding in "value"
  ## to the semi_join filters out minority ratings, since they aren't in s4.
  # ^^^ 100% agree. Thank you! -Jessica
  
  #pull rows in s0 (original data w/ added row_id and group_id) with a match in s4 (groups with ties)
  group_w_ties <- semi_join(s0, s4, by=c("group_index", "value")) %>% 
    #add column to prioritize participant based on participant hierarchy. 
    # 1 is the most important. 7 is the least important. 
    mutate(participant_priority = case_when(
      participantRole == "el coach" ~ 1,
      participantRole == "principal" ~ 2,
      participantRole == "school coach" ~ 3,
      participantRole == "asst principal" ~ 4,
      participantRole == "district leader" ~ 5,
      participantRole == "teacher" ~ 6,
      participantRole == "el admin" ~ 7,
      participantRole == "other"  ~ 7,
      #make everything else a 7
      TRUE ~ 7
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
  
  
  rows_w_ties <- semi_join(s0, s4, by=c("group_index","value")) %>% 
    #add column to prioritize participant based on participant hierarchy. 1 is the most important. 7 is the least important. 
    mutate(participant_priority = case_when(
      participantRole == "el coach" ~ 1,
      participantRole == "principal" ~ 2,
      participantRole == "school coach" ~ 3,
      participantRole == "asst principal" ~ 4,
      participantRole == "district leader" ~ 5,
      participantRole == "teacher" ~ 6,
      participantRole == "el admin" ~ 7,
      participantRole == "other"  ~ 7,
      #make everything else a 7
      TRUE ~ 7
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
  break_ties <- group_w_ties %>% 
    group_by(group_index) %>%
    arrange(eventId, classroomId, identifier, participant_priority, value_priority) %>% 
    #add group_index column again just to see it at the end of df when using View() 
    mutate(group_index_ = group_index) %>% 
    slice(1) #keep the first row 
  
  #checking a tie
  # s0 %>% filter(eventId == 10986, classroomId == 56485, identifier == "4C")
  # group_w_ties %>% filter(eventId == 10986, classroomId == 56485, identifier == "4C")
  # break_ties %>% filter(eventId == 10986, classroomId == 56485, identifier == "4C")

  #tie has been broken. we only need a few variables. 
  s5 <- break_ties %>% 
    select(group_index, eventId, classroomId, identifier, value, participantRole)
  
  
  #this lists the winning value for all groups. (combines rows that had no ties and rows once ties were broken.)
  group_value <- rbind(s3, s5) %>% 
    #select(-max_exists, -vote_count) %>% 
    arrange(group_index, eventId, classroomId, identifier)
  
  #remove row_id and participantId to combine all data columns
  s6 <- s0 %>% 
    select(-row_id, -participantId) 
  
  #combine all data 
  cln_data <- s6 %>% 
    semi_join(group_value) %>% 
    slice(1) #keeps only first matching row for each eventId, classroomId, identifier
  
  #Create a dataframe in the global environment.
  assign(cln_dataName, cln_data, envir = .GlobalEnv)
  
  
  print( paste("The data has",  max(s0$group_index), "entries when grouped by eventId, classroomId, and identifier."))  
  print(paste("The data had ", num_of_ties, "ties."))
  
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


#quicker filter function. #only potentially useful for checking.
qf <- function(data, `eventId`, `classroomId`, `identifier`){
  data %>% 
    filter({{ eventId }} == eventId,
           {{classroomId }} == classroomId, 
           {{ identifier }} == `identifier`)
}


# Testing edge case ----
#created this dataset with a couple of edge cases. 
#some columns (that don't matter to the tie breaking algorithm were left blank). ignore the NAs.
edge <- read.csv("edge_case_data.csv") 
process_walkthrough_data(edge, "edge_clean", dataCheck = FALSE, setseed = 701, n = 4)

edge
edge_clean

#Testing walkthrough script ----
#Load data
load(here("data","Testwalkthroughs.RData")) 
data <- SY23_24walkthroughs

#Note: function requires cln_dataName to be entered in quotations.
process_walkthrough_data(data = SY23_24walkthroughs, cln_dataName = "clean_walkthrough", dataCheck = TRUE, setseed = 605, n = 15)

###########################################################
#Save clean walkthrough data as csv or RData file -----
###########################################################
#write.csv(clean_walkthrough, file="clean_SY23_24walkthroughs_test123.csv")
#save(clean_walkthrough, file="clean_SY23_24walkthroughs_test123.RData")


#Data Checks to ensure function is working properly.  -----
#Check
data %>%
  filter(eventId == 10986, classroomId == 56485, identifier == "4C") %>% 
  select(eventId, classroomId, identifier, participantRole, value)

clean_walkthrough %>% 
  filter(eventId == 10986, classroomId == 56485, identifier == "4C") %>% 
  select(eventId, classroomId, identifier, participantRole, value)


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


