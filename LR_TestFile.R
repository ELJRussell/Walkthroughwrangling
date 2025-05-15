
# LR Test File to Practice GitHub

library(here)
library(tidyverse)
library(jsonlite)


SY24_25walkthroughs <- read_json(here("data","SY24-25walkthroughs.json"), simplifyVector = TRUE) |>
  mutate(json_parsed = map(walkthroughdata, ~ jsonlite::fromJSON(., flatten=TRUE)))  |> 
  unnest(json_parsed) |> 
  unnest(fields)  |> 
  select(!walkthroughdata)

data <- SY24_25walkthroughs


## Do some explorations

# who mostly enters data
table (data$participantRole)
## huge majority of data is entered by EL Coach


# do people upload images?
table(data$imageUpload.show)
## surprisingly many people actually do upload images


