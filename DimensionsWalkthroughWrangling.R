## Goal: To wrangle a dataset such that each classroom in a walkthrough is scored correctly using the guidelines
## given here: https://docs.google.com/document/d/1EjshjMoKBF6DJj2d7YTJoVd03-Kh7XT9iO57h9zHgfM/edit?usp=sharing

## Sample dataset - in /data, you will find the a goodly portion of 
## last year's walkthroughs, scrubbed to have very little identifiable information

library(here)
library(tidyverse)

load(here("data","Testwalkthroughs.RData")) 

## so the challenge will be to figure out how to the following:
## 1) use group_by by event and classroom to get down to where Dimensions makes its calculations
## 2) sum up the number of Evidents, Somewhat Evidents, and Not Evident (maybe do a second level of group_by?)
## 3) use the highest ranking observer as a tie breaker (so if is is 2-2-0 for E-SE-NE, then whichever one the el coach decided
## is the indicator to use)

## I wonder if factoring and slicing could be a way to get around this? That is - factor the participant role, arrange it, and then
## when you group_by and calculate, you could also slice so that the highest ranking participant is also reported

## I leave it up to you! It's a neat little problem to solve, and I look forward to hearing how you do it!