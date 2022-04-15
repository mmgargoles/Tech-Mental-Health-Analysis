library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library("DT")
library('dplyr')
library('markdown')
library('tidyr')


####
survey <- read.csv('survey-cleaned.csv')
survey$year <- format(as.POSIXct(survey$Timestamp, format = "%Y-%m-%d %H:%M:%S"),format= "%Y")
quesToChoose <-c('Do you have a family history of mental illness?' = 'family_history'
                 ,'Have you sought treatment for a mental health condition?' = 'treatment'
                 ,'If you have a mental health condition, do you feel that it interferes with your work?' = 'work_interfere'
                 ,'Does your employer provide mental health benefits?' = 'benefits'
                 ,'Do you think that discussing a mental health issue with your employer would have negative consequences?' = 'mental_health_consequence'
                 ,'Do you feel that your employer takes mental health as seriously as physical health?' = 'mental_vs_physical'
)

