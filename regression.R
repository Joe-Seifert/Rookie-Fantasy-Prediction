###
#   This piece of the project informs the variables used to create the simulation models
#   on the excel spreadsheet.  The conclusion of the project as a whole is that college 
#   performance, in general, is not a good indicator of fantasy points produced by a WR.
###

#import the data set from excel
library(readxl)
CFB_Receiving_Data <- read_excel("~/Documents/Undergraduate/C.4 college seinor year/S22/BUSOBA 4242 - Sports/group - fantasy from college/CFB Receiving Data.xlsx",sheet = "Rookie Numbers v College")
View(CFB_Receiving_Data)   

#subset data by position
library(tidyverse)

WR_data <- CFB_Receiving_Data 

#run regression on different metrics
#our end goal is to predict fantasy points
factor_list <-  WR_data[22:30]


summary(lm(formula = `Fantasy Points` ~ Conf + Col_G + Col_Rec + Col_Rec_Yds + 
             Col_Rec_Avg + Col_Rec_TD, data = WR_data))
#Our R-squared for conference alone is higher than for the rest of the factors together
#it also looks like SEC is the only conference that really makes that much of a difference



# I want to separate conference from the other stats
summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + Col_Rec_Yds + 
             Col_Rec_Avg + Col_Rec_TD + 0, data = WR_data))

summary(lm(formula = `Fantasy Points` ~ Conf + 0, data = WR_data))



#####
# I want to do a point system on schools creating tier 1, 2, and 3 schools,
# that will be assigned points in the excel linear regression

#I will Start with the WRs
#regression_model_WR <- coefficients(summary(lm(formula = `Fantasy Points` ~ School + 0, data = WR_data)))
regression_model_WR <- lm(formula = `Fantasy Points` ~ School + 0, data = WR_data) %>%
  summary() %>%
  coefficients() %>%
  as.data.frame()

regression_model_WR %>%
  filter(Estimate >= 130)%>%
  arrange('Pr(>|t|)')

top_tier_WR_school <- c("Alabama","Clemson","LSU","Maryland","OhioState","OklahomaState",
                        "USC")
#this is the list of our top-tier schools for Wide Receivers


#running backs
"
regression_model_RB <- lm(formula = `Fantasy Points` ~ School + 0, data = RB_data) %>%
  summary() %>%
  coefficients() %>%
  as.data.frame()

regression_model_RB %>%
  filter(Estimate >= 130)%>%
  arrange('Pr(>|t|)')
" #I took the data from runningbacks out here because the group decided to just look at the WRs
top_tier_RB_school <- c("Alabama","Georgia","OhioState","Oklahoma","PennState")


#tight ends
"
regression_model_TE <- lm(formula = `Fantasy Points` ~ School + 0, data = TE_data) %>%
  summary() %>%
  coefficients() %>%
  as.data.frame()

regression_model_TE %>%
  filter(Estimate >= 100)%>%
  arrange('Pr(>|t|)')
"
top_tier_TE_school <- c("Arkansas","Florida","OleMiss","PennState")



#now i want to look at college stats against draft position and
# draft position to fantasy performance, which I think will prove useful
# in making a predictive model

PickRegData <- CFB_Receiving_Data %>%
  filter(Pick != "Undrafted") #The data from the undrafted players is much more sparratic than the rest

#first, I want to look at the correlation between draft position and performance
summary(lm(formula = `Fantasy Points` ~ Round + 0, data = PickRegData))
#this seems to be significant

#this is the summary of different college stats effect on  draft position
summary(lm(formula = Pick ~ Conf + Col_G + Col_Rec + Col_Rec_Yds + 
             Col_Rec_Avg + Col_Rec_TD + 0,
           data = PickRegData))

#this is the summary of different college stats effect on draft round
summary(lm(formula = Round ~ Conf + Col_G + Col_Rec + Col_Rec_Yds + 
             Col_Rec_Avg + Col_Rec_TD + 0,
           data = PickRegData))


#I want to do the same thing again, but without considering conference
#this is the summary of different college stats effect on  draft position
summary(lm(formula = Pick ~ Col_G + Col_Rec + Col_Rec_Yds + 
             Col_Rec_Avg + Col_Rec_TD + 0,
           data = PickRegData))

#this is the summary of different college stats effect on draft round
summary(lm(formula = Round ~ Col_G + Col_Rec + Col_Rec_Yds + 
             Col_Rec_Avg + Col_Rec_TD + 0,
           data = PickRegData))


#testing interplay between variables for both proposed models
#we want to see if college receiving average + rec TDS is better than just
# Rec_Avg, which has the highest individual significance
summary(lm(formula = `Fantasy Points` ~ Col_Rec_Avg * Col_Rec_TD + 
             0, data = PickRegData))
#it looks like the correlation does not improve the predictability, so I will
# not include receiving tds in the model

#for the other model (indirect college stats through draft stock), rec td
# is the most significant factor in determining draft stock, followed by games
# so I will test the interplay of these two variables here
summary(lm(formula = Pick ~ Col_G * Col_Rec_TD + 0,
           data = PickRegData))
summary(lm(formula = Round ~ Col_G * Col_Rec_TD + 0,
           data = PickRegData))
#for this model, including the secondary factor makes the estimate make more
# sense, and the model is still significant



#model creation
lm(formula = `Fantasy Points` ~ Conf * Col_Rec_Avg, data = CFB_Receiving_Data)


#jordan college fantasy points indicator
CFB_Receiving_Data %>%
  mutate(Col_Rec_Yds_G = Col_Rec_Yds/Col_G) %>%
  mutate(col_fantasy = Col_Rec_Yds_G/10 + 
         (Col_Rec_TD*6)/Col_G + 
         (Col_Rec/Col_G)*0.5)



