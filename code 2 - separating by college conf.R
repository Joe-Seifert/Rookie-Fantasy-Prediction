###
#   In regression.r, which is the variable regression for the project, I discovered that
#   conference is the variable with highest Rsq with fantasy points.  This is me analyzing the
#   differences between the conferences in an attempt to determine which conference has the
#   highest positive correlation to fantasy performance
###

library(readxl)
CFB_Receiving_Data <- read_excel("~/Documents/Undergraduate/C.4 college seinor year/S22/BUSOBA 4242 - Sports/group - fantasy from college/CFB Receiving Data.xlsx", sheet = "Rookie Numbers v College")
View(CFB_Receiving_Data)

library(tidyverse)
#now we want to make a separate data set for each conference

conf_list <- unique(CFB_Receiving_Data$Conf)

SEC_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[1])

pac12_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[2])

big10_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[3])

ACC_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[4])

big12_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[5])

Ind_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[6])

American_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[7])

MAC_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[8])

MWC_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[9])

CUSA_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[10])

BigSky_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[11])

GreatMidwest_data <- CFB_Receiving_Data %>%
  filter(Conf == conf_list[12])

#forfor(i in 1:length(conf_list)){
#  michelle[i] <-  CFB_Receiving_Data %>%
#    filter(Conf == conf_list[i])
#}

#we now have the data separated into conference, and data stored independently
# so we want to run lm on the same stats as before to see if anything is
# significant within conferences.  Then we will go to excl to optimize
# coefficients for a linear model

SEC_summary <- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = SEC_data))

PAC12_summary <- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = pac12_data))

big10_summary <- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = big10_data))

 ACC_summary<- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = ACC_data))

BIG12_summary <- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = big12_data))

#not enough data here to make valid claims
Ind_summary <- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = Ind_data))

 American_summary<- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = American_data))

 
 
 #from here on, there is not enough data to make valid claims
 MAC_summary<- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = MAC_data))

 CUSA_summary<- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = CUSA_data))

BigSky_summary <- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = BigSky_data))

GreatMidwest_summary <- summary(lm(formula = `Fantasy Points` ~ Col_G + Col_Rec + 
                            Col_Rec_Yds + Col_Rec_Avg + Col_Rec_TD + 0, 
                          data = GreatMidwest_data))










