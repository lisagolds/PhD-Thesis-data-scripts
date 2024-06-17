
# 1-------------------------------------------------------------------------------

options(scipen = 999) #real pvalue

#Data Cleaning

library(readxl)
library(tidyverse)

# Inspecting data-------------------------------------------------------------------------------


summary(df)
str(df)
head(df)
head(df$Maternal_age)

# Transforming data-------------------------------------------------------------------------------


df_2 <- df %>% mutate(
                     SUBJID = as.factor(SUBJID),
                     Maternal_age = as.numeric(Maternal_age),
                     Ethnicity = as.factor(Ethnicity),
                     Ethnicity_mixed = as.factor(Ethnicity_mixed),
                     Ethnicity_other = as.factor(Ethnicity_other),
                     Education = as.factor(Education),
                     Education_other = as.factor(Education_other),
                     Employment = as.factor(Employment),
                     Employment_Other = as.factor(Employment_Other),
                     Maternity_leave = as.factor(Maternity_leave),
                     Household = as.factor(Household),
                     Other_children = as.factor(Other_children),
                     Child_sex = as.factor(Child_sex),
                     Phone_Use_1 = as.factor(Phone_Use_1),
                     Phone_Use_2 = as.factor(Phone_Use_2),
                     Phone_Use_3 = as.factor(Phone_Use_3),
                     Phone_Use_4 = as.factor(Phone_Use_4),
                     Phone_Use_5 = as.factor(Phone_Use_5),
                     Phone_Use_6 = as.factor(Phone_Use_6),
                     Phone_Use_7 = as.factor(Phone_Use_7),
                     Phone_Use_8 = as.factor(Phone_Use_8),
                     Phone_Use_8.Text = as.factor(Phone_Use_8.Text),
                     IBQ_1= recode(IBQ_1,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                                                            "About half the time" = 4, "More than half the time" = 5, 
                                                            "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_2= recode(IBQ_2,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_3= recode(IBQ_3,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_4= recode(IBQ_4,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_5= recode(IBQ_5,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_6= recode(IBQ_6,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_7= recode(IBQ_7,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_8= recode(IBQ_8,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_9= recode(IBQ_9,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_10= recode(IBQ_10,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_11= recode(IBQ_11,"Never" = 7, "Very rarely" = 6 , "Less than half the time" = 5, 
                "About half the time" = 4, "More than half the time" = 3, 
                "Almost always" = 2, "Always" = 1, "N/A" = 0),
  IBQ_12= recode(IBQ_12,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_13= recode(IBQ_13,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_14= recode(IBQ_14,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_15= recode(IBQ_15,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_16= recode(IBQ_16,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_17= recode(IBQ_17,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_18= recode(IBQ_18,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_19= recode(IBQ_19,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_20= recode(IBQ_20,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_21= recode(IBQ_21,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_22= recode(IBQ_22,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_23= recode(IBQ_23,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_24= recode(IBQ_24,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_25= recode(IBQ_25,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_26= recode(IBQ_26,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_27= recode(IBQ_27,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_28= recode(IBQ_28,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_29= recode(IBQ_29,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_30= recode(IBQ_30,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_31= recode(IBQ_31,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_32= recode(IBQ_32,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_33= recode(IBQ_33,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_34= recode(IBQ_34,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_35= recode(IBQ_35,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_36= recode(IBQ_36,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  IBQ_37= recode(IBQ_37,"Never" = 1, "Very rarely" = 2 , "Less than half the time" = 3, 
                "About half the time" = 4, "More than half the time" = 5, 
                "Almost always" = 6, "Always" = 7, "N/A" = 0),
  DASS_1= recode(DASS_1,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                "Almost always" = 3),
  DASS_2= recode(DASS_2,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_3= recode(DASS_3,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_4= recode(DASS_4,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_5= recode(DASS_5,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_6= recode(DASS_6,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_7= recode(DASS_7,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_8= recode(DASS_8,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_9= recode(DASS_9,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_10= recode(DASS_10,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_11= recode(DASS_11,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_12= recode(DASS_12,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_13= recode(DASS_13,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_14= recode(DASS_14,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_15= recode(DASS_15,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_16= recode(DASS_16,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_17= recode(DASS_17,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_18= recode(DASS_18,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_19= recode(DASS_19,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_20= recode(DASS_20,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  DASS_21= recode(DASS_21,"Never" = 0, "Sometimes" = 1, "Often" = 2, 
                 "Almost always" = 3),
  COPE_1= recode(COPE_1,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_2= recode(COPE_2,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_3= recode(COPE_3,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_4= recode(COPE_4,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_5= recode(COPE_5,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_6= recode(COPE_6,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_7= recode(COPE_7,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_8= recode(COPE_8,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_9= recode(COPE_9,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_10= recode(COPE_10,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_11= recode(COPE_11,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_12= recode(COPE_12,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_13= recode(COPE_13,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_14= recode(COPE_14,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_15= recode(COPE_15,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_16= recode(COPE_16,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_17= recode(COPE_17,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_18= recode(COPE_18,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_19= recode(COPE_19,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_20= recode(COPE_20,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_21= recode(COPE_21,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_22= recode(COPE_22,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_23= recode(COPE_23,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_24= recode(COPE_24,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_25= recode(COPE_25,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_26= recode(COPE_26,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_27= recode(COPE_27,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  COPE_28= recode(COPE_28,"I haven't been doing this at all" = 1, "A little bit" = 2, "A medium amount" = 3, 
                 "I've been doing this a lot" = 4),
  ISEL_1= recode(ISEL_1,"Definitely false" = 4, "Probably false" = 3, "Probably true" = 2, 
                 "Definitely true" = 1),
  ISEL_2= recode(ISEL_2,"Definitely false" = 4, "Probably false" = 3, "Probably true" = 2, 
                 "Definitely true" = 1),
  ISEL_3= recode(ISEL_3,"Definitely false" = 1, "Probably false" = 2, "Probably true" = 3, 
                 "Definitely true" = 4),
  ISEL_4= recode(ISEL_4,"Definitely false" = 1, "Probably false" = 2, "Probably true" = 3, 
                 "Definitely true" = 4),
  ISEL_5= recode(ISEL_5,"Definitely false" = 1, "Probably false" = 2, "Probably true" = 3, 
                 "Definitely true" = 4),
  ISEL_6= recode(ISEL_6,"Definitely false" = 1, "Probably false" = 2, "Probably true" = 3, 
                 "Definitely true" = 4),
  ISEL_7= recode(ISEL_7,"Definitely false" = 4, "Probably false" = 3, "Probably true" = 2, 
                 "Definitely true" = 1),
  ISEL_8= recode(ISEL_8,"Definitely false" = 4, "Probably false" = 3, "Probably true" = 2, 
                 "Definitely true" = 1),
  ISEL_9= recode(ISEL_9,"Definitely false" = 1, "Probably false" = 2, "Probably true" = 3, 
                 "Definitely true" = 4),
  ISEL_10= recode(ISEL_10,"Definitely false" = 1, "Probably false" = 2, "Probably true" = 3, 
                 "Definitely true" = 4),
  ISEL_11= recode(ISEL_11,"Definitely false" = 4, "Probably false" = 3, "Probably true" = 2, 
                 "Definitely true" = 1),
  ISEL_12= recode(ISEL_12,"Definitely false" = 4, "Probably false" = 3, "Probably true" = 2, 
                 "Definitely true" = 1),
  Phone_Beliefs_1= recode(Phone_Beliefs_1, "Strongly disagree" = 1, "Disagree" = 2, "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5),
  Phone_Beliefs_2= recode(Phone_Beliefs_2, "Strongly disagree" = 1, "Disagree" = 2, "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5),
  Phone_Beliefs_3= recode(Phone_Beliefs_3, "Strongly disagree" = 1, "Disagree" = 2, "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5),
  Phone_Beliefs_4= recode(Phone_Beliefs_4, "Strongly disagree" = 1, "Disagree" = 2, "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5),
  Phone_Beliefs_5= recode(Phone_Beliefs_5, "Strongly disagree" = 1, "Disagree" = 2, "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5),
  TIPS_1= recode(TIPS_1,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                          "Often" = 3, "Very often" = 4),
  TIPS_2= recode(TIPS_2,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_3= recode(TIPS_3,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_4= recode(TIPS_4,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_5= recode(TIPS_5,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_6= recode(TIPS_6,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_7= recode(TIPS_7,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_8= recode(TIPS_8,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_9= recode(TIPS_9,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_10= recode(TIPS_10,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_11= recode(TIPS_11,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_12= recode(TIPS_12,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_13= recode(TIPS_13,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4),
  TIPS_14= recode(TIPS_14,"Never" = 0, "Rarely" = 1, "Sometimes" = 2, 
                 "Often" = 3, "Very often" = 4)
  ) 

                   
     
summary(df_2)

# Missing data-------------------------------------------------------------------------------

library(naniar)

# How many missing values?
any_na(df_2)
n_miss(df_2)

# What data is missing?
df_2 %>% is.na() %>% colSums()

# Checking only numeric variables (omitting text responses from 'other' categories)
df_2_numeric <- df_2[, sapply(df_2, is.numeric)]
any_na(df_2_numeric)
n_miss(df_2_numeric)
df_2_numeric %>% is.na() %>% colSums()

# Number of NAs per variable
miss_var_summary(df_2_numeric)
miss_var_table(df_2_numeric)

# Number of NAs per participant
miss_case_summary(df_2_numeric)
miss_case_table(df_2_numeric)
              
# Where are NAs located?
vis_miss(df_2_numeric)

prop_miss(df_2_numeric)


# Data imputation (person median imputation)-------------------------------------------------------------------------------
                            
library(missMethods)

DASS_stress <- df_2 %>% dplyr::select(DASS_1 , DASS_6 , DASS_8 , DASS_11 , DASS_12 , DASS_14 , DASS_18)
stress_fix <- apply_imputation(DASS_stress, FUN = median, type = "rowwise")  

DASS_anxiety <- df_2 %>% dplyr::select(DASS_2 , DASS_4 , DASS_7, DASS_9 , DASS_15 , DASS_19 , DASS_20)
anxiety_fix <- apply_imputation(DASS_anxiety, FUN = median, type = "rowwise") 

DASS_depression <- df_2 %>% dplyr::select(DASS_3 , DASS_5 , DASS_10 , DASS_13 , DASS_16 , DASS_17 , DASS_21)
depression_fix <- apply_imputation(DASS_depression, FUN = median, type = "rowwise") 

IBQ_surgency <- df_2 %>% dplyr::select(IBQ_1 , IBQ_2 , IBQ_7 , IBQ_8 , IBQ_13 , IBQ_14 , IBQ_15 , IBQ_20 , IBQ_21 , 
                     IBQ_26 , IBQ_27 , IBQ_36 , IBQ_37)
surgency_fix <- apply_imputation(IBQ_surgency, FUN = median, type = "rowwise") 

IBQ_neg_affect <- df_2 %>% dplyr::select(IBQ_3 , IBQ_4 , IBQ_9 , IBQ_10 , IBQ_16 , IBQ_17 , IBQ_22 , IBQ_23 , IBQ_28 ,
  IBQ_29 , IBQ_32 , IBQ_33)
neg_affect_fix <- apply_imputation(IBQ_neg_affect, FUN = median, type = "rowwise") 

IBQ_effortful_control <- df_2 %>% dplyr::select(IBQ_5 , IBQ_6 , IBQ_11 , IBQ_12 , IBQ_18 , IBQ_19 , IBQ_24 , IBQ_25 , IBQ_30 , 
  IBQ_31 , IBQ_34 , IBQ_35)
effortful_fix <- apply_imputation(IBQ_effortful_control, FUN = median, type = "rowwise") 

COPE_approach <- df_2 %>% dplyr::select(COPE_2 , COPE_7 , COPE_5 , COPE_15 , COPE_10 , COPE_23 , COPE_12 , COPE_17 , COPE_14 ,
  COPE_25 , COPE_20 , COPE_24)
approach_fix <- apply_imputation(COPE_approach, FUN = median, type = "rowwise") 

COPE_avoidant <- df_2 %>% dplyr::select(COPE_1 , COPE_19 , COPE_3 , COPE_8 , COPE_4 , COPE_11 , COPE_5 , COPE_15 , COPE_6 ,
  COPE_16 , COPE_9 , COPE_21 , COPE_13 , COPE_26)
avoidant_fix <- apply_imputation(COPE_avoidant, FUN = median, type = "rowwise") 

ISEL_appraisal <- df_2 %>% dplyr::select(ISEL_2 , ISEL_4 , ISEL_6 , ISEL_11)
appraisal_fix <- apply_imputation(ISEL_appraisal, FUN = median, type = "rowwise") 

ISEL_belonging <- df_2 %>% dplyr::select(ISEL_1 , ISEL_5 , ISEL_7 , ISEL_9)
belonging_fix <- apply_imputation(ISEL_belonging, FUN = median, type = "rowwise") 

ISEL_tangible <- df_2 %>% dplyr::select(ISEL_3 , ISEL_8 , ISEL_10 , ISEL_12)
tangible_fix <- apply_imputation(ISEL_tangible, FUN = median, type = "rowwise") 

Phone_beliefs <- df_2 %>% dplyr::select(Phone_Beliefs_1 , Phone_Beliefs_2 , Phone_Beliefs_3 , Phone_Beliefs_4 , Phone_Beliefs_5)
beliefs_fix <- apply_imputation(Phone_beliefs, FUN = median, type = "rowwise") 

TIPS_total <- df_2 %>% dplyr::select(TIPS_1 , TIPS_2 , TIPS_3 , TIPS_4 , TIPS_5 , TIPS_6 , TIPS_7 , TIPS_8 , TIPS_9 , TIPS_10 , TIPS_11 , TIPS_12
, TIPS_13 , TIPS_14)
TIPS_fix <- apply_imputation(TIPS_total, FUN = median, type = "rowwise") 


# Scoring data-------------------------------------------------------------------------------

# Scoring schemes
df_3 <- df_2 %>% mutate(
  DASS_1 = stress_fix$DASS_1, DASS_6 = stress_fix$DASS_6, DASS_8 = stress_fix$DASS_8, DASS_11 = stress_fix$DASS_11, DASS_12 = stress_fix$DASS_12, DASS_14 = stress_fix$DASS_14, DASS_18 = stress_fix$DASS_18, #stress
  DASS_2 = anxiety_fix$DASS_2, DASS_4 = anxiety_fix$DASS_4, DASS_7 = anxiety_fix$DASS_7, DASS_9 = anxiety_fix$DASS_9, DASS_15 = anxiety_fix$DASS_15, DASS_19 = anxiety_fix$DASS_19, DASS_20 = anxiety_fix$DASS_20, #anxiety
  DASS_3 = depression_fix$DASS_3, DASS_5 = depression_fix$DASS_5, DASS_10 = depression_fix$DASS_10, DASS_13 = depression_fix$DASS_13, DASS_16 = depression_fix$DASS_16, DASS_17 = depression_fix$DASS_17, DASS_21 = depression_fix$DASS_21, #depression
  IBQ_1 = surgency_fix$IBQ_1, IBQ_2 = surgency_fix$IBQ_2, IBQ_7 = surgency_fix$IBQ_7, IBQ_8 = surgency_fix$IBQ_8, IBQ_13 = surgency_fix$IBQ_13, IBQ_14 = surgency_fix$IBQ_14, IBQ_15 = surgency_fix$IBQ_15, IBQ_20 = surgency_fix$IBQ_20, IBQ_21 = surgency_fix$IBQ_21, 
    IBQ_26 = surgency_fix$IBQ_26, IBQ_27 = surgency_fix$IBQ_27, IBQ_36 = surgency_fix$IBQ_36, IBQ_37 = surgency_fix$IBQ_37, #IBQ_surgency
  IBQ_3 = neg_affect_fix$IBQ_3, IBQ_4 = neg_affect_fix$IBQ_4, IBQ_9 = neg_affect_fix$IBQ_9, IBQ_10 = neg_affect_fix$IBQ_10, IBQ_16 = neg_affect_fix$IBQ_16, IBQ_17 = neg_affect_fix$IBQ_17, IBQ_22 = neg_affect_fix$IBQ_22, IBQ_23 = neg_affect_fix$IBQ_23, IBQ_28 = neg_affect_fix$IBQ_28, 
    IBQ_29 = neg_affect_fix$IBQ_29, IBQ_32 = neg_affect_fix$IBQ_32, IBQ_33 = neg_affect_fix$IBQ_33, #IBQ_neg_affect
  IBQ_5 = effortful_fix$IBQ_5, IBQ_6 = effortful_fix$IBQ_6, IBQ_11 = effortful_fix$IBQ_11, IBQ_12 = effortful_fix$IBQ_12, IBQ_18 = effortful_fix$IBQ_18, IBQ_19 = effortful_fix$IBQ_19, IBQ_24 = effortful_fix$IBQ_24, IBQ_25 = effortful_fix$IBQ_25, IBQ_30 = effortful_fix$IBQ_30, 
    IBQ_31 = effortful_fix$IBQ_31, IBQ_34 = effortful_fix$IBQ_34, IBQ_35 = effortful_fix$IBQ_35, #IBQ_effortful_control
  COPE_2 = approach_fix$COPE_2, COPE_7 = approach_fix$COPE_7, COPE_5 = approach_fix$COPE_5, COPE_15 = approach_fix$COPE_15, COPE_10 = approach_fix$COPE_10, COPE_23 = approach_fix$COPE_23, COPE_12 = approach_fix$COPE_12, COPE_17 = approach_fix$COPE_17, COPE_14 = approach_fix$COPE_14,
    COPE_25 = approach_fix$COPE_25, COPE_20 = approach_fix$COPE_20, COPE_24 = approach_fix$COPE_24, #COPE_approach
  COPE_1 = avoidant_fix$COPE_1, COPE_19 = avoidant_fix$COPE_19, COPE_3 = avoidant_fix$COPE_3, COPE_8 = avoidant_fix$COPE_8, COPE_4 = avoidant_fix$COPE_4, COPE_11 = avoidant_fix$COPE_11, COPE_5 = avoidant_fix$COPE_5, COPE_15 = avoidant_fix$COPE_15, COPE_6 = avoidant_fix$COPE_6,
    COPE_16 = avoidant_fix$COPE_16, COPE_9 = avoidant_fix$COPE_9, COPE_21 = avoidant_fix$COPE_21, COPE_13 = avoidant_fix$COPE_13, COPE_26 = avoidant_fix$COPE_26, #COPE_avoidant
  ISEL_2 = appraisal_fix$ISEL_2, ISEL_4 = appraisal_fix$ISEL_4, ISEL_6 = appraisal_fix$ISEL_6, ISEL_11 = appraisal_fix$ISEL_11, #ISEL_appraisal
  ISEL_1 = belonging_fix$ISEL_1, ISEL_5 = belonging_fix$ISEL_5, ISEL_7 = belonging_fix$ISEL_7, ISEL_9 = belonging_fix$ISEL_9, #ISEL_belonging
  ISEL_3 = tangible_fix$ISEL_3, ISEL_8 = tangible_fix$ISEL_8, ISEL_10 = tangible_fix$ISEL_10, ISEL_12 = tangible_fix$ISEL_12,
  Phone_Beliefs_1 = beliefs_fix$Phone_Beliefs_1, Phone_Beliefs_2 = beliefs_fix$Phone_Beliefs_2, Phone_Beliefs_3 = beliefs_fix$Phone_Beliefs_3, Phone_Beliefs_4 = beliefs_fix$Phone_Beliefs_4, Phone_Beliefs_5 = beliefs_fix$Phone_Beliefs_5,
  TIPS_1 = TIPS_fix$TIPS_1, TIPS_2 = TIPS_fix$TIPS_2, TIPS_3 = TIPS_fix$TIPS_3, TIPS_4 = TIPS_fix$TIPS_4, TIPS_5 = TIPS_fix$TIPS_5, TIPS_6 = TIPS_fix$TIPS_6, TIPS_7 = TIPS_fix$TIPS_7, TIPS_8 = TIPS_fix$TIPS_8, TIPS_9 = TIPS_fix$TIPS_9, TIPS_10 = TIPS_fix$TIPS_10, TIPS_11 = TIPS_fix$TIPS_11, TIPS_12 = TIPS_fix$TIPS_12,
  TIPS_13 = TIPS_fix$TIPS_13, TIPS_14 = TIPS_fix$TIPS_14
  )

df_4 <- df_3 %>% mutate(
  DASS_stress = DASS_1 + DASS_6 + DASS_8 + DASS_11 + DASS_12 + DASS_14 + DASS_18,
  DASS_anxiety = DASS_2 + DASS_4 + DASS_7 + DASS_9 + DASS_15 + DASS_19 + DASS_20,
  DASS_depression = DASS_3 + DASS_5 + DASS_10 + DASS_13 + DASS_16 + DASS_17 + DASS_21,
  DASS_total = DASS_stress + DASS_anxiety + DASS_depression,
  IBQ_surgency = sum(IBQ_1 + IBQ_2 + IBQ_7 + IBQ_8 + IBQ_13 + IBQ_14 + IBQ_15 + IBQ_20 + IBQ_21 + 
       IBQ_26 + IBQ_27 + IBQ_36 + IBQ_37),
  IBQ_neg_affect = IBQ_3 + IBQ_4 + IBQ_9 + IBQ_10 + IBQ_16 + IBQ_17 + IBQ_22 + IBQ_23 + IBQ_28 + 
  IBQ_29 + IBQ_32 + IBQ_33,
  IBQ_control = IBQ_5 + IBQ_6 + IBQ_11 + IBQ_12 + IBQ_18 + IBQ_19 + IBQ_24 + IBQ_25 + IBQ_30 + 
  IBQ_31 + IBQ_34 + IBQ_35,
  IBQ_total = IBQ_surgency + IBQ_neg_affect +  IBQ_effortful_control,
  COPE_problem_Dias = COPE_2 + COPE_7 + COPE_10 + COPE_12 + COPE_14 + COPE_17 + COPE_23 + COPE_25,
  COPE_avoidant_Dias = COPE_1 + COPE_3 + COPE_4 + COPE_6 + COPE_8 + COPE_11 + COPE_16 + COPE_19,
  COPE_emotion_Dias = COPE_5 + COPE_9 + COPE_13 + COPE_15 + COPE_18 + COPE_20 + COPE_21 + COPE_22 + COPE_24 + COPE_26 + COPE_27 + COPE_28,
  COPE_approach_Eis = COPE_2 + COPE_7 + COPE_5 + COPE_15 + COPE_10 + COPE_23 + COPE_12 + COPE_17 + COPE_14 +
  COPE_25 + COPE_20 + COPE_24,
  COPE_avoidant_Eis = COPE_1 + COPE_19 + COPE_3 + COPE_8 + COPE_4 + COPE_11 + COPE_5 + COPE_15 + COPE_6 +
  COPE_16 + COPE_9 + COPE_21 + COPE_13 + COPE_26,
  COPE_total = COPE_1 + COPE_2 + COPE_3 + COPE_4 + COPE_5 + COPE_6 + COPE_7 + COPE_8 + COPE_9 + COPE_10 + COPE_11 + COPE_12 + COPE_13 +
    COPE_14 + COPE_15 + COPE_16 + COPE_17 + COPE_18 + COPE_19 + COPE_20 + COPE_21 + COPE_22 + COPE_23 + COPE_24 + COPE_25 + COPE_26 +
    COPE_27 + COPE_28,
  ISEL_appraisal = ISEL_2 + ISEL_4 + ISEL_6 + ISEL_11,
  ISEL_belonging = ISEL_1 + ISEL_5 + ISEL_7 + ISEL_9,
  ISEL_tangible = ISEL_3 + ISEL_8 + ISEL_10 + ISEL_12,
  ISEL_total = ISEL_appraisal + ISEL_belonging + ISEL_tangible,
  Phone_beliefs = Phone_Beliefs_1 + Phone_Beliefs_2 + Phone_Beliefs_3 + Phone_Beliefs_4 + Phone_Beliefs_5,
  TIPS_total = TIPS_1 + TIPS_2 + TIPS_3 + TIPS_4 + TIPS_5 + TIPS_6 + TIPS_7 + TIPS_8 + TIPS_9 + TIPS_10 + TIPS_11 + TIPS_12
  + TIPS_13 + TIPS_14
)

df_4$IBQ_surgency <- rowMeans(df_3[, c('IBQ_1' , 'IBQ_2', 'IBQ_7' , 'IBQ_8', 'IBQ_13' , 'IBQ_14' , 'IBQ_15' , 'IBQ_20' , 'IBQ_21' , 
                                       'IBQ_26' , 'IBQ_27' , 'IBQ_36' , 'IBQ_37')]) 
df_4$IBQ_neg_affect <- rowMeans(df_3[, c('IBQ_3' , 'IBQ_4' , 'IBQ_9' , 'IBQ_10' , 'IBQ_16' , 'IBQ_17' , 'IBQ_22' , 'IBQ_23' , 'IBQ_28' , 
                                      'IBQ_29' , 'IBQ_32' , 'IBQ_33')])
df_4$IBQ_control <- rowMeans(df_3[, c('IBQ_5' ,'IBQ_6' , 'IBQ_11' , 'IBQ_12', 'IBQ_18' , 'IBQ_19' ,'IBQ_24' , 'IBQ_25' , 'IBQ_30' , 
                                        'IBQ_31' , 'IBQ_34' , 'IBQ_35')])
df_4$IBQ_total <- rowMeans(df_3[, c('IBQ_1' , 'IBQ_2', 'IBQ_7' , 'IBQ_8', 'IBQ_13' , 'IBQ_14' , 'IBQ_15' , 'IBQ_20' , 'IBQ_21' , 
                                       'IBQ_26' , 'IBQ_27' , 'IBQ_36' , 'IBQ_37', 'IBQ_3' , 'IBQ_4' , 'IBQ_9' , 'IBQ_10' , 'IBQ_16' , 'IBQ_17' , 'IBQ_22' , 'IBQ_23' , 'IBQ_28' , 
                                    'IBQ_29' , 'IBQ_32' , 'IBQ_33', 'IBQ_5' ,'IBQ_6' , 'IBQ_11' , 'IBQ_12', 'IBQ_18' , 'IBQ_19' ,'IBQ_24' , 'IBQ_25' , 'IBQ_30' , 
                                    'IBQ_31' , 'IBQ_34' , 'IBQ_35')])
                                        

# Standardised scores

df_4 <- df_4 %>% mutate(
  DASS_total_z = scale(DASS_total),
  DASS_d_z = scale(DASS_depression),
  DASS_a_z = scale(DASS_anxiety),
  DASS_s_z = scale(DASS_stress),
  IBQ_s_z = scale(IBQ_surgency),
  IBQ_n_z = scale(IBQ_neg_affect),
  IBQ_c_z = scale(IBQ_control),
  COPE_av_z = scale(COPE_avoidant_Eis),
  COPE_app_z = scale(COPE_approach_Eis),
  ISEL_a_z = scale(ISEL_appraisal),
  ISEL_b_z = scale(ISEL_belonging),
  ISEL_t_z = scale(ISEL_tangible),
  ISEL_z = scale(ISEL_total),
  Phone_bel_z = scale(Phone_beliefs),
  TIPS_z = scale(TIPS_total),
)


# Subset scoring (alphas)
library(ltm)
DASS_full <- df_4 %>% dplyr::select(DASS_1:DASS_21)
DASS_stress <- df_4 %>% dplyr::select(DASS_1, DASS_6, DASS_8, DASS_11, DASS_12, DASS_14, DASS_18)
DASS_anxiety <- df_4 %>% dplyr::select(DASS_2, DASS_4, DASS_7, DASS_9, DASS_15, DASS_19, DASS_20)
DASS_depression <- df_4 %>% dplyr::select(DASS_3, DASS_5, DASS_10, DASS_13, DASS_16, DASS_17, DASS_21)

IBQ_full <- df_4 %>% dplyr::select(IBQ_1:IBQ_37)
IBQ_surgency <- df_4 %>% dplyr::select(IBQ_1, IBQ_2, IBQ_7, IBQ_8, IBQ_13, IBQ_14, IBQ_15, IBQ_20, IBQ_21, 
                                            IBQ_26, IBQ_27, IBQ_36, IBQ_37)
IBQ_neg_affect <- df_4 %>% dplyr::select(IBQ_3, IBQ_4, IBQ_9, IBQ_10, IBQ_16, IBQ_17, IBQ_22, IBQ_23, IBQ_28, 
                                              IBQ_29, IBQ_32, IBQ_33) 
IBQ_control <- df_4 %>% dplyr::select(IBQ_5, IBQ_6, IBQ_11, IBQ_12, IBQ_18, IBQ_19, IBQ_24, IBQ_25, IBQ_30, 
                                                     IBQ_31, IBQ_34, IBQ_35)

COPE_full <- df_4 %>% dplyr::select(COPE_1:COPE_28)
COPE_problem <- df_4 %>% dplyr::select(COPE_2, COPE_7, COPE_10, COPE_12, COPE_14, COPE_17, COPE_23, COPE_25)
COPE_emotion <- df_4 %>% dplyr::select(COPE_5, COPE_9, COPE_13, COPE_15, COPE_18, COPE_20, COPE_21, COPE_22, COPE_24, COPE_26, COPE_27, COPE_28)                                                   
COPE_avoidant <- df_4 %>% dplyr::select(COPE_1, COPE_3, COPE_4, COPE_6, COPE_8, COPE_11, COPE_16, COPE_19)
COPE_approach_Eis <- df_4 %>% dplyr::select(COPE_2, COPE_7, COPE_5, COPE_15, COPE_10, COPE_23, COPE_12, COPE_17, COPE_14,
                                                 COPE_25, COPE_20, COPE_24)
COPE_avoidant_Eis <- df_4 %>% dplyr::select(COPE_1, COPE_19, COPE_3, COPE_8, COPE_4, COPE_11, COPE_5, COPE_15, COPE_6,
                                                 COPE_16, COPE_9, COPE_21, COPE_13, COPE_26)                                            

ISEL_full <- df_4 %>% dplyr::select(ISEL_1:ISEL_12)
ISEL_appraisal <- df_4 %>% dplyr::select(ISEL_2, ISEL_4, ISEL_6, ISEL_11)
ISEL_belonging <- df_4 %>% dplyr::select(ISEL_1, ISEL_5, ISEL_7, ISEL_9)
ISEL_tangible <- df_4 %>% dplyr::select(ISEL_3, ISEL_8, ISEL_10, ISEL_12)

Phone_beliefs <- df_4 %>% dplyr::select(Phone_Beliefs_1, Phone_Beliefs_2, Phone_Beliefs_3, Phone_Beliefs_4, Phone_Beliefs_5)

TIPS <- df_4 %>% dplyr::select(TIPS_1:TIPS_14)


# 2-----------------------------------------------------------------------------
# Data analysis-----------------------------------------------------------------

# Descriptives------------------------------------------------------------------
summary(df_4)
sd_child <- sd(df_4$Child_age)
sd_mum <- sd(df_4$Maternal_age)
sd_FP_neg <- sd(df_4$FP_neg)
sd_call_neg <- sd(df_4$RUcall_neg)
sd_text_neg <- sd(df_4$RUtext_neg)
sd_FP_disp <- sd(df_4$FP_dispersion)
sd_call_disp <- sd(df_4$RUcall_dispersion)
sd_text_disp <- sd(df_4$RUtext_dispersion)
sd_stress <- sd(df_4$DASS_stress)
sd_dep <-sd(df_4$DASS_depression)
sd_surg <- sd(df_4$IBQ_surgency)
sd_negaff <- sd(df_4$IBQ_neg_affect)
sd_control <- sd(df_4$IBQ_control)
sd_approach <- sd(df_4$COPE_approach_Eis)
sd_avoid <- sd(df_4$COPE_avoidant_Eis)
sd_appraisal <- sd(df_4$ISEL_appraisal)
sd_belong <- sd(df_4$ISEL_belonging)
sd_tang <- sd(df_4$ISEL_tangible)
sd_beliefs <- sd(df_4$Phone_beliefs)
sd_TIPS <- sd(df_4$TIPS_total)
sd_DASS <- sd(df_4$DASS_total)
sd_ISEL <- sd(df_4$ISEL_total)


# Cronbach's alphas-------------------------------------------------------------

library(ltm)
cronbach.alpha(DASS_full)
cronbach.alpha(DASS_stress)
cronbach.alpha(DASS_anxiety)
cronbach.alpha(DASS_depression)

cronbach.alpha(IBQ_full)
cronbach.alpha(IBQ_surgency)
cronbach.alpha(IBQ_neg_affect)
cronbach.alpha(IBQ_control)

cronbach.alpha(COPE_full)
cronbach.alpha(COPE_approach_Eis)
cronbach.alpha(COPE_avoidant_Eis)

cronbach.alpha(ISEL_full)
cronbach.alpha(ISEL_appraisal)
cronbach.alpha(ISEL_belonging)
cronbach.alpha(ISEL_tangible)

cronbach.alpha(Phone_beliefs)

cronbach.alpha(TIPS)


# Normality assumptions-------------------------------------------------------------------------------

set.seed(1984)
shapiro.test(df_4$FP_neg)

hist(df_4$FP_neg)
hist(df_4$RUcall_neg)
hist(df_4$RUtext_neg)



# Bivariate correlations-------------------------------------------------------------------------------

# Spearmans rank order - non parametric

selected_columns <- df_4[, c("DASS_a_z", "DASS_s_z", "DASS_d_z", "IBQ_s_z", "IBQ_n_z", "IBQ_c_z",
                             "COPE_app_z", "COPE_av_z", "ISEL_a_z", "ISEL_b_z",
                             "ISEL_t_z", "Phone_bel_z", "TIPS_z",
                             "FP_neg", "RUcall_neg", "RUtext_neg")]

cor_matrix <- cor(selected_columns, method = "spearman")

# customise
rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(selected_columns)
cor_matrix <- round(cor_matrix, 2)

print(cor_matrix)

library(correlation)

cor <- correlation(selected_columns, method = "spearman", ci = .90)

summary(cor, redundant = TRUE)

write_csv(selected_columns, "~/cor_data.csv")


# Tidying for LMM -------------------------------------------------------------------------------

# Omitting "other" columns 
df_5 <- df_4[, sapply(df_4, function(x) !any(is.na(x)))]

library(DescTools)

# Creating long data formats
long_disp <- df_5 %>% 
  pivot_longer(
    cols = c(FP_dispersion, RUcall_dispersion, RUtext_dispersion), 
    names_to = "condition",
    values_to = 'disp_score') %>%
  mutate(disp_prop = disp_score/2,
         disp_score_z =scale(disp_score))


long_neg <- df_5 %>% 
  pivot_longer(
    cols = c(FP_neg, RUcall_neg, RUtext_neg), 
    names_to = "condition",
    values_to = 'neg_score') %>%
  mutate(neg_prop = neg_score/2,
         neg_score_z =scale(neg_score))


# grouping by participant
long_disp$SUBJID <- as.factor(long_disp$SUBJID)
summary(long_disp$SUBJID)

long_neg$SUBJID <- as.factor(long_neg$SUBJID)
summary(long_neg$SUBJID)


# Log transform outcomes
hist(long_neg$neg_prop)  #- right skewed results 1.814967 (sqrt transform to 0.6567324)
hist(long_disp$disp_score)  #- moderate left skewed results -0.5846383 (leave as original)


library(moments)
skewness(long_disp$disp_score)
skewness(long_neg$neg_prop)

long_neg$sqrt_neg_prop <- sqrt(long_neg$neg_prop)
hist(long_neg$sqrt_neg_prop)
skewness(long_neg$sqrt_neg_prop)




# 3-----------------------------------------------------------------------------
# Linear mixed models-----------------------------------------------------------

library(Matrix)
library(lme4)
library(nlme)
library(dplyr)
library(sjPlot)
library(lmerTest)
library(optimx) #convergence issues
library(merTools)



# Grand mean centering (predictor variables)
long_disp$IBQ_centered <- long_disp$IBQ_n_z - mean(long_disp$IBQ_n_z)
long_disp$DASS_centered <- long_disp$DASS_total_z - mean(long_neg$DASS_total_z)

long_neg$IBQ_centered <- long_neg$IBQ_n_z - mean(long_neg$IBQ_n_z)
long_neg$DASS_centered <- long_neg$DASS_total_z - mean(long_neg$DASS_total_z)


# Sum to zero coding (Conditions- allows focused pairwise comparisons - needed to answer hypotheses)
library(contrast)

long_disp$condition <- as.factor(long_disp$condition)
levels(long_disp$condition)

long_neg$condition <- as.factor(long_neg$condition)
levels(long_neg$condition)

contrasts(long_disp$condition) <- contr.treatment(3, base = 1)  # Sum-to-zero contrasts: FP as the reference level
contrasts(long_neg$condition) <- contr.treatment(3, base = 1)  # Sum-to-zero contrasts: FP as the reference level

summary(long_disp$condition)



# fixed effect
# random intercept
# random intercept random effects

#Anova for best fit - which is the model that works best for interpretation


# (1) - Dispersion-------------------------------------------------------------------------------

# Step 1 - Fit the model--------------------------------------------------
long_disp$condition <- as.factor(long_disp$condition)
levels(long_disp$condition)


# model 1-----------------------------------------------------------------------
## mixed model - works & converged
model1_disp = lmerTest::lmer(disp_score ~ 1 + condition + (1|SUBJID), data=long_disp)
# dispersion predicted by condition accounting for individual differences within dyads
summary(model1_disp)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: disp_score ~ 1 + condition + (1 | SUBJID)
#    Data: long_disp
# 
# REML criterion at convergence: -131.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.8737 -0.6649  0.1779  0.7119  2.0098 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev.
#  SUBJID   (Intercept) 0.0006963 0.02639 
#  Residual             0.0195942 0.13998 
# Number of obs: 138, groups:  SUBJID, 46
# 
# Fixed effects:
#             Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   0.56233    0.02100 134.68277  26.774  < 2e-16 ***
# condition2    0.07967    0.02919  90.00000   2.730  0.00762 ** 
# condition3    0.12204    0.02919  90.00000   4.181 6.71e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) cndtnRUc_
# cndtnRUcll_ -0.695          
# cndtnRUtxt_ -0.695  0.500   

confint(model1_disp)

#                                 2.5 %    97.5 %
# .sig01                     0.00000000 0.0697314
# .sigma                     0.12062068 0.1588700
# (Intercept)                0.52132650 0.6033257
# conditionRUcall_dispersion 0.02249654 0.1368513
# conditionRUtext_dispersion 0.06486611 0.1792208



model1_disp

# Linear mixed model fit by REML ['lmerModLmerTest']
# Formula: disp_score ~ 1 + condition + (1 | SUBJID)
# Data: long_disp
# REML criterion at convergence: -131.7328
# Random effects:
#   Groups   Name        Std.Dev.
# SUBJID   (Intercept) 0.02639 
# Residual             0.13998 
# Number of obs: 138, groups:  SUBJID, 46
# Fixed Effects:
#   (Intercept)  conditionRUcall_dispersion  conditionRUtext_dispersion  
# 0.56233                     0.07967                     0.12204  



# Posthoc pairwise Tukey's HSD comparisons
library(lsmeans)

lsmeans_model_model1disp <- lsmeans(model1_disp, ~condition)

pairwise_comparisons_disp1 <- pairs(lsmeans_model_model1disp)

print(pairwise_comparisons_disp1)

# contrast                              estimate     SE df t.ratio p.value
# FP_dispersion - RUcall_dispersion      -0.0797 0.0292 90  -2.730  0.0206
# FP_dispersion - RUtext_dispersion      -0.1220 0.0292 90  -4.181  0.0002
# RUcall_dispersion - RUtext_dispersion  -0.0424 0.0292 90  -1.452  0.3191
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates 


# model 2-----------------------------------------------------------------------
## boundary (singular) fit: see help('isSingular')
model2_disp = lmerTest::lmer(disp_score ~ 1 + condition + (1 + IBQ_centered|SUBJID), 
                        data = long_disp,  
                        control = lmerControl(optimizer = "bobyqa"),
                        REML = FALSE)

#model run but boundary singular fit - model overfitted, unnecessarily complicated 
#- reduce complexity of random structure by removing the random slopes
#following *** suggestion reduced the model by removing random slopes
#still overfitting which suggests not enough data. 
#Reduced back to model 1


# model 3-----------------------------------------------------------------------
## boundary (singular) fit: see help('isSingular')
model3_disp = lmerTest::lmer(disp_score ~ 1 + condition + (1 + DASS_centered|SUBJID), 
                        data = long_disp,  
                        control = lmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 10000)),
                        REML = FALSE)


#model run but convergence / boundary singular fit - model overfitted, unnecessarily complicated 
#- reduce complexity of random structure by removing the random slopes
#following *** suggestion reduced the model by removing random slopes
#still overfitting which suggests not enough data. 
#Reduced back to model 1



# Step 2 - Visualise the model-------------------------------------------------------------------------------

## This plot is maybe not so useful? Ask Rhys what it showing?
plot_model(model1_disp, 
           type = "pred", #so it brings a prediction model
           terms = c("condition", "SUBJID"),
           pred.type = "re", # to plot random effects
           ci.lvl = NA 
)



# Plot individual participants
ggplot(long_disp,
       aes(x = condition, y = disp_score, color = SUBJID)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(aes(group = SUBJID))+ 
  labs(x = "Condition", y = "Dispersion rate") +  # Add labels to axes
  theme_bw() +
  theme(legend.position = "none") +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis


# Calculate the overall mean disp_score
mean_disp <- long_disp %>%
  group_by(condition) %>%
  summarise(mean_disp_score = mean(disp_score))

overall_mean_disp <- mean(long_disp$disp_score)


# Plot the mean disp_score for each condition
ggplot() +
  geom_point(data = long_disp, aes(x = condition, y = disp_score, color = SUBJID), alpha = 0.7) +  # Individual participant points
  geom_point(data = mean_disp, aes(x = condition, y = mean_disp_score), color = "blue", size = 3) +  # Mean points
  geom_line(data = mean_disp, aes(x = condition, y = mean_disp_score, group = 1), color = "blue", linewidth = 1) +  # Connect mean points with lines
  labs(x = "Condition", y = "Dispersion rate") +  # Add labels to axes
  theme_minimal() +  # Optional: use a minimal theme
  theme(legend.position = "none", panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis


# (2) - Negative affect-------------------------------------------------------------------------------

# Step 1 - Fit the model---------------------------------------------------------
long_neg$condition <- as.factor(long_neg$condition)
levels(long_neg$condition)


# model 1---------------------------------------------------------
## mixed model - works & converged
model1_neg = lmerTest::lmer(sqrt_neg_prop ~ 1 + condition + (1|SUBJID), data=long_neg)


summary(model1_neg)
ranef(model1_neg)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: sqrt_neg_prop ~ 1 + condition + (1 | SUBJID)
#    Data: long_neg
# 
# REML criterion at convergence: -149.8
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.78766 -0.58945  0.02296  0.48061  2.61555 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 0.008159 0.09033 
#  Residual             0.012303 0.11092 
# Number of obs: 138, groups:  SUBJID, 46
# 
# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   0.16727    0.02109 102.42998   7.931 2.82e-12 ***
# condition2    0.07389    0.02313  90.00000   3.195  0.00193 ** 
# condition3    0.23613    0.02313  90.00000  10.210  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#            (Intr) cndtn2
# condition2 -0.548       
# condition3 -0.548  0.500 

confint(model1_neg)


model1_neg

# Linear mixed model fit by REML ['lmerModLmerTest']
# Formula: sqrt_neg_prop ~ 1 + condition + (1 | SUBJID)
# Data: long_neg
# REML criterion at convergence: -149.8399
# Random effects:
#   Groups   Name        Std.Dev.
# SUBJID   (Intercept) 0.09033 
# Residual             0.11092 
# Number of obs: 138, groups:  SUBJID, 46
# Fixed Effects:
#   (Intercept)   condition2   condition3  
#       0.16727      0.07389      0.23613  



# Posthoc pairwise Tukey's HSD comparisons
library(lsmeans)

lsmeans_model_model1neg <- lsmeans(model1_neg, ~condition)

pairwise_comparisons_neg1 <- pairs(lsmeans_model_model1neg)

print(pairwise_comparisons_neg1)

# contrast                estimate     SE df t.ratio p.value
# FP_neg - RUcall_neg      -0.0739 0.0231 90  -3.195  0.0054
# FP_neg - RUtext_neg      -0.2361 0.0231 90 -10.210  <.0001
# RUcall_neg - RUtext_neg  -0.1622 0.0231 90  -7.015  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates 


# model 2-----------------------------------------------------------------------
model2_neg = lmerTest::lmer(sqrt_neg_prop ~ 1 + condition + (1 + IBQ_centered|SUBJID), 
                        data = long_neg,  
                        control = lmerControl(optimizer = "bobyqa"),
                        REML = FALSE)

summary(model2_neg)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: sqrt_neg_prop ~ 1 + condition + (1 + IBQ_centered | SUBJID)
#    Data: long_neg
# Control: lmerControl(optimizer = "bobyqa")
# 
#      AIC      BIC   logLik deviance df.resid 
#   -157.4   -136.9     85.7   -171.4      131 
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.86360 -0.65795  0.02398  0.57903  2.58826 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr 
#  SUBJID   (Intercept)  0.004033 0.06350       
#           IBQ_centered 0.004648 0.06818  -0.63
#  Residual              0.012036 0.10971       
# Number of obs: 138, groups:  SUBJID, 46
# 
# Fixed effects:
#             Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  0.16185    0.01986 98.96518   8.151 1.14e-12 ***
# condition2   0.07389    0.02288 92.00000   3.230  0.00172 ** 
# condition3   0.23613    0.02288 92.00000  10.322  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#            (Intr) cndtn2
# condition2 -0.576       
# condition3 -0.576  0.500

confint(model2_neg)

# 2.5 %    97.5 %
#   .sig01       0.03126005 0.1063723
# .sig02      -1.00000000 1.0000000
# .sig03       0.00000000 0.1160988
# .sigma               NA        NA
# (Intercept)  0.12251512 0.2014672
# condition2   0.02857879 0.1191929
# condition3   0.19082362 0.2814377

anova(model1_neg, model2_neg)

# Data: long_neg
# Models:
#   model1_neg: sqrt_neg_prop ~ 1 + condition + (1 | SUBJID)
# model2_neg: sqrt_neg_prop ~ 1 + condition + (1 + IBQ_n_z | SUBJID)
#            npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model1_neg    5 -157.94 -143.31 83.972  -167.94                     
# model2_neg    7 -157.39 -136.90 85.696  -171.39 3.4486  2     0.1783

##Can infer from this that while there is a small effect size for infant neg affect across conditions,
# the model fit is not significantly better for this sample and therefore not necessarily generalisable. 
# Use model 1 - May need a larger sample size to explore the effect of IBQ_n across conditions




# model 3-----------------------------------------------------------------------
model3neg = lmerTest::lmer(sqrt_neg_prop ~ 1 + condition + (1 + IBQ_centered + DASS_centered|SUBJID), 
                        data = long_neg,  
                        control = lmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 10000)),
                        REML = FALSE)


# sample size too small for number of observations
# not enough data. 


# Step 2 - Visualise the model-------------------------------------------------------------------------------

# plot_model(model1_neg, 
#            type = "pred", #so it brings a prediction model
#            terms = c("condition", "SUBJID"),
#            pred.type = "re", # to plot random effects
#            ci.lvl = NA 
# )


# Plot individual participants

ggplot(long_neg,
       aes(x = condition, y = sqrt_neg_prop, color = SUBJID)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(aes(group = SUBJID))+ 
  labs(x = "Condition", y = "Proportion of synchronous negative affect") +  # Add labels to axes
  theme_bw() +
  theme(legend.position = "none") +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis


# ggplot(long_neg,
#        aes(x = condition, y = sqrt_neg_prop, color = SUBJID)) +
#   geom_point(alpha = .7) +
#   geom_smooth(method = "lm", se = FALSE) +
#   geom_line(aes(group = SUBJID))+ 
#   facet_wrap(~condition) +
#   labs(x = "Condition", y = "Proportion of synchronous negative affect") +  # Add labels to axes
#   theme_bw() +
#   theme(legend.position = "none") +  # Remove legend
#   scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis




# Calculate the overall mean neg affect
mean_neg <- long_neg %>%
  group_by(condition) %>%
  summarise(mean_neg_score = mean(sqrt_neg_prop))

overall_mean_neg <- mean(long_neg$sqrt_neg_prop)

# Plot the mean neg affect for each condition
ggplot() +
  geom_point(data = long_neg, aes(x = condition, y = sqrt_neg_prop, color = SUBJID), alpha = 0.7) +  # Individual participant points
  geom_point(data = mean_neg, aes(x = condition, y = mean_neg_score), color = "blue", size = 3) +  # Mean points
  geom_line(data = mean_neg, aes(x = condition, y = mean_neg_score, group = 1), color = "blue", linewidth = 1) +  # Connect mean points with lines
  labs(x = "Condition", y = "Proportion of synchronous negative affect") +  # Add labels to axes
  theme_minimal() +  # Optional: use a minimal theme
  theme(legend.position = "none", panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis


# ggplot() +
#   geom_point(data = long_neg, aes(x = condition, y = sqrt_neg_prop, color = SUBJID), alpha = 0.7) +  # Individual participant points
#   geom_point(data = mean_neg, aes(x = condition, y = mean_neg_score), color = "blue", size = 3) +  # Mean points
#   geom_line(data = mean_neg, aes(x = condition, y = mean_neg_score, group = 1), color = "blue", linewidth = 1) +  # Connect mean points with lines
#   facet_wrap(~condition) +
#   labs(x = "Condition", y = "Proportion of synchronous negative affect") +  # Add labels to axes
#   theme_minimal() +  # Optional: use a minimal theme
#   theme(legend.position = "none", panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +  # Remove legend
#   scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis



## Looking at difference in random effects
p <- plot_model(model2_neg, 
                type = "pred", #so it brings a prediction model
                terms = c("IBQ_centered", "condition", "SUBJID" ),
                pred.type = "re", # to plot random effects
                ci.lvl = NA
)

# Modify axis labels
p + xlab("infant") + ylab("affect") + 
 scale_color_manual(values = c("red", "blue", "darkgreen"), labels = c("FP", "RU(Call)", "RU(text)")) +
  ggtitle("Synchronous negative affect across conditions (Infant negative affect as random effect")





# 4-------------------------------------------------------------------------------
# Outliers---------------------------------------------------------------

library(tidyverse)




# (1) - Dispersion------------------------------------------------------------------

# Step 1 - Fit the model----------------------------------------------------------
explore_disp <- long_disp %>% 
  group_by(SUBJID) %>%
  dplyr::filter(all(DASS_total_z <3, disp_score_z <3))

summary(explore_disp$SUBJID)


explore_disp$condition <- as.factor(explore_disp$condition)
levels(explore_disp$condition)




# model 1-----------------------------------------------------------------------
## mixed model - works & converged
model1_disp_ex = lmerTest::lmer(disp_score ~ 1 + condition + (1|SUBJID), data=explore_disp)


summary(model1_disp_ex)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: disp_score ~ 1 + condition + (1 | SUBJID)
#    Data: explore_disp
# 
# REML criterion at convergence: -127.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.8450 -0.6550  0.1824  0.7058  1.9985 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev.
#  SUBJID   (Intercept) 0.0004298 0.02073 
#  Residual             0.0200263 0.14151 
# Number of obs: 135, groups:  SUBJID, 45
# 
# Fixed effects:
#              Estimate Std. Error        df t value             Pr(>|t|)    
# (Intercept)   0.55933    0.02132 131.88358  26.234 < 0.0000000000000002 ***
# condition2    0.08073    0.02983  87.99985   2.706              0.00818 ** 
# condition3    0.12256    0.02983  87.99985   4.108            0.0000892 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#            (Intr) cndtn2
# condition2 -0.700       
# condition3 -0.700  0.500

model1_disp_ex  

# Linear mixed model fit by REML ['lmerModLmerTest']
# Formula: disp_score ~ 1 + condition + (1 | SUBJID)
# Data: explore_disp
# REML criterion at convergence: -127.4486
# Random effects:
#   Groups   Name        Std.Dev.
# SUBJID   (Intercept) 0.02073 
# Residual             0.14151 
# Number of obs: 135, groups:  SUBJID, 45
# Fixed Effects:
#   (Intercept)   condition2   condition3  
# 0.55933      0.08073      0.12256 

confint(model1_disp_ex)

#                  2.5 %     97.5 %
# .sig01      0.00000000 0.06787849
# .sigma      0.12173640 0.15997392
# (Intercept) 0.51771616 0.60095051
# condition2  0.02229058 0.13917609
# condition3  0.06411280 0.18099831




# Posthoc pairwise Tukey's HSD comparisons
library(lsmeans)

lsmeans_model_model1dispex <- lsmeans(model1_disp_ex, ~condition)

pairwise_comparisons_dispex <- pairs(lsmeans_model_model1dispex)

print(pairwise_comparisons_dispex)

# contrast                              estimate     SE df t.ratio p.value
# FP_dispersion - RUcall_dispersion      -0.0807 0.0298 88  -2.706  0.0221
# FP_dispersion - RUtext_dispersion      -0.1226 0.0298 88  -4.108  0.0003
# RUcall_dispersion - RUtext_dispersion  -0.0418 0.0298 88  -1.402  0.3444
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates 


# model 2 -----------------------------------------------------------------------
#boundary (singular) fit: see help('isSingular')
model2_disp_ex = lmerTest::lmer(disp_score ~ 1 + condition + (1 + IBQ_centered|SUBJID), 
                             data = explore_disp,  
                             control = lmerControl(optimizer = "bobyqa"),
                             REML = FALSE)

#model run but boundary singular fit - model overfitted, unnecessarily complicated 
#- reduce complexity of random structure by removing the random slopes
#following *** suggestion reduced the model by removing random slopes
#still overfitting which suggests not enough data. 
#Reduced back to model 1



#model 3
#boundary (singular) fit: see help('isSingular')
model3_disp_ex = lmerTest::lmer(disp_score ~ 1 + condition + (1 + DASS_centered|SUBJID), 
                             data = long_disp,  
                             control = lmerControl(optimizer = "bobyqa", 
                                                   optCtrl = list(maxfun = 10000)),
                             REML = FALSE)


#model run but boundary singular fit - model overfitted, unnecessarily complicated 
#- reduce complexity of random structure by removing the random slopes
#following *** suggestion reduced the model by removing random slopes
#still overfitting which suggests not enough data. 
#Reduced back to model 1


# Step 2 - Visualise the model-------------------------------------------------------------------------------

## This plot is maybe not so useful? Ask Rhys what it showing?
plot_model(model1_disp_ex, 
           type = "pred", #so it brings a prediction model
           terms = c("condition", "SUBJID"),
           pred.type = "re", # to plot random effects
           ci.lvl = NA 
)



# Plot individual participants
ggplot(explore_disp,
       aes(x = condition, y = disp_score, color = SUBJID)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(aes(group = SUBJID))+ 
  labs(x = "Condition", y = "Mean dispersion rate") +  # Add labels to axes
  theme(legend.position = "none") +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis


# Calculate the overall mean disp_score
mean_disp <- explore_disp %>%
  group_by(condition) %>%
  summarise(mean_disp_score = mean(disp_score))

overall_mean_disp <- mean(explore_disp$disp_score)


# Plot the mean disp_score for each condition
ggplot() +
  geom_point(data = explore_disp, aes(x = condition, y = disp_score, color = SUBJID), alpha = 0.7) +  # Individual participant points
  geom_point(data = mean_disp, aes(x = condition, y = mean_disp_score), color = "blue", size = 3) +  # Mean points
  geom_line(data = mean_disp, aes(x = condition, y = mean_disp_score, group = 1), color = "blue", linewidth = 1) +  # Connect mean points with lines
  labs(x = "Condition", y = "Mean dispersion rate") +  # Add labels to axes
  theme_minimal() +  # Optional: use a minimal theme
  theme(legend.position = "none") +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis




# (1) - Negative affect------------------------------------------------------------------

# Step 1 - Fit the model----------------------------------------------------------

explore_neg <- long_neg%>% mutate(
  sqrt_neg_prop_z = scale(sqrt_neg_prop)
)

explore_neg <- explore_neg %>% 
  group_by(SUBJID) %>%
  dplyr::filter(all(DASS_total_z <3, sqrt_neg_prop_z <3))


summary(explore_neg$SUBJID)


explore_disp$condition <- as.factor(explore_disp$condition)
levels(explore_disp$condition)


# model 1---------------------------------------------------------
## mixed model - works & converged
model1_neg_ex = lmerTest::lmer(sqrt_neg_prop ~ 1 + condition + (1|SUBJID), data= explore_neg)


summary(model1_neg_ex)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: sqrt_neg_prop ~ 1 + condition + (1 | SUBJID)
#    Data: explore_neg
# 
# REML criterion at convergence: -159.1
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.72862 -0.63909  0.05773  0.54461  2.87311 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  SUBJID   (Intercept) 0.004933 0.07023 
#  Residual             0.011937 0.10925 
# Number of obs: 132, groups:  SUBJID, 44
# 
# Fixed effects:
#              Estimate Std. Error        df t value             Pr(>|t|)    
# (Intercept)   0.15250    0.01958 110.16127   7.788     0.00000000000402 ***
# condition2    0.07422    0.02329  86.00000   3.186              0.00201 ** 
# condition3    0.24246    0.02329  86.00000  10.409 < 0.0000000000000002 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#            (Intr) cndtn2
# condition2 -0.595       
# condition3 -0.595  0.500

model1_neg_ex

# Linear mixed model fit by REML ['lmerModLmerTest']
# Formula: sqrt_neg_prop ~ 1 + condition + (1 | SUBJID)
# Data: explore_neg
# REML criterion at convergence: -159.1179
# Random effects:
#   Groups   Name        Std.Dev.
# SUBJID   (Intercept) 0.07023 
# Residual             0.10925 
# Number of obs: 132, groups:  SUBJID, 44
# Fixed Effects:
#   (Intercept)   condition2   condition3  
# 0.15250      0.07422      0.24246  

confint(model1_neg_ex)
#                 2.5 %     97.5 %
# .sig01      0.04081307 0.09925645
# .sigma      0.09382044 0.12616304
# (Intercept) 0.11423443 0.19076696
# condition2  0.02859093 0.11984925
# condition3  0.19683417 0.28809250



# Posthoc pairwise Tukey's HSD comparisons
library(lsmeans)

lsmeans_model_model1neg_ex <- lsmeans(model1_neg_ex, ~condition)

pairwise_comparisons_neg1_ex <- pairs(lsmeans_model_model1neg_ex)

print(pairwise_comparisons_neg1_ex)

# contrast                estimate     SE df t.ratio p.value
# FP_neg - RUcall_neg      -0.0742 0.0233 86  -3.186  0.0056
# FP_neg - RUtext_neg      -0.2425 0.0233 86 -10.409  <.0001
# RUcall_neg - RUtext_neg  -0.1682 0.0233 86  -7.223  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates  


# model 2-----------------------------------------------------------------------
model2_neg_ex = lmerTest::lmer(sqrt_neg_prop ~ 1 + condition + (1 + IBQ_centered|SUBJID), 
                        data = explore_neg,  
                        control = lmerControl(optimizer = "bobyqa"),
                        REML = FALSE)

summary(model2_neg_ex)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: sqrt_neg_prop ~ 1 + condition + (1 + IBQ_centered | SUBJID)
#    Data: explore_neg
# Control: lmerControl(optimizer = "bobyqa")
# 
#      AIC      BIC   logLik deviance df.resid 
#   -163.6   -143.4     88.8   -177.6      125 
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.75178 -0.65352  0.07755  0.55735  2.92071 
# 
# Random effects:
#  Groups   Name         Variance  Std.Dev. Corr 
#  SUBJID   (Intercept)  0.0043046 0.06561       
#           IBQ_centered 0.0005413 0.02327  -0.02
#  Residual              0.0116653 0.10801       
# Number of obs: 132, groups:  SUBJID, 44
# 
# Fixed effects:
#              Estimate Std. Error        df t value             Pr(>|t|)    
# (Intercept)   0.15248    0.01934 112.46224   7.886      0.0000000000022 ***
# condition2    0.07422    0.02303  88.00000   3.223              0.00178 ** 
# condition3    0.24246    0.02303  88.00000  10.530 < 0.0000000000000002 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#            (Intr) cndtn2
# condition2 -0.595       
# condition3 -0.595  0.500

anova(model1_neg_ex, model2_neg_ex)

# Data: explore_neg
# Models:
# model1_neg_ex: sqrt_neg_prop ~ 1 + condition + (1 | SUBJID)
# model2_neg_ex: sqrt_neg_prop ~ 1 + condition + (1 + IBQ_centered | SUBJID)
#               npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model1_neg_ex    5 -167.47 -153.06 88.735  -177.47                     
# model2_neg_ex    7 -163.56 -143.38 88.780  -177.56 0.0899  2      0.956

##Can infer from this that while there is a small effect size for infant neg affect across conditions,
# the model fit is not significantly better for this sample and therefore not necessarily generalisable. 
# Use model 1 - May need a larger sample size to explore the effect of IBQ_n across conditions


#model 2a
model2a_neg_ex = lmerTest::lmer(sqrt_neg_prop ~ 1 + condition + (1 + DASS_centered|SUBJID), 
                            data = explore_neg,  
                            control = lmerControl(optimizer = "bobyqa"),
                            REML = FALSE)

## Boundary singular fit



# model 3-----------------------------------------------------------------------
model3_neg_ex = lmerTest::lmer(sqrt_neg_prop ~ 1 + condition + (1 + IBQ_centered + DASS_centered|SUBJID), 
                        data = explore_neg,  
                        control = lmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 10000)),
                        REML = FALSE)


# sample size too small for number of observations
# not enough data. 


# Step 2 - Visualise the model-------------------------------------------------------------------------------

##Can these plots be useful in showing the differences between random effects?
##Ask Rhys
plot_model(model1_neg_ex, 
           type = "pred", #so it brings a prediction model
           terms = c("condition", "SUBJID"),
           pred.type = "re", # to plot random effects
           ci.lvl = NA 
)


plot_model(model2_neg_ex, 
           type = "pred", #so it brings a prediction model
           terms = c("condition", "SUBJID", "IBQ"),
           pred.type = "re", # to plot random effects
           ci.lvl = NA 
)


# Plot individual participants

ggplot(explore_neg,
       aes(x = condition, y = sqrt_neg_prop, color = SUBJID)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(aes(group = SUBJID))+ 
  labs(x = "Condition", y = "Proportion of synchronous negative affect") +  # Add labels to axes
  theme(legend.position = "none") +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis

# Calculate the overall mean neg affect
mean_neg_ex <- explore_neg %>%
  group_by(condition) %>%
  summarise(mean_neg_score = mean(sqrt_neg_prop))

overall_mean_neg_ex <- mean(explore_neg$sqrt_neg_prop)

# Plot the mean neg affect for each condition
ggplot() +
  geom_point(data = explore_neg, aes(x = condition, y = sqrt_neg_prop, color = SUBJID), alpha = 0.7) +  # Individual participant points
  geom_point(data = mean_neg_ex, aes(x = condition, y = mean_neg_score), color = "blue", size = 3) +  # Mean points
  geom_line(data = mean_neg_ex, aes(x = condition, y = mean_neg_score, group = 1), color = "blue", linewidth = 1) +  # Connect mean points with lines
  labs(x = "Condition", y = "Proportion of synchronous negative affect") +  # Add labels to axes
  theme_minimal() +  # Optional: use a minimal theme
  theme(legend.position = "none") +  # Remove legend
  scale_x_discrete(labels = c("Free Play", "RU(call)", "RU(text)"))  # Rename conditions on the x-axis





# 5 -----------------------------------------------------------------------------
# Adding covariates as fixed effects--------------------------------------------------------------------------------
 
modeltest_disp = lmerTest::lmer(disp_score ~ 1 + condition + Maternal_age + Child_sex + Education + Other_children +
                                  (1|SUBJID), data=long_disp)



modeltest_neg = lmerTest::lmer(neg_prop ~ 1 + condition + Maternal_age + Child_sex + Education + Other_children +
                                 (1 + IBQ_n_z|SUBJID), data=long_neg)



summary(modeltest_disp)

summary(modeltest_neg)





# Appendix----------------------------------------------------------------------
# Sensitivity test for ANOVA results - Repeated measures ANOVA------------------


#Friedman and post-hoc Nemenyi (non parametric version of repeated measures ANOVA)

#1 - Negative affect
friedman_result <- friedman.test(cbind(df_5$FP_neg, df_5$RUcall_neg, df_5$RUtext_neg), data = df_5)
print(friedman_result)


#Friedman chi-squared = 57.916, df = 2, p-value = 0.0000000000002653

NemenyiTest(neg_score ~ condition, data = long_neg)

#                        mean.rank.diff        pval    
#RUcall_neg-FP_neg            21.40000       0.02561 *   
#RUtext_neg-FP_neg            54.36667 0.00000000013 ***
#RUtext_neg-RUcall_neg        32.96667       0.00019 ***



#2 - Dispersion

friedman_result <- friedman.test(cbind(df_5$FP_dispersion, df_5$RUcall_dispersion, df_5$RUtext_dispersion), data = df_5)
print(friedman_result)

#Friedman chi-squared = 18.978, df = 2, p-value = 0.00007569

NemenyiTest(disp_score ~ condition, data = long_disp)

#                                       mean.rank.diff   pval    
#RUcall_dispersion-FP_dispersion              20.21111 0.03789 *  
#RUtext_dispersion-FP_dispersion              32.38889 0.00025 *** 
#RUtext_dispersion-RUcall_dispersion          12.17778 0.30211  



#-------------------------------------------------------------------------------

library(writexl)
output_path <- "C:\\Users\\lisag\\OneDrive - University of Edinburgh\\Observations\\Data Analysis\\df_5.xlsx"
write_xlsx(df_5, path = output_path)

output_path1 <- "C:\\Users\\lisag\\OneDrive - University of Edinburgh\\Observations\\Data Analysis\\df_2.xlsx"
write_xlsx(df_2_numeric, path = output_path1)

output_path2 <- "C:\\Users\\lisag\\OneDrive - University of Edinburgh\\Observations\\Data Analysis\\df_explore.xlsx"
write_xlsx(explore_neg, path = output_path2)

