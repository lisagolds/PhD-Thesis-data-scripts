﻿* Encoding: UTF-8.

*#Step 1 - Missing Data
*Checking patterns of missingness
*Missing value analysis

MVA VARIABLES=Demo_1 ASQ_1 ASQ_4_1 ASQ_4_2 ASQ_4_3 ASQ_4_4 ASQ_4_5 ASQ_4_6 ASQ_4_7 ASQ_4_8 ASQ_4_9 
    ASQ_4_10 ASQ_4_11 ASQ_4_12 ASQ_4_13 ASQ_4_14 ASQ_4_15 ASQ_4_16 ASQ_4_17 ASQ_4_18 ASQ_4_19 ASQ_4_20 
    ASQ_4_21 ASQ_4_22 ASQ_4_23 DASS_1 DASS_2 DASS_3 DASS_4 DASS_5 DASS_6 DASS_7 DASS_8 DASS_9 DASS_10 
    DASS_11 DASS_12 DASS_13 DASS_14 DASS_15 DASS_16 DASS_17 DASS_18 DASS_19 DASS_20 DASS_21 WHO_1 WHO_2 
    WHO_3 WHO_4 WHO_5 ISEL_1 ISEL_2 ISEL_3 ISEL_4 ISEL_5 ISEL_6 ISEL_7 ISEL_8 ISEL_9 ISEL_10 ISEL_11 
    ISEL_12 TIPS_1 TIPS_2 TIPS_3 TIPS_4 TIPS_5 TIPS_6 TIPS_7 TIPS_8 TIPS_9 TIPS_10 TIPS_11 TIPS_12 
    TIPS_13 TIPS_14 MIRI_1 MIRI_2 MIRI_3 MIRI_4 MIRI_5 MIRI_6 MIRI_7 MIRI_8 MIRI_9 MIRI_10 
    MIRI_11 MIRI_12 MIRI_13 MIRI_14 MIRI_15 MIRI_16 MIRI_17 MIRI_18 MIRI_19 MIRI_20 MIRI_21 
    MIRI_22 Demo_2 Demo_3 Demo_4 Demo_5 Demo_6 Demo_7 Demo_8 Covid_1 Covid_2 Covid_3 Covid_4 ASQ_2 
    ASQ_3
  /MAXCAT=25
  /CATEGORICAL=Demo_2 Demo_3 Demo_4 Demo_5 Demo_6 Demo_7 Demo_8 Covid_1 Covid_2 Covid_3 Covid_4 
    ASQ_2 ASQ_3
  /TTEST NOPROB PERCENT=5
  /CROSSTAB PERCENT=5.


*Analyze Patterns of Missing Values.
MULTIPLE IMPUTATION  Demo_1 Demo_2 Demo_3 Demo_4 Demo_5 Demo_6 Demo_7 Demo_8 Covid_1 Covid_2 
    Covid_3 Covid_4 ASQ_1 ASQ_2 ASQ_3 ASQ_4_1 ASQ_4_1.1 ASQ_4_2 ASQ_4_2.1 ASQ_4_3 ASQ_4_3.1 ASQ_4_4 
    ASQ_4_4.1 ASQ_4_5 ASQ_4_5.1 ASQ_4_6 ASQ_4_6.1 ASQ_4_7 ASQ_4_7.1 ASQ_4_8 ASQ_4_8.1 ASQ_4_9 
    ASQ_4_9.1 ASQ_4_10 ASQ_4_10.1 ASQ_4_11 ASQ_4_11.1 ASQ_4_12 ASQ_4_12.1 ASQ_4_13 ASQ_4_13.1 
    ASQ_4_14 ASQ_4_14.1 ASQ_4_15 ASQ_4_15.1 ASQ_4_16 ASQ_4_16.1 ASQ_4_17 ASQ_4_17.1 ASQ_4_18 
    ASQ_4_18.1 ASQ_4_19 ASQ_4_19.1 ASQ_4_20 ASQ_4_20.1 ASQ_4_21 ASQ_4_21.1 ASQ_4_22 ASQ_4_22.1 
    ASQ_4_23 ASQ_4_23.1 DASS_1 DASS_2 DASS_3 DASS_4 DASS_5 DASS_6 DASS_7 DASS_8 DASS_9 DASS_10 DASS_11 
    DASS_12 DASS_13 DASS_14 DASS_15 DASS_16 DASS_17 DASS_18 DASS_19 DASS_20 DASS_21 WHO_1 WHO_2 WHO_3 
    WHO_4 WHO_5 ISEL_1 ISEL_2 ISEL_3 ISEL_4 ISEL_5 ISEL_6 ISEL_7 ISEL_8 ISEL_9 ISEL_10 ISEL_11 ISEL_12 
    TIPS_1 TIPS_2 TIPS_3 TIPS_4 TIPS_5 TIPS_6 TIPS_7 TIPS_8 TIPS_9 TIPS_10 TIPS_11 TIPS_12 TIPS_13 
    TIPS_14 MIRI_1 MIRI_2 MIRI_3 MIRI_4 MIRI_5 MIRI_6 MIRI_7 MIRI_8 MIRI_9 MIRI_10 MIRI_11 
    MIRI_12 MIRI_13 MIRI_14 MIRI_15 MIRI_16 MIRI_17 MIRI_18 MIRI_19 MIRI_20 MIRI_21 MIRI_22
   /IMPUTE METHOD=NONE
   /MISSINGSUMMARIES  PATTERNS.


*Descriptive statistics by group (complete vs. not complete)

SORT CASES  BY Complete_data.
SPLIT FILE LAYERED BY Complete_data.


FREQUENCIES VARIABLES=Demo_1 Demo_4 Demo_7 Demo_8Binary ASQ_1 ASQ_3
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

* Logistic regression by group (complete vs. not complete = DV)

LOGISTIC REGRESSION VARIABLES Complete_data
  /METHOD=ENTER Demo_1 Demo_4 Demo_7 Demo_8Binary ASQ_1 ASQ_3 
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).



*Little's MCAR test

MVA VARIABLES=Demo_1 ASQ_1 ASQ_4_1 ASQ_4_1.1 ASQ_4_2 ASQ_4_2.1 ASQ_4_3 ASQ_4_3.1 ASQ_4_4 
    ASQ_4_4.1 ASQ_4_5 ASQ_4_5.1 ASQ_4_6 ASQ_4_6.1 ASQ_4_7 ASQ_4_7.1 ASQ_4_8 ASQ_4_8.1 ASQ_4_9 
    ASQ_4_9.1 ASQ_4_10 ASQ_4_10.1 ASQ_4_11 ASQ_4_11.1 ASQ_4_12 ASQ_4_12.1 ASQ_4_13 ASQ_4_13.1 
    ASQ_4_14 ASQ_4_14.1 ASQ_4_15 ASQ_4_15.1 ASQ_4_16 ASQ_4_16.1 ASQ_4_17 ASQ_4_17.1 ASQ_4_18 
    ASQ_4_18.1 ASQ_4_19 ASQ_4_19.1 ASQ_4_20 ASQ_4_20.1 ASQ_4_21 ASQ_4_21.1 ASQ_4_22 ASQ_4_22.1 
    ASQ_4_23 ASQ_4_23.1 DASS_1 DASS_2 DASS_3 DASS_4 DASS_5 DASS_6 DASS_7 DASS_8 DASS_9 DASS_10 DASS_11 
    DASS_12 DASS_13 DASS_14 DASS_15 DASS_16 DASS_17 DASS_18 DASS_19 DASS_20 DASS_21 WHO_1 WHO_2 WHO_3 
    WHO_4 WHO_5 ISEL_1 ISEL_2 ISEL_3 ISEL_4 ISEL_5 ISEL_6 ISEL_7 ISEL_8 ISEL_9 ISEL_10 ISEL_11 ISEL_12 
    TIPS_1 TIPS_2 TIPS_3 TIPS_4 TIPS_5 TIPS_6 TIPS_7 TIPS_8 TIPS_9 TIPS_10 TIPS_11 TIPS_12 TIPS_13 
    TIPS_14 MIRI_1 MIRI_2 MIRI_3 MIRI_4 MIRI_5 MIRI_6 MIRI_7 MIRI_8 MIRI_9 MIRI_10 MIRI_11 
    MIRI_12 MIRI_13 MIRI_14 MIRI_15 MIRI_16 MIRI_17 MIRI_18 MIRI_19 MIRI_20 MIRI_21 MIRI_22 
    Demo_2 Demo_3 Demo_4 Demo_5 Demo_6 Demo_7 Demo_8 Covid_1 Covid_2 Covid_3 
    Covid_4 ASQ_2 ASQ_3 
  /MAXCAT=25
  /CATEGORICAL=Demo_2 Demo_3 Demo_4 Demo_5 Demo_6 Demo_7 Demo_8 Covid_1 Covid_2
    Covid_3 Covid_4 ASQ_2 ASQ_3 
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=50).


*#Step 2 - Data Imputation
*Set seed

SET RNG=MT MTINDEX=1984.



*recode ASQ for imputation

RECODE ASQ_4_3 ASQ_4_5 ASQ_4_6 ASQ_4_7 ASQ_4_9 ASQ_4_11 ASQ_4_12 ASQ_4_13 ASQ_4_14 ASQ_4_17 ASQ_4_18 ASQ_4_20 ASQ_4_21 ASQ_4_22 
    ASQ_4_23 (0=1) (5=2) (10=3) INTO ASQ_4_3i ASQ_4_5i ASQ_4_6i ASQ_4_7i ASQ_4_9i ASQ_4_11i ASQ_4_12i 
    ASQ_4_13i ASQ_4_14i ASQ_4_17i ASQ_4_18i ASQ_4_20i ASQ_4_21i ASQ_4_22i ASQ_4_23i.
EXECUTE.




DATASET ACTIVATE DataSet1.
*Impute Missing Data Values.
DATASET DECLARE df_2.
MULTIPLE IMPUTATION Demo_1 Demo_3 Covid_3 ASQ_4_3i ASQ_4_5i ASQ_4_6i ASQ_4_7i ASQ_4_9i ASQ_4_11i 
    ASQ_4_12i ASQ_4_13i ASQ_4_14i ASQ_4_17i ASQ_4_18i ASQ_4_20i ASQ_4_21i ASQ_4_22i ASQ_4_23i DASS_1 
    DASS_2 DASS_3 DASS_4 DASS_5 DASS_6 DASS_7 DASS_8 DASS_9 DASS_10 DASS_11 DASS_12 DASS_13 DASS_14 
    DASS_15 DASS_16 DASS_17 DASS_18 DASS_19 DASS_20 DASS_21 WHO_1 WHO_2 WHO_3 WHO_4 WHO_5 ISEL_1 ISEL_2 
    ISEL_3 ISEL_4 ISEL_5 ISEL_6 ISEL_7 ISEL_8 ISEL_9 ISEL_10 ISEL_11 ISEL_12 TIPS_1 TIPS_2 TIPS_3 
    TIPS_4 TIPS_5 TIPS_6 TIPS_7 TIPS_8 TIPS_9 TIPS_10 TIPS_11 TIPS_12 TIPS_13 TIPS_14 MIRI_1 MIRI_2 
    MIRI_3 MIRI_4 MIRI_5 MIRI_6 MIRI_7 MIRI_8 MIRI_9 MIRI_10 MIRI_11 MIRI_12 MIRI_13 MIRI_14 MIRI_15 
    MIRI_16 MIRI_17 MIRI_18 MIRI_19 MIRI_20 MIRI_21 MIRI_22 
  /IMPUTE METHOD=AUTO NIMPUTATIONS=10 MAXPCTMISSING=NONE MAXCASEDRAWS=10000 MAXPARAMDRAWS=10000 MAXMODELPARAM=100000
  /CONSTRAINTS Demo_1( MIN=16.0 MAX=45.0 RND=1.0 ROLE=DEP)
   /CONSTRAINTS Demo_3( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS Demo_5( MIN=0.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS Covid_3( MIN=0.0 MAX=1.0 RND=1.0)
  /CONSTRAINTS ASQ_4_3i( MIN=1.0 MAX=3.0 RND=1.0 ROLE=DEP)
  /CONSTRAINTS ASQ_4_5i (MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_6i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_7i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_9i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_11i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_12i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_13i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_14i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_17i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_18i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_20i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_21i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_22i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS ASQ_4_23i( MIN=1.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_1( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_2( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_3( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_4( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_5( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_6( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_7( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_8( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_9( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_10( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_11( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_12( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_13( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_14( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_15( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_16( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_17( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_18( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_19( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_20( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS DASS_21( MIN=0.0 MAX=3.0 RND=1.0)
  /CONSTRAINTS WHO_1( MIN=0.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS WHO_2( MIN=0.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS WHO_3( MIN=0.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS WHO_4( MIN=0.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS WHO_5( MIN=0.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS ISEL_1( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_2( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_3( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_4( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_5( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_6( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_7( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_8( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_9( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_10( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_11( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS ISEL_12( MIN=1.0 MAX=4.0 RND=1.0)
  /CONSTRAINTS TIPS_1( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_2( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_3( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_4( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_5( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_6( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_7( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_8( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_9( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_10( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_11( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_12( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_13( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS TIPS_14( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_1( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_2( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_3( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_4( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_5( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_6( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_7( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_8( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_9( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_10( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_11( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_12( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_13( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_14( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_15( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_16( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_17( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_18( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_19( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_20( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_21( MIN=1.0 MAX=5.0 RND=1.0)
  /CONSTRAINTS MIRI_22( MIN=1.0 MAX=5.0 RND=1.0)
  /MISSINGSUMMARIES NONE 
  /IMPUTATIONSUMMARIES MODELS 
  /OUTFILE IMPUTATIONS=df_2 .


*Comparing imputation groups

DATASET ACTIVATE df_2.
SORT CASES  BY Imputation_.
SPLIT FILE LAYERED BY Imputation_.


*recode ASQ

RECODE ASQ_4_3i ASQ_4_5i ASQ_4_6i ASQ_4_7i ASQ_4_9i ASQ_4_11i ASQ_4_12i ASQ_4_13i ASQ_4_14i 
    ASQ_4_17i ASQ_4_18i ASQ_4_20i ASQ_4_21i ASQ_4_22i ASQ_4_23i (1=0) (2=5) (3=10) INTO ASQ_4_3impute 
    ASQ_4_5impute ASQ_4_6impute ASQ_4_7impute ASQ_4_9impute ASQ_4_11impute ASQ_4_12impute 
    ASQ_4_13impute ASQ_4_14impute ASQ_4_17impute ASQ_4_18impute ASQ_4_20impute ASQ_4_21impute 
    ASQ_4_22impute ASQ_4_23impute.
EXECUTE.

   ***Scoring schemes***
   
COMPUTE ASQ=ASQ_4_1 + ASQ_4_1.1 + ASQ_4_2 + ASQ_4_2.1 + ASQ_4_3impute + ASQ_4_3.1 + ASQ_4_4 + 
    ASQ_4_4.1 + ASQ_4_5impute + ASQ_4_5.1 + ASQ_4_6impute + ASQ_4_6.1 + ASQ_4_7impute + ASQ_4_7.1 + 
    ASQ_4_8 + ASQ_4_8.1 + ASQ_4_9impute + ASQ_4_9.1 + ASQ_4_10 + ASQ_4_10.1 + ASQ_4_11impute + 
    ASQ_4_11.1 + ASQ_4_12impute + ASQ_4_12.1 + ASQ_4_13impute + ASQ_4_13.1 + ASQ_4_14impute + 
    ASQ_4_14.1 + ASQ_4_15 + ASQ_4_15.1 + ASQ_4_16 + ASQ_4_16.1 + ASQ_4_17impute + ASQ_4_17.1 + 
    ASQ_4_18impute + ASQ_4_18.1 + ASQ_4_19 + ASQ_4_19.1 + ASQ_4_20impute + ASQ_4_20.1 + ASQ_4_21impute 
    + ASQ_4_21.1 + ASQ_4_22impute + ASQ_4_22.1 + ASQ_4_23impute + ASQ_4_23.1.


COMPUTE DASS_total=DASS_1 + DASS_2 + DASS_3 + DASS_4 + DASS_5 + DASS_6 + DASS_7 + DASS_8 + DASS_9 + 
    DASS_10 + DASS_11 + DASS_12 + DASS_13 + DASS_14 + DASS_15 + DASS_16 + DASS_17 + DASS_18 + DASS_19
     + DASS_20 + DASS_21.
  EXECUTE.   

COMPUTE dass_d=DASS_3 + DASS_5 + DASS_10 + DASS_13 + DASS_16 + DASS_17 + DASS_21.
 EXECUTE.   

COMPUTE dass_a=DASS_2 + DASS_4 + DASS_7 + DASS_9 + DASS_15 + DASS_19 + DASS_20.
 EXECUTE.   

COMPUTE dass_s=DASS_1 + DASS_6 + DASS_8 + DASS_11 + DASS_12 + DASS_14 + DASS_18.
EXECUTE. 
     
COMPUTE WHO=WHO_1 + WHO_2 + WHO_3 + WHO_4 + WHO_5.
EXECUTE.   

COMPUTE ISEL_total=ISEL_1 + ISEL_2 + ISEL_3 + ISEL_4 + ISEL_5 + ISEL_6 + ISEL_7 + ISEL_8 + ISEL_9 + ISEL_10 + ISEL_11 + ISEL_12.
EXECUTE.   

COMPUTE ISEL_a=ISEL_2 + ISEL_4 + ISEL_6 + ISEL_11.
    EXECUTE. 
 
COMPUTE ISEL_b=ISEL_1 + ISEL_5 + ISEL_7 + ISEL_9.
    EXECUTE. 

COMPUTE ISEL_t=ISEL_3 + ISEL_8 + ISEL_10 + ISEL_12.
    EXECUTE. 
    
COMPUTE TIPS=TIPS_1 + TIPS_2 + TIPS_3 + TIPS_4 + TIPS_5 + TIPS_6 + TIPS_7 + TIPS_8 + TIPS_9 + TIPS_10 + TIPS_11 + TIPS_12 + 
    TIPS_13 + TIPS_14.
    EXECUTE. 

COMPUTE tips_active=TIPS_1 + TIPS_2 + TIPS_4.
    EXECUTE. 

COMPUTE tips_passive= TIPS_3 + TIPS_7 + TIPS_8 + TIPS_11 + TIPS_12 + TIPS_13.
    EXECUTE. 
    
COMPUTE tips_sleep=TIPS_6 + TIPS_10 + TIPS_14.
    EXECUTE. 

COMPUTE MIRI=MIRI_1 + MIRI_2 + MIRI_3 + MIRI_4 + MIRI_5 + MIRI_6 + MIRI_7 + MIRI_8 + MIRI_9 + MIRI_10 + MIRI_11 + MIRI_12 +
    MIRI_13 + MIRI_14 + MIRI_15 + MIRI_16 + MIRI_17 + MIRI_18 + MIRI_19 + MIRI_20 + MIRI_21 + MIRI_22.
    EXECUTE. 
 

*#Step 3 - Analysis

*Correlation analysis

CORRELATIONS
  /VARIABLES=MIRI ASQ dass_d dass_a dass_s WHO ISEL_a ISEL_b ISEL_t 
    tips_active tips_passive tips_sleep
  /PRINT=TWOTAIL NOSIG FULL
  /STATISTICS DESCRIPTIVES XPROD
  /MISSING=PAIRWISE.

*Hierarchical regression analysis

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MIRI
  /METHOD=ENTER Demo_1 ASQ_3 ASQ_2 Demo_4 ASQ dass_d dass_a dass_s WHO ISEL_a ISEL_b ISEL_t 
  /METHOD=ENTER tips_active tips_passive tips_sleep 
  /PARTIALPLOT ALL
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).
    

DATASET ACTIVATE df_2.

EXECUTE.