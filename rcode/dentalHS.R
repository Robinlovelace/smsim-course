# loading and scrambling the Dental Health survey data

library(sjPlot)
adhs <- sji.SPSS("/scratch/data/adult_dental_health_survey_2009_end_user_licence_270712.sav")
sji.viewSPSS(adhs)
adhs <- adhs[ adhs$SHA == 3, ] # subset to Yorkshire and the Humber
adhs <- adhs[c("NCakes", # how often eats cakes? 
#                -7 Refused/not obtained
#                1 6 or more times a week
#                2 3-5 times a week
#                3 1-2 times a week
#                4 Less than once a week
#                5 Rarely or never   
               "Car", # owns a car? 1 = yes, 2 = no
               "Sex", # gender: male = 1
               "NSSEC8", # social class:
#                 2 Lower managerial and professional occupations
#                  3 Intermediate occupations
#                  4 Small employers and own account workers
#                  5 Lower supervisory & technical occupations
#                  6 Semi-routine Occupations
#                  7 Routine occupations
#                  8 Never worked and long term unemployed
#                  97 Not classified   
               "ageband4")] # grouped age - 7 cats  16 to 24, 25 to 34, 35 to 44, 45 to 54, 55 to 64, 65 to 74, 75 and over
unique(adhs$NSSEC8)


# Re-code to categorical


