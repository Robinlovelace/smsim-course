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

               "NSSEC8", # social class: 1.1, 1.2 (higher managerial)
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
sapply(adhs, class) # note the variables are all numeric
head(adhs)

# Re-code to categorical
library(car) # package for recode function
ind <- adhs
ind$NCakes <- recode(ind$NCakes, "1='6+';
2='3-5'; 3='1-2'; 4='<1'; 5='rarely'", as.factor.result = T)
head(ind$NCakes)

ind <- ind[-which(ind$ageband4 == 7),]

head(ind$ageband4)
ind$ageband4 <- recode(ind$ageband4, "1='16-24'; 2='25-34'; 3='35-44';
                       4='45-54'; 5='55-64'; 6='65-74'; 7='65-74'", as.factor.result = T)
summary(ind$ageband4)

ind$NSSEC8 <- as.factor(ind$NSSEC8)
summary(ind$NSSEC8)

ind$Car <- as.factor(ind$Car)
summary(ind$Car) # no car is 2

write.csv(ind, "data/cakeMap/ind.csv", row.names = F)

