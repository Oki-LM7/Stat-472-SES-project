library(tidyverse)
library(dplyr)

dat_before <- read.csv("maalongdatafile_ANON.csv")
dat_after <- read.csv("CSPCC_Data_FUSonly (1).csv")

dat_total <- merge(dat_before, dat_after, by.x = "studid", by.y = "StudentID")

#spr26calc2  - begining, do you plan to take calc 2? #Old Q26
#sq5initialc2  - end, at the begining of the semester did you intend to take calc 2 #Old Q5Post
#sqr26calc2  - Old Q3Post end, intend to take calc 2
#sp43fatheduc - fathers education
#sp44motheduc - mother education
#sp45zip - hightschool zipcode
#sp54homesup - To what degree was your home environment supportive of your studying math
#sp57work = Approximately how many hours per week do you expect to work at a job this semester/term
#sp57workl = Approximately how many hours per week do you expect to work at a job this semester/term? 
#sp59prep = Approximately how many hours per week do you expect to spend preparing for your classes this semester/term (studying, reading, writing, doing homework or lab work, analyzing data, rehearsing, or other academic activities)? 
#sp59prepl = Approximately how many hours per week do you expect to spend preparing for your classes this semester/term (studying, reading, writing, doing homework or lab work, analyzing data, rehearsing, or other academic activities)? 
#sp60career = Which of the following BEST describes your current career goal
#sp61pay = I am anticipating difﬁculty paying for college 
#sp61prevcalc = In order to succeed in calculus at a college or university, I must have taken it before 
#sqr25grade = What grade do you expect (or did you receive) in this calculus course?
#sq4 - If you are not intending to take Calculus II, check all reasons that apply
#sqr29conﬁdent = I am conﬁdent in my mathematics abilities 
#sqr29enjoy = I enjoy doing mathematics  
#sqr36choice = If I had a choice would you take make again
#sq39job = Approximately how many hours per week did you work at a job this semester/term
#sq39jobl = Approximately how many hours per week did you work at a job this semester/term
#sq41prepareall = Approximately how many hours per week did you spend preparing for all classes (studying, reading, writing, doing homework or lab work, analyzing data, or other academic activities) this semester/ term? 
#sq41preparealll = Approximately how many hours per week did you spend preparing for all classes (studying, reading, writing, doing homework or lab work, analyzing data, or other academic activities) this semester/ term?  
#sq48grade = Student grade. Actual grades provided by departments based on student ID match
#sq48gradel = Student grade. Actual grades provided by departments based on student ID match
#Q47 seris - Do the following people see you as a person who is good at mathematics
#sp41weak = If I am unable to solve a problem within a few minutes, it is an indication of my weakness in mathematics


##Can't find code for Q55 or sp45zip

dat_use <- dat_total %>%
  select(spr26calc2 , sq5initialc2, Q3FUS_Yes, Q3FUS_No, sqr26calc2,
         sp43fatheduc, sp44motheduc, sp54homesup,
         sp57work, sp57workl, sq39job, sq39jobl, 
         sp59prep, sp59prepl,sq41prepareall, sq41preparealll, 
         sp60career, sp61pay, sp61prevcalc, sqr25grade, sq48grade, sq48gradel,
         sqr29enjoy, sqr36choice, 
         sp47mpersonyou, sp47mpersonparent, sp47mpersonteach, sp47mpersonfriend,
         sp41weak 
  ) %>%
  dplyr::rename(Q26 = spr26calc2, Q5Post = sq5initialc2, Q3Post =sqr26calc2 ) %>%
  mutate(., switcher_group =
           case_when(
             Q26 == "Yes" & Q3FUS_No == "No" ~ 1,
             Q26 == "Dont know/ Not sure/ N/A" & Q3FUS_No == "No" ~ 2,
             is.na(Q26) & Q5Post == "Yes" & Q3FUS_No == "No" ~ 3,
             is.na(Q26) & Q5Post == "Dont know/ Not sure/ N/A" & Q3FUS_No == "No" ~ 4,
             Q26 == "Yes" & Q5Post == "Yes" & Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 5,
             Q26 == "Yes" & Q3Post == "No" & Q3FUS_Yes == " " &
               Q3FUS_No == " " ~ 6,
             Q26 == "Dont know/ Not sure/ N/A" & Q5Post == "Yes" &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 7,
             Q26 == "Dont know/ Not sure/ N/A" &  Q3Post == "No" & Q3FUS_Yes == " " &
               Q3FUS_No == " " ~ 8,
             is.na(Q26) & Q5Post == "Yes" &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 9,
             is.na(Q26) & Q5Post == "Yes" &  Q3Post == "No" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 10,
             is.na(Q26) & Q5Post == "Dont know/ Not sure/ N/A" &  Q3Post == "No" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 11,
             Q26 == "Yes" & Q3FUS_Yes == "Yes" ~ 12,
             Q26 == "Dont know/ Not sure/ N/A" & Q3FUS_Yes == "Yes" ~ 13,
             is.na(Q26) & Q5Post == "Yes" & Q3FUS_Yes == "Yes" ~ 14,
             is.na(Q26) & Q5Post == "Dont know/ Not sure/ N/A" & Q3FUS_Yes == "Yes" ~ 15,
             Q26 == "Yes" &  Q3Post == "Yes" & Q3FUS_Yes == " " &
               Q3FUS_No == " " ~ 16,
             Q26 == "Yes" & Q5Post == "Dont know/ Not sure/ N/A" &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 17,
             Q26 == "Yes" & Q5Post == "No" &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 18,
             Q26 == "Yes" & is.na(Q5Post) &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 19,
             Q26 == "Dont know/ Not sure/ N/A" &  Q3Post == "Yes" & Q3FUS_Yes == " " &
               Q3FUS_No == " " ~ 20,
             Q26 == "Dont know/ Not sure/ N/A" & Q5Post == "Dont know/ Not sure/ N/A" &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 21,
             Q26 == "Dont know/ Not sure/ N/A" & Q5Post == "No" &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 22,
             Q26 == "Dont know/ Not sure/ N/A" & is.na(Q5Post) & Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 23,
             is.na(Q26) & Q5Post == "Yes" &  Q3Post == "Yes" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 24,
             is.na(Q26) & Q5Post == "Dont know/ Not sure/ N/A" &  Q3Post == "Yes" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 25,
             is.na(Q26) & Q5Post == "Dont know/ Not sure/ N/A" &  Q3Post == "Dont know/ Not sure/ N/A" &
               Q3FUS_Yes == " " & Q3FUS_No == " " ~ 26
           )) %>%
  mutate(is_switcher = switcher_group<12)

final_data <- na.omit(dat_use)
