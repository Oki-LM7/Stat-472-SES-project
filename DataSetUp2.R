library(tidyverse)
library(dplyr)

dat_before <- read.csv("maalongdatafile_ANON.csv")
dat_after <- read.csv("CSPCC_Data_FUSonly (1).csv")

dat_total <- merge(dat_before, dat_after, by.x = "studid", by.y = "StudentID")


dat_use <- dat_total %>%
  select(spr26calc2 , sq5initialc2, Q3FUS_Yes, Q3FUS_No, sqr26calc2, #switcher/persister
         sp43fatheduc, sp44motheduc, sq39jobl, sp61pay, sq41preparealll, #SES
         sp54homesup, sp47mpersonparent, #home Support
         sp41weak, sqr25grade, sqr29confident, #belief 
         sq48gradel #actual
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
  mutate(is_switcher = switcher_group<12) %>%
  mutate(fath_educ_tf = 
           case_when(
             sp43fatheduc == "Did not finish high school" ~ TRUE,
             sp43fatheduc == "High school" ~ TRUE,
             sp43fatheduc == "Graduate school" ~ FALSE, 
             sp43fatheduc == "Four years of college" ~ FALSE,
             sp43fatheduc == "Some college" ~ FALSE
           )) %>%
  mutate(fath_educ_scale = 
           case_when(
             sp43fatheduc == "Did not finish high school" ~ 1,
             sp43fatheduc == "High school" ~ 2,
             sp43fatheduc == "Graduate school" ~ 5, 
             sp43fatheduc == "Four years of college" ~ 4,
             sp43fatheduc == "Some college" ~ 3
           )) %>%
    mutate(moth_educ_tf = 
           case_when(
             sp44motheduc == "Did not finish high school" ~ TRUE,
             sp44motheduc == "High school" ~ TRUE,
             sp44motheduc == "Graduate school" ~ FALSE, 
             sp44motheduc == "Four years of college" ~ FALSE,
             sp44motheduc == "Some college" ~ FALSE
           )) %>%
  mutate(moth_educ_scale = 
           case_when(
             sp44motheduc == "Did not finish high school" ~ 1,
             sp44motheduc == "High school" ~ 2,
             sp44motheduc == "Graduate school" ~ 5, 
             sp44motheduc == "Four years of college" ~ 4,
             sp44motheduc == "Some college" ~ 3
           )) %>%
  mutate(work_tf = sq39jobl >10) %>%
  mutate(work_scale = 
           case_when(
             sq39jobl == 0.0 ~ 0,
             sq39jobl == 3.0 ~ 1, 
             sq39jobl == 8.0 ~ 2,
             sq39jobl == 13.0 ~ 3,
             sq39jobl == 18.0 ~ 4,
             sq39jobl == 25.5 ~ 5,
             sq39jobl == 35.0 ~ 6
           )) %>%
  mutate(pay_tf = 
           case_when(
             sp61pay == "Strongly disagree" ~ FALSE,
             sp61pay == "Disagree" ~ FALSE,
             sp61pay == "Slightly disagree" ~ FALSE,
             sp61pay == "Strongly agree" ~ TRUE,
             sp61pay == "Agree" ~ TRUE,
             sp61pay == "Slightly agree" ~ TRUE
           )) %>%
  mutate(pay_scale = 
           case_when(
             sp61pay == "Strongly disagree" ~ 1,
             sp61pay == "Disagree" ~ 2,
             sp61pay == "Slightly disagree" ~ 3,
             sp61pay == "Strongly agree" ~ 6,
             sp61pay == "Agree" ~ 5,
             sp61pay == "Slightly agree" ~ 4
           )) %>%
  mutate(weak_tf = 
           case_when(
             sp41weak == "Strongly disagree" ~ FALSE,
             sp41weak == "Disagree" ~ FALSE,
             sp41weak == "Slightly disagree" ~ FALSE,
             sp41weak == "Strongly agree" ~ TRUE,
             sp41weak == "Agree" ~ TRUE,
             sp41weak == "Slightly agree" ~ TRUE
           )) %>%
  mutate(weak_scale = 
           case_when(
             sp41weak == "Strongly disagree" ~ 1,
             sp41weak == "Disagree" ~ 2,
             sp41weak == "Slightly disagree" ~ 3,
             sp41weak == "Strongly agree" ~ 6,
             sp41weak == "Agree" ~ 5,
             sp41weak == "Slightly agree" ~ 4
           )) %>%
  mutate(no_homesupp_tf = 
           case_when(
             sp54homesup == "Strongly" ~ FALSE, 
             sp54homesup == "Very strongly" ~FALSE,
             sp54homesup == "Somewhat" ~ TRUE,
             sp54homesup == "Not at all" ~ TRUE
           )) %>%
  mutate(no_homesupp_scale = 
           case_when(
             sp54homesup == "Strongly" ~ 3, 
             sp54homesup == "Very strongly" ~4,
             sp54homesup == "Somewhat" ~ 2,
             sp54homesup == "Not at all" ~ 1
           )) %>%
  mutate(no_conf_tf = 
           case_when(
             sqr29confident == "Strongly disagree" ~ 1,
             sqr29confident == "Disagree" ~ 2,
             sqr29confident == "Slightly disagree" ~ 3,
             sqr29confident == "Strongly agree" ~ 6,
             sqr29confident == "Agree" ~ 5,
             sqr29confident == "Slightly agree" ~ 4
           )) %>%
  mutate(endgrade_letter = sq48gradel) %>%
  mutate(endgrade_tf = sq48gradel >3.4) %>%
  mutate(endgrade_goodbad = 
           case_when(
             sq48gradel < 1.9 ~ 0, 
             (sq48gradel >1.9 & sq48gradel < 3.5) ~ 1,
             sq48gradel >= 3.5 ~ 2
           )) %>%
  mutate(endgreade_letter_rounded = 
           case_when(
             sq48gradel  < 0.6 ~ 1, #F
             (sq48gradel >0.6 & sq48gradel < 1.6) ~ 2, #D
             (sq48gradel >1.6 & sq48gradel < 2.6) ~ 3, #C
             (sq48gradel >2.6 & sq48gradel < 3.6) ~ 3, #B
             sq48gradel >3.6 ~ 4 #A
           )) %>%
  mutate(parent =
           case_when(
             sp47mpersonparent == "Not at all" ~ 0,
             sp47mpersonparent == "1" ~ 1, 
             sp47mpersonparent == "2" ~ 2, 
             sp47mpersonparent =="3" ~ 3, 
             sp47mpersonparent == "4" ~ 4, 
             sp47mpersonparent == "Very much" ~ 5))


  


final_data <- na.omit(dat_use)

#see Variable Use.txt for variable explinations