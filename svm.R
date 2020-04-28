#is_swither - true is student is a switcher, false if persister
#low_ses - true if student is low ses, false if high ses
#no_belief - true if student DID NOT believe in themself, false if the DID believe in them selves
#no_home_supp - true is student DID NOT HAVE home support, false if student HAD home support


final_data <- final_data %>%
  mutate(., is_switcher = case_when(
    is_switcher == TRUE ~ 1,
    is_switcher == FALSE ~ 0
  )) %>%
  select(., is_switcher, fath_educ_scale, moth_educ_scale, work_scale,
         weak_scale, no_homesupp_scale, no_conf_tf) 

final_data$is_switcher = factor(final_data$is_switcher, levels = c(0,1))

install.packages('e1071')
install.packages('caTools')
library(caTools)
library(e1071)

set.seed(472)
split <- sample.split(final_data$is_switcher, SplitRatio = 0.75)
training_set <- subset(final_data, split == TRUE) 
test_set <- subset(final_data, split == FALSE)



classifier <- svm(formula = is_switcher ~ .,
                  data = training_set,
                  kernel = 'radial')

classifier

y_pred <- predict(classifier, test_set[,-1])

y_pred

cm <- as.data.frame(table(test_set[,1], y_pred))
colnames(cm) <- c("Actual Classification", "Prediction", "Frequency")
formattable(cm, align = "l")

accuracy = 143/171 #83.6%
specificity = 141/142 #99.3% #prop of persisters who were correctly identified as such
sensitivity = 2/29 #6.9% #prop of switchers who were correctly identified as such

#can increase sensitivity by weighting switcher class higher, but this decreases specificity and overall accuracy
#still very low sensitivity though

classifier_weight <- svm(formula = is_switcher ~ .,
                  data = training_set,
                  kernel = 'radial',
                  class.weights= c("0" = 1, "1" = 3))

classifier_weight

y_pred_weight <- predict(classifier_weight, test_set[,-1])

y_pred_weight

cm_weight <- as.data.frame(table(test_set[,1], y_pred_weight))
colnames(cm_weight) <- c("Actual Classification", "Prediction", "Frequency")
formattable(cm_weight, align = "l")

#accuracy = 133/171 = 77.7%
#sp = 128/142 = 90.1%
#se = 5/29 = 17.2%

