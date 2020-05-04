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

#making plot of tradeoff between specificity and sensitivity when weighting switchers
switcher_weight = c(1,2,3,4,5,6,7,8,9,10)
sp <- c(99.3, 95.8, 90.1, 74.6,70.4,63.4,56.3,52.1,49.3, 47.2)
se <- c(6.9, 10.3, 17.2, 31.8, 31.0, 41.4, 48.3, 48.3, 48.3, 48.3)


plot(switcher_weight, sp, type = "o", col = "blue", ylim = c(0,100), xlim = c(0,10), 
     pch = 19, lwd = 2,
     main = "Sensitivity vs Specificity When Weighting Switcher Class in SVM Model",
     xlab = "Switcher Weight", ylab = "Sensitivity or Specificity Percentage")
lines(switcher_weight,se, type = "o", col = "red", pch = 19, lwd = 2)
legend("bottomright", legend = c("Specificity", "Sensitivity"), col = c("blue", "red"), pch = 19, lwd = 2, box.lty = 0)

#ROC plot
library(pROC)

plot.roc(test_set$is_switcher, as.numeric(y_pred_weight), legacy.axes = FALSE,
                col = "blue", main = "ROC plot for SVM Model",
                ylab = "Sensitivity (Proportion of Switchers Correctly Identified)", xlab = "Specificity (Proportion of Persisters Correctly Identified)")

