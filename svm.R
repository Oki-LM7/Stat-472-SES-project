#is_swither - true is student is a switcher, false if persister
#low_ses - true if student is low ses, false if high ses
#no_belief - true if student DID NOT believe in themself, false if the DID believe in them selves
#no_home_supp - true is student DID NOT HAVE home support, false if student HAD home support


final_data <- final_data %>%
  mutate(., is_switcher = case_when(
    is_switcher == TRUE ~ 1,
    is_switcher == FALSE ~ 0
  )) %>%
  select(., is_switcher, low_ses, no_belief, no_home_supp) 

final_data$is_switcher = factor(final_data$is_switcher, levels = c(0,1))


install.packages('caTools')
library(caTools)

split <- sample.split(final_data$is_switcher, SplitRatio = 0.75)
training_set <- subset(final_data, split == TRUE) 
test_set <- subset(final_data, split == FALSE)

install.packages('e1071')
library(e1071)

classifier <- svm(formula = is_switcher ~ .,
                  data = training_set,
                  kernel = 'radial', 
                  class.weights= c("0" = 1, "1" = 3))

classifier

y_pred <- predict(classifier, test_set[,-1])

y_pred

cm <- as.data.frame(table(test_set[,1], y_pred))
colnames(cm) <- c("Actual Classification", "Prediction", "Frequency")
formattable(cm, align = "l")

svm_tune <- tune(svm, is_switcher ~ ., data = training_set,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9),
                               class.weights = c("0" = 1, "1" = 2))
)
print(svm_tune)

best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, test_set[,-1])

best_cm <- table(test_set[,1],best_mod_pred)
best_cm
