library(tidyverse)
library(randomForest)

dat = final_data

dat = mutate_each(dat, funs(as.factor), moth_educ_scale, fath_educ_scale, work_scale,
                  no_homesupp_scale, endgrade_goodbad, is_switcher, moth_educ_tf,
                  fath_educ_tf, work_tf, pay_tf, no_homesupp_tf,
                  no_conf_tf, endgrade_tf, parent)

# Split data into 50/50 training and testing data set
set.seed(472)
n = nrow(dat)
samp = sample(1:n, size = n * 0.5)
train = final_data[samp,]
test = final_data[-samp,]


# Build logistic model
glm_original = glm(is_switcher ~ moth_educ_tf + fath_educ_tf +
                     work_tf + pay_tf + weak_tf + no_homesupp_tf +
                     no_conf_tf + endgrade_tf,
                     weights = ifelse(is_switcher == 1, 3, 1),
                     data = train, family = binomial(link = "logit"))

# Predict responses with testing data set
switch_pred = predict.glm(glm_original, newdata = test, type = "response")
# If probability is greater than 0.5, then true
switch_pred = ifelse(switch_pred > 0.5, 1, 0)
# Confusion matrix
t1 = table(truth = test$is_switcher, pred = switch_pred)
t1
# Checking accuracy
acc = sum(diag(t1))/sum(t1)
acc

# 4x's acc is 0.56
# 3x's acc is 0.70

# Random Forest
rf = randomForest(is_switcher ~ moth_educ_tf + fath_educ_tf + 
                    work_tf + pay_tf + weak_tf + no_homesupp_tf + 
                    no_conf_tf + endgrade_tf, 
                    data = train,
                    ntree = 500)
rf_pred = predict(rf, newdata = test)
t2 = table(truth = test$is_switcher, pred = rf_pred)
t2
acc = sum(diag(t2))/sum(t2)
acc