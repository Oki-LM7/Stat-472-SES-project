library(tidyverse)
library(ggplot2)
library(caret)

dat = final_data

dat = mutate_each(dat, funs(as.factor), moth_educ_scale, fath_educ_scale, work_scale,
                  no_homesupp_scale, endgrade_goodbad, is_switcher, moth_educ_tf,
                  fath_educ_tf, work_tf, pay_tf, no_homesupp_tf,
                  no_conf_tf, endgrade_tf, parent)

# Split data into 75/25 training and testing data set
set.seed(472)
n = nrow(dat)
samp = sample(1:n, size = n * 0.75)
train = final_data[samp,]
test = final_data[-samp,]


# Build logistic model
glm_original = glm(is_switcher ~ moth_educ_scale + fath_educ_scale +
                     work_scale + weak_scale + no_homesupp_scale + no_conf_tf,
                     weights = ifelse(is_switcher == 1, 4, 1),
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

# 4x's acc is 0.63
# 3x's acc is 0.77

n = 10
sens = 1:n
spec = 1:n
accs = 1:n
for(i in 1:n){
  glm_original = glm(is_switcher ~ moth_educ_scale + fath_educ_scale +
                       work_scale + weak_scale + no_homesupp_scale + no_conf_tf,
                     weights = ifelse(is_switcher == 1, i, 1),
                     data = train, family = binomial(link = "logit"))
  
  # Predict responses with testing data set
  switch_pred = predict.glm(glm_original, newdata = test, type = "response")
  # If probability is greater than 0.5, then true
  switch_pred = ifelse(switch_pred > 0.5, 1, 0)
  t1 = table(pred = switch_pred, truth = test$is_switcher)
  sens[i] = t1[1, 1]/(t1[1, 1] + t1[2, 1])
  spec[i] = t1[2, 2]/(t1[1, 2] + t1[2, 2])
  accs[i] = sum(diag(t1))/sum(t1)
}

# Weight vs Sensitivity, Specificity, and Accuracy
stats = c(sens, spec, accs)
weight = rep(1:n, 3)
Measure = rep(c("Sensitivity", "Specificity", "Accuracy"), each = n)
summ = data.frame(measure, stats, weight)

# Plot 1
ggplot(summ, aes(weight, stats, color = Measure)) + geom_line() + 
  scale_x_continuous(breaks = 1:n) +
  labs(title = "Weight vs Sensitivity, Specificity, and Accuracy", x = "Weight", y = "Stats") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme_test()

# Sensitivity vs Specificity
mes = data.frame(sens, spec)

# Plot 2
ggplot(mes, aes(spec, sens)) + geom_line() +
  labs(title = "Specificity vs Sensitivity", x = "Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme_test()
  

# Changing work scale while holding everything else constant at 3
vals = 1:6
con = 1
pred_dat = data.frame(moth_educ_scale = con, fath_educ_scale = con,
                      weak_scale = con, no_homesupp_scale = con,
                      work_scale = con)
pe = lapply(vals, function(j){
  pred_dat$no_conf_tf = j
  predict(glm_original, newdata = pred_dat, type = "response")
})
pred_vals = 1:6
for(i in 1:6){
  pred_vals[i] = pe[[i]]
}
pred_work = data.frame(vals, pred_vals)

# Plot 3
ggplot(pred_work, aes(vals, pred_vals)) + geom_line() + 
  labs(title = "Switcher or Persister given Confidence", x = "Confidence Scale", y = "Switcher or Persister") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme_test()
  