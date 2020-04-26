dat_persist <- final_data[!final_data$is_switcher, ]
dat_switcher <- final_data[final_data$is_switcher, ]

num <- dim(dat_switcher)[1]

samp <- sample(dim(dat_persist)[1], num)
dat_persist_use <- dat_persist[samp,]

final_data <- rbind(dat_persist_use, dat_switcher)

set.seed(472)
n = nrow(final_data)
samp = sample(1:n, size = n * 0.6)
train = final_data[samp,]
test = final_data[-samp,]

# Build logistic model
glm_original = glm(is_switcher ~ low_ses + no_belief + no_home_supp, data = train,
                   family = binomial(link = "logit"))
# Playing with model, has better accuracy
glm1 = glm(is_switcher ~ low_ses + no_belief + no_home_supp + sqr25grade + 
             sq39jobl + sp43fatheduc + sp44motheduc, data = train,
           family = binomial(link = "logit"))
glm2 = glm(is_switcher ~  sp43fatheduc + sp44motheduc +
            work + pay +weak +ability +parent_belief +sum_home, 
           data = train,
           family = binomial(link = "logit"))


# Predict responses with testing data set
switch_pred = predict.glm(glm_original, newdata = test, type = "response")
switch_pred1 = predict.glm(glm1, newdata = test, type = "response")
switch_pred2 = predict.glm(glm2, newdata = test, type = "response")

# If probability is greater than 0.5, then true
switch_pred = ifelse(switch_pred > 0.5, 1, 0)
switch_pred1 = ifelse(switch_pred1 > 0.5, 1, 0)
switch_pred2 = ifelse(switch_pred2 > 0.5, 1, 0)

# Checking accuracy
t1 = table(truth = test$is_switcher, pred = switch_pred)
acc = sum(diag(t1))/sum(t1)
acc

t11 = table(truth = test$is_switcher, pred = switch_pred1)
acc1 = sum(diag(t11))/sum(t11)
ac
c1

t12 = table(truth = test$is_switcher, pred = switch_pred2)
acc2 = sum(diag(t12))/sum(t12)
acc2
# Check for omitted values based on given variable
table(train$Q3FUS_Yes)
table(train$sq48gradel, train$is_switcher)
