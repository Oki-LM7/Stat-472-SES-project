# Changed sqr25grade and sum_ses to a factor
final_data = mutate_each(final_data, funs(as.factor), sqr25grade, sum_ses)

# Split data into 60/40 training and testing data set
n = nrow(final_data)
samp = sample(1:n, size = n * 0.6)
train = final_data[samp,]
test = final_data[-samp,]

# Build logistic model
glm_original = glm(is_switcher ~ low_ses + no_belief + no_home_supp, data = train,
                   family = binomial(link = "logit"))
# Playing with model, has better accuracy
# glm1 = glm(is_switcher ~ low_ses + no_belief + no_home_supp + sqr25grade + 
#              sq39jobl + sp43fatheduc + sp44motheduc, data = train,
#            family = binomial(link = "logit"))

# Predict responses with testing data set
switch_pred = predict.glm(glm_original, newdata = test, type = "response")
# If probability is greater than .5, then true
switch_pred = ifelse(switch_pred > 0.5, 1, 0)
# Checking accuracy
t1 = table(truth = test$is_switcher, pred = switch_pred)
acc = sum(diag(t1))/sum(t1)
acc

# Check for omitted values based on given variable
table(train$Q3FUS_Yes)
table(train$sq48gradel, train$is_switcher)
