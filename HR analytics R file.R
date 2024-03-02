View(org)
# Load the dplyr package
install.packages('dplyr')
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages('car')
library(car)
install.packages("tidypredict")
library(tidypredict)
# Get an overview of the recruitment data
summary(org)

# See which recruiting sources the company has been using
org %>%
  count(status)
org %>% summarize(turnover_rate = mean(turnover))
df_level = org %>% group_by(level) %>% summarize(turnover_rate = mean(turnover))
##The graph represents a high turnover rate at the Analyst and Specialist levels as compared to other levels.
ggplot(df_level, aes(x = level, y = turnover_rate)) + geom_col()
df_level

unique(org$department)  # we have only one department in dataset
unique(org$location) 
# Calculate location wise turnover rate
df_location <- org %>% 
  group_by(location) %>% 
  summarize(turnover_location = mean(turnover))

# Check the results
df_location
#chicago and new york has highest turnover rate
ggplot(df_location,aes(x=location,y=turnover_location)) + geom_col()
org2 = org%>%filter(level %in% c("Analyst","Specialist"))
#filtering dataset based on level 
# View the structure of rating dataset
glimpse(rating)

# Complete the code to join rating to org2 dataset
org3 <- left_join(org2, rating, by = "emp_id")

# Calculate rating wise turnover rate
df_rating <- org3 %>% 
  group_by(rating) %>% 
  summarize(turnover_rating = mean(turnover))

# Check the result
df_rating


dataset
glimpse(survey)

# Complete the code to join survey to org3 dataset
org_final <- left_join(org3,survey,by="mgr_id")

# Compare manager effectiveness scores
ggplot(org_final, aes(x = status, y = mgr_effectiveness)) + geom_boxplot()

# View the structure of the dataset
glimpse(org_final)

# Number of variables in the dataset
variables <- 34

# Compare the travel distance of Active and Inactive employees
ggplot(org_final, aes(x = status, y = distance_from_home)) +
  geom_boxplot()

# Add age_diff
emp_age_diff <- org_final %>%
  mutate(age_diff = mgr_age - emp_age)

# Plot the distribution of age difference
ggplot(emp_age_diff, aes(x = status, y = age_diff)) + 
  geom_boxplot()

# Add job_hop_index
emp_jhi <- emp_age_diff %>% 
  mutate(job_hop_index =  total_experience / no_previous_companies_worked)

# Compare job hopping index of Active and Inactive employees
ggplot(emp_jhi, aes(x = status, y = job_hop_index)) + 
  geom_boxplot()
glimpse(emp_age_diff)

# Add tenure
emp_jhi = emp_jhi %>% 
  mutate(date_of_joining = dmy(date_of_joining),
         cutoff_date = dmy('31/12/2014'),
         last_working_date = dmy(last_working_date))
emp_tenure <- emp_jhi %>%
  mutate(tenure = ifelse(status == "Active", 
                         time_length(interval(date_of_joining, cutoff_date), 
                                     "years"), 
                         time_length(interval(date_of_joining, last_working_date), 
                                     "years")))

# Compare tenure of active and inactive employees
ggplot(emp_tenure, aes(x = status, y = tenure)) + 
  geom_boxplot()

emp_campa_ratio = emp_tenure %>% 
  group_by(level) %>% 
  mutate(median_compensation = median(compensation),
         campa_ratio = compensation/median_compensation)
  
glimpse(emp_campa_ratio)
ggplot(emp_tenure, aes(x = compensation)) + 
  geom_histogram(bins = 30)

# Plot the distribution of compensation across levels
ggplot(emp_tenure, aes(x = level, y = compensation, fill = status)) + 
  geom_boxplot()
#Here we can see 
#Nice looking plots! Did you notice the variation of 
#compensation within Analyst and Specialist levels?

emp_compa_ratio %>% 
  distinct(level, median_compensation)


emp_final = emp_campa_ratio %>% mutate(compa_level = ifelse(campa_ratio>1,"Above","Below"))
ggplot(emp_final,aes(x = status,fill = compa_level)) + geom_bar(position = 'fill')

install.packages("Information")
library(Information)
#here we are calculating information value of each and every variable
#Generally any value greater than 0.4 is considered good predictor
#any value between 0.15 and 0.4 is moderate predictor
#else all predictor variables are poor predictor
IV = create_infotables(data = emp_final, y = "turnover")
IV$Summary
emp_final$job_hop_index = emp_final$total_experience/(emp_final$no_previous_companies_worked+1)

library(caret)
set.seed(567)
index_train = createDataPartition(emp_final$turnover,p=0.7,list = FALSE)
train_set = emp_final[index_train,]
test_set = emp_final[-index_train,]

#It is important that test set is representative of training set
# Calculate turnover proportion in train_set
train_set %>% 
  count(status) %>% 
  mutate(prop = n / sum(n))

# Calculate turnover proportion in test_set
test_set %>% 
  count(status) %>% 
  mutate(prop = n / sum(n))

train_set_multi =  train_set %>%
  select(-c(emp_id, mgr_id,
            date_of_joining, last_working_date, cutoff_date,
            mgr_age, emp_age,
            median_compensation,
            department, status))
test_set_final = test_set %>%
  select(-c(emp_id, mgr_id,
            date_of_joining, last_working_date, cutoff_date,
            mgr_age, emp_age,
            median_compensation,
            department, status))
View(train_set_multi)
multi_log = glm(turnover ~ ., family = 'binomial', data = train_set_multi)
summary(multi_log)
#Remove all variables that have vif value grater than 5
vif(multi_log)
highest = 'level'
model_1 <- glm(turnover ~ . - level, family = "binomial", 
               data = train_set_multi)
vif(model_1)
model_2 <- glm(turnover ~ .-level - compensation, family = "binomial", 
               data = train_set_multi)
vif(model_2)
final_log = model_2
prediction_train = predict(final_log, newdata =train_set_multi,type ="response" )
prediction_train[c(205,645)]
hist(prediction_train)
prediction_test = predict(final_log, newdata = test_set_final,type ="response" )
hist(prediction_test)

prediction_categories <- ifelse(prediction_test > 0.5, 1, 0)

# Construct a confusion matrix
conf_matrix <- table(prediction_categories, test_set$turnover)
confusionMatrix(conf_matrix)
# we want to know how about the active employees who will be leaving the organization
emp_risk =  emp_final %>% filter(status=="Active") %>% tidypredict_to_column(final_log)
emp_risk %>% 
  select(emp_id,fit) %>%
  slice_max(n = 5, fit)
emp_risk_bucket = emp_risk %>% mutate(risk_bucket = cut(fit, breaks = c(0,0.5,0.6,0.8, 1)))
emp_risk_bucket %>% 
  count(risk_bucket)

# Plot histogram of percent hike
ggplot(emp_final, aes(x = percent_hike)) +
  geom_histogram(binwidth = 3)

# Create salary hike_range of Analyst level employees
emp_hike_range <- emp_final %>% 
  filter(level == "Analyst") %>% 
  mutate(hike_range = cut(percent_hike, breaks = c(0, 10, 15, 20),
                          include.lowest = TRUE, 
                          labels = c("0 to 10", 
                                     "11 to 15", "16 to 20")))


# Calculate the turnover rate for each salary hike range
df_hike <- emp_hike_range %>% 
  group_by(hike_range) %>% 
  summarize(turnover_rate_hike = mean(turnover))

# Check the results
summarize(df_hike)

# Visualize the results
ggplot(df_hike, aes(x = hike_range, y = turnover_rate_hike)) + 
  geom_col()

# Compute extra cost
extra_cost <- 50000 * (0.15-0.10)

# Compute savings
savings <-  40000 * (0.17)

# Calculate ROI
ROI <- (savings / extra_cost) * 100

# Print ROI
cat(paste0("The return on investment is ", round(ROI), "%!"))
summary(final_log)