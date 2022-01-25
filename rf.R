library(ranger)
library(caret)
library(readr)
library(tidyverse)



fn <- "hotel_bookings.csv"  # source: https://www.kaggle.com/jessemostipak/hotel-booking-demand
df_booking <- read_csv(fn, col_types = colTypes)
smp_size_reduce <- floor(0.2 * nrow(df_booking))
reduced_idx <- sample(seq_len(nrow(df_booking)), size = smp_size_reduce)
df_reduced <- df_booking[reduced_idx, ]
df_reduced %>% select(-reservation_status_date,
                      -reservation_status,
                      -arrival_date_year,
                      -arrival_date_month,
                      -arrival_date_week_number,
                      -arrival_date_day_of_month) -> df_filtered
SPLIT_RATIO <- 0.8
set.seed(123)

df_filtered<-drop_na(df_filtered)
smp_size <- floor(SPLIT_RATIO * nrow(df_filtered))
train_ind <- sample(seq_len(nrow(df_filtered)), size = smp_size)
train <- df_filtered[train_ind, ]
test <- df_filtered[-train_ind, ]

model_rf <- ranger(
  is_canceled ~ .,
  data = train,
  importance='impurity')

df_filtered$is_canceled <- as.factor(df_filtered$is_canceled)

test_rf <- test
pred_rf <- predict(model_rf, test)
test_rf$pred <- pred_rf$predictions

cm_rf <- table(test_rf$is_canceled, pred_rf$predictions)

acc_rf <- sum(test_rf$is_canceled == test_rf$pred) / nrow(test_rf)
cat("Accurracy: ", acc_rf)
df_importance <- data.frame(attributes(model_rf$variable.importance),
                            as.vector(model_rf$variable.importance))
colnames(df_importance) <- c("name", "importance")
df_importance$name <- factor(
  df_importance$name,
  levels = df_importance$name[order(df_importance$importance)])
ggplot(df_importance, aes(x=name, y=importance, fill=importance)) + 
  geom_bar(stat="identity") +
  coord_flip()

