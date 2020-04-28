#sorting observations according to uplift values
uplift_sorted = uplift[order(uplift[, 3], decreasing = TRUE), ]


#targeting the top fixed percentage of customers(50%)
top_obs = uplift_sorted$idx[1:(0.5*nrow(uplift_sorted))]


evaluation = data.frame("visit" = test_set$visit)
evaluation$idx = test_set$idx
evaluation$treatment_cknn = 0
evaluation$treatment_cknn[top_obs] = 1


#calculate mean squared error (Hitsch - page 27)
mse = mean((transformed_outcome(e_x = e_x, test_set$treatment, test_set$visit) - uplift[, 3])^2)
mse


###calculating the mse of the causal tree model
#sorting observations according to uplift values
uplift_ct_sorted = uplift_ct[order(uplift_ct$uplift, decreasing = TRUE), ]

#targeting the top fixed percentage of customers(50%)
top_obs = uplift_ct_sorted$idx[1:(0.5*nrow(uplift_ct_sorted))]

evaluation_ct = data.frame("visit" = test_set$visit)
evaluation_ct$idx = test_set$idx
evaluation_ct$treatment_ct = 0
evaluation_ct$treatment_ct[top_obs] = 1

#calculate mean squared error (Hitsch - page 27)
mse_ct = mean((transformed_outcome(e_x = e_x, test_set$treatment, test_set$visit) - uplift_ct$uplift)^2)
mse_ct

#comparing both mse values
mse
mse_ct


###qini plot
#random assignment of treatments
qini_data_rnd = data.frame(uplift)
qini_data_rnd$visit = test_set$visit
#model assignment of treatments
qini_data_model = merge(qini_data_rnd, evaluation[,2:3], by= "idx")
qini_data_model = qini_data_model[order(qini_data_model[, 3], decreasing = TRUE), ]

# #incremental gain
# segments = seq(0, nrow(qini_data_rnd), by = 100)
# incremental_gain = data.frame("part_from" = segments[1:(length(segments)-1)]+1)
# incremental_gain$part_to = segments[1:(length(segments)-1)]+100
# incremental_gain$sum = 0
# 
# #calculate reactions for segments
# for (i in 1:(nrow(incremental_gain))){
#   incremental_gain$sum[i] = sum(qini_data_rnd$visit[incremental_gain$part_from[i]:incremental_gain$part_to[i]])
# }
# 
# #cumulative reactions
# incremental_gain$cumsum = cumsum(incremental_gain$sum)
# incremental_gain[1:10,]
# 
# sum(qini_data_rnd$visit[1:100])
# sum(qini_data_rnd$visit)

# #incremental gain
# segments = seq(0, nrow(qini_data_model), by = 100)
# incremental_gain = data.frame("part_from" = segments[1:(length(segments)-1)]+1)
# incremental_gain$part_to = segments[1:(length(segments)-1)]+100
# incremental_gain$sum = 0
# 
# #calculate reactions for segments
# for (i in 1:(nrow(incremental_gain))){
#   incremental_gain$sum[i] = sum(qini_data_model$visit[incremental_gain$part_from[i]:incremental_gain$part_to[i]])
# }
# #order by sum
# incremental_gain = incremental_gain[order(-incremental_gain$sum),]
# 
# #cumulative reactions
# incremental_gain$cumsum = cumsum(incremental_gain$sum)
# plot(incremental_gain$cumsum)

#plotting qini curves of the causal knn predictions
qini_plot_data = data.frame("visit_rnd" = cumsum(qini_data_rnd$visit), 
                            "visit_model" = cumsum(qini_data_model$visit), 
                            "idx" = 1:nrow(qini_data_rnd))

library(ggplot2)

qini_plot = ggplot(data = qini_plot_data, aes(x = idx)) +
  geom_line(aes(y = visit_rnd), method = "gam", size=1) +
  geom_line(aes(y = visit_model),color = "red", size=1) + 
  labs(title = "Qini-Curves of Treatment Assignment for Causal KNN Model", x = "Observations", y = "Cumulative Visitations of the Website") +
  theme_light()

qini_plot

#AUUC
qini_plot_data$rnd_cumsum = cumsum(qini_plot_data$visit_rnd)
qini_plot_data$model_cumsum = cumsum(qini_plot_data$visit_model)
head(qini_plot_data)

auuc_cknn = 0

for(i in 1:(nrow(qini_plot_data)-1)){
  
  x = qini_plot_data$model_cumsum[i] + 0.5*(qini_plot_data$model_cumsum[i+1]-qini_plot_data$model_cumsum[i])
  auuc_cknn = auuc_cknn + x
}
auuc_cknn

#plotting qini curves of the causal tree predictions
#random assignment of treatments
qini_data_rnd[, 3] = NULL

qini_data_rnd$uplift = uplift_ct$uplift
qini_data_model = merge(qini_data_rnd, evaluation_ct[,2:3], by= "idx")
qini_data_model = qini_data_model[order(qini_data_model[, 4], decreasing = TRUE), ]

qini_plot_data = data.frame("visit_rnd" = cumsum(qini_data_rnd$visit), 
                            "visit_model" = cumsum(qini_data_model$visit), 
                            "idx" = 1:nrow(qini_data_rnd))

qini_plot_ct = ggplot(data = qini_plot_data, aes(x = idx)) +
  geom_line(aes(y = visit_rnd), size=1) +
  geom_line(aes(y = visit_model),size=1, color = "red") + 
  labs(title = "Qini-Curves of Treatment Assignment for Causal Tree Model", x = "Observations", y = "Cumulative Visitations of the Website") +
  theme_light()

qini_plot_ct
  


#AUUC
qini_plot_data$rnd_cumsum = cumsum(qini_plot_data$visit_rnd)
qini_plot_data$model_cumsum = cumsum(qini_plot_data$visit_model)
head(qini_plot_data)

auuc_ct = 0

for(i in 1:(nrow(qini_plot_data)-1)){
  
  x = qini_plot_data$model_cumsum[i] + 0.5*(qini_plot_data$model_cumsum[i+1]-qini_plot_data$model_cumsum[i])
  auuc_ct = auuc_ct + x
}

#compare AUUC for both models
auuc_ct = auuc_ct/1000000
auuc_cknn = auuc_cknn/1000000
auuc_cknn
auuc_ct
