###transformed outcome
#propensity score
e_x = mean(data_d$treatment)
e_x 


#function for transformed outcome
transformed_outcome = function(e_x, w_i, target){
  return(((w_i - e_x)/(e_x*(1-e_x)))*target)
}

#apply function on all observations
trans_out = sapply(uplift$idx, FUN = function(x){
  transformed_outcome(e_x = e_x, 
                      w_i = data_d$treatment[x], 
                      target = data_d$visit[x])
})

uplift$trans_out = trans_out
rm(trans_out)

#transformed outcome loss
outcome_loss = data.frame("k" = k_values, "loss" = 0)

#find optimal k value from transformed outcome loss
for (i in 1:length(k_values)){
  outcome_loss[i, 2] = mean((uplift$trans_out - 
                               uplift[, i + 2])^2)
}

outcome_loss

#find minimal outcome loss value
min(outcome_loss$loss)

outcome_loss$k[which.min(outcome_loss$loss)]

plot(outcome_loss$loss, type="b")

#plot result
library(ggplot2)

k_plot = ggplot(data = outcome_loss) +
  geom_point(aes(x = outcome_loss$k, 
                 y = outcome_loss$loss), 
             size = 2, shape = 18) +
  geom_point(aes(x = outcome_loss$k[which.min(outcome_loss$loss)], 
                 y = min(outcome_loss$loss)), 
             size = 4, shape = 18, color = "red") +
  geom_text(aes(x = outcome_loss$k[which.min(outcome_loss$loss)], 
                y = min(outcome_loss$loss)), 
            label = paste("K = ", outcome_loss$k[which.min(outcome_loss$loss)]), 
            color = "black", size = 4, nudge_x = 120, check_overlap = TRUE) +
  geom_line(aes(x = outcome_loss$k, 
                y = outcome_loss$loss)) +
  #stat_smooth(data = outcome_loss, 
  #            aes(x = outcome_loss$k, y = outcome_loss$loss), 
  #           method = "loess", se = FALSE, span = 0.1) +
  labs(title="Parameter Optimization of Causal-KNN CATE-Estimation", 
       x ="Value of K", y = "Outcome Loss Value") +
  theme_light()

k_plot

#extract best uplift estimation
data_d$uplift = uplift[, paste0("uplift_", outcome_loss$k[which.min(outcome_loss$loss)])]
data_d$uplift[1:20]

#ATE
ate = mean(data_d$uplift)
ate

#ATT
att = aggregate(data_d$uplift, by=list(data$treatment[1:nrow(data_d)]), FUN=mean)
att

#CATE
cate_sex = aggregate(data_d$uplift, by=list(data$mens[1:nrow(data_d)]), FUN=mean)
cate_sex

cate_hist_seg = aggregate(data_d$uplift, by=list(data$history_segment[1:nrow(data_d)]), FUN=mean)
cate_hist_seg

ggplot(data = cate_hist_seg, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#adc7db") +
  labs(title="CATE-Estimation for different History Segments", x ="History Segment", y = "CATE") +
  theme_light() +
  geom_text(aes(label = round(x, digits = 4)), vjust = 1.6, color = "black", size = 3.5)

cate_zip_code = aggregate(data_d$uplift, by=list(data$zip_code[1:nrow(data_d)]), FUN=mean)
cate_zip_code

cate_newbie = aggregate(data_d$uplift, by=list(data$newbie[1:nrow(data_d)]), FUN=mean)
cate_newbie

ggplot(data = cate_newbie, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#adc7db") +
  labs(title="CATE-Estimation for different Newbie-Status", x ="Newbie", y = "CATE") +
  theme_light() +
  geom_text(aes(label = round(x, digits = 4)), vjust = 1.6, color = "black", size = 3.5)

cate_channel = aggregate(data_d$uplift, by=list(data$channel[1:nrow(data_d)]), FUN=mean)
cate_channel

ggplot(data = cate_channel, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#adc7db") +
  labs(title="CATE-Estimation for different Communication Channels", x ="Communication Channel", y = "CATE") +
  theme_light() +
  geom_text(aes(label = round(x, digits = 4)), vjust = 1.6, color = "black", size = 3.5)

