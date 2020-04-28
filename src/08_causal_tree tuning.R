#setting parameters for the complexity parameters to test for
ms_start = 0
ms_end = 20
ms_steps = 5

#creating sequence of k values to test for
ms_values = seq(from = ms_start, to = ms_end, by = ms_steps)

uplift_ct = data.frame("idx" = data_d$idx, "treatment" = data_d$treatment, "visit" = data_d$visit) 

#propensity score
e_x = mean(data_d$treatment)
e_x

library(causalTree)

#calculating uplift for specified min size values
for (ms in ms_values) {
  causal_tree = causalTree(visit~.-spend -conversion -idx -segment.Mens.E.Mail -segment.Womens.E.Mail -segment.No.E.Mail, 
                           data = data_d, treatment = data_d$treatment, split.Rule = "CT", cv.option = "CT", 
                           split.Honest = T, cv.Honest = T, split.Bucket = F, xval = 5, cp = 0, minsize = ms, 
                           propensity = e_x)
  
  uplift_ct[paste("uplift_",ms, sep = "")] = sapply(causal_tree$where, FUN = function(x){causal_tree$frame$yval[x]})
  
  print(paste("ms = ", ms))
}

#calculate the transformed outcome
uplift_ct$trans_out = sapply(uplift_ct$idx, FUN = function(x){
  transformed_outcome(e_x = e_x, w_i = data_d$treatment[x], target = data_d$visit[x])
})

uplift_ct$trans_out[1:20]
data$treatment[1:20]
data$visit[1:20]


#transformed outcome loss
outcome_loss = data.frame("ms" = ms_values, "loss" = 0)


#find optimal k value
for (i in 1:length(ms_values)){
  outcome_loss[i, 2] = mean((uplift_ct$trans_out - uplift_ct[, i + 3])^2)
}


outcome_loss

#find minimal value
min(outcome_loss$loss)

outcome_loss$ms[which.min(outcome_loss$loss)]

plot(outcome_loss$loss)

ms_plot = ggplot(data = outcome_loss) +
  geom_point(aes(x = outcome_loss$ms, y = outcome_loss$loss), size = 2, shape = 18) +
  geom_point(aes(x = outcome_loss$ms[which.min(outcome_loss$loss)], y = min(outcome_loss$loss)), 
             size = 4, shape = 18, color = "red") +
  geom_text(aes(x = outcome_loss$ms[which.min(outcome_loss$loss)], y = min(outcome_loss$loss)), 
            label = paste("min size = ", outcome_loss$ms[which.min(outcome_loss$loss)]), color = "black", size = 4, 
            nudge_x = 5, nudge_y = 0, check_overlap = TRUE) +
  stat_smooth(data = outcome_loss, aes(x = outcome_loss$ms, y = outcome_loss$loss), method = "loess", 
              se = FALSE, span = 0.1) +
  labs(title="Parameter Optimization of Causal-KNN CATE-Estimation", x ="Value of K", y = "Outcome Loss Value") +
  theme_light()

ms_plot
