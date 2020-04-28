#using transformed outcome for tuning parameters of other uplift models
#different cp for causal tree


#install.packages("devtools")
#library(devtools) 
#install_github("susanathey/causalTree")

library(causalTree)


#setting parameters for the complexity parameters to test for
cp_start = 0
cp_end = 0.0001
cp_steps = 0.00001

#creating sequence of k values to test for
cp_values = seq(from = cp_start, to = cp_end, by = cp_steps)
length(cp_values)

uplift_ct = data.frame("idx" = data_d$idx, "treatment" = data_d$treatment, "visit" = data_d$visit) 

#calculating uplift for specified min size values
for (cp in cp_values) {
  causal_tree = causalTree(visit~.-spend -conversion -idx -segment.Mens.E.Mail -segment.Womens.E.Mail -segment.No.E.Mail, data = data_d, treatment = data_d$treatment, split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, xval = 5, cp = cp, propensity = e_x)
  
  uplift_ct[paste("uplift_",cp, sep = "")] = sapply(causal_tree$where, FUN = function(x){causal_tree$frame$yval[x]})
  
  print(paste("cp =", cp))
}


#calculate the transformed outcome
trans_out_ct = sapply(uplift_ct$idx, FUN = function(x){
  transformed_outcome(e_x = e_x, w_i = data_d$treatment[x], target = data_d$visit[x])
})

uplift_ct$trans_out_ct = trans_out_ct

#transformed outcome loss
outcome_loss_ct = data.frame("cp" = cp_values, "loss" = 0)

#find optimal cp value

for (i in 1:length(cp_values)){
  outcome_loss_ct[i, 2] = mean((uplift_ct$trans_out_ct - uplift_ct[, i+3 ])^2, na.rm = TRUE)
}

outcome_loss_ct

#find minimal value
min(outcome_loss_ct$loss)

outcome_loss_ct$cp[which.min(outcome_loss_ct$loss)]

plot(outcome_loss_ct$loss)

cp_plot = ggplot(data = outcome_loss_ct) +
  geom_line(aes(x = outcome_loss_ct$cp, y = outcome_loss_ct$loss)) +
  geom_point(aes(x = outcome_loss_ct$cp, y = outcome_loss_ct$loss), size=2, shape=18) +
  geom_point(aes(x = outcome_loss_ct$cp[which.min(outcome_loss_ct$loss)], y = min(outcome_loss_ct$loss)), 
             size = 4, shape = 18, color = "red") +
  geom_text(aes(x = outcome_loss_ct$cp[which.min(outcome_loss_ct$loss)], y = min(outcome_loss_ct$loss)), 
            label = paste("cp = ", outcome_loss_ct$cp[which.min(outcome_loss_ct$loss)]), color = "black", size = 4, 
            nudge_x = 0.00001, nudge_y = 0, check_overlap = TRUE) +
  labs(title="Parameter Optimization of Causal-KNN CATE-Estimation", x ="Value of Complexity Parameter", y = "Outcome Loss Value") +
  theme_light()

cp_plot


### building causal tree model for test set
#extracting optimal value for complexity parameter (cp) from tuning
optimal_cp = outcome_loss_ct$cp[which.min(outcome_loss_ct$loss)]

#learning causal tree model for the test set
causal_tree = causalTree(visit~.-spend -conversion -idx -segment.Mens.E.Mail -segment.Womens.E.Mail -segment.No.E.Mail, 
                         data = test_set, treatment = test_set$treatment, split.Rule = "CT", cv.option = "CT", 
                         split.Honest = T, cv.Honest = T, split.Bucket = F, xval = 5, cp = optimal_cp, minsize = 20, 
                         propensity = e_x)

uplift_ct = data.frame("idx" = test_set$idx, "treatment" = test_set$treatment, "visit" = test_set$visit) 

uplift_ct$uplift = sapply(causal_tree$where, FUN = function(x){causal_tree$frame$yval[x]})
