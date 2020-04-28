###repeating causal knn with continuous target variable spend
#using reduced data set
data_d = data_d[1:10000, ]

###splitting data set
#1 no treatment
data_nt = data_d[which(data_d$segment.No.E.Mail == 1),]

#2 treatment
data_t = data_d[which(data_d$segment.No.E.Mail == 0),]

###Causal KNN
#install.packages("FNN")
library(FNN)

#setting parameter k for number of nearest neighbours
k = 3000
k = k + 1

#select columns to be eliminated for nearest neighbour search
drop_cols = c("visit", 
              "spend", 
              "conversion", 
              "idx", 
              "treatment", 
              "segment.Mens.E.Mail",
              "segment.Womens.E.Mail",
              "segment.No.E.Mail")

#garbage collection to maximize available main memory
gc()

#calculate indices of k treated nearest neighbours
treated_nn = get.knnx(data_t[, !(names(data_t) %in% drop_cols)], 
                      query = data_d[, !(names(data_d) %in% drop_cols)], 
                      k = k)
treated_nn = data.frame(treated_nn$nn.index)

#deleting first column
treated_nn = treated_nn[, 2:ncol(treated_nn)]

#calculate indices of k untreated nearest neighbours
untreated_nn = get.knnx(data_nt[, !(names(data_nt) %in% drop_cols)], 
                        query = data_d[, !(names(data_d) %in% drop_cols)], 
                        k = k)
untreated_nn = data.frame(untreated_nn$nn.index)

#deleting first column
untreated_nn = untreated_nn[, 2:ncol(untreated_nn)]

#replacing index values
treated_nn = sapply(treated_nn, FUN = function(x){
  data_t$idx[x]
})

untreated_nn = sapply(untreated_nn, FUN = function(x){
  data_nt$idx[x]
}) 

treated_nn = t(treated_nn)
untreated_nn = t(untreated_nn)

#setting parameters for the number of neigbours
k_start = 100
k_end = 3000
steps = 100

#creating sequence of k values to test for
k_values = seq(from = k_start, to = k_end, by = steps)

#preparing ulift data frame
uplift = data.frame("idx" = data_d$idx,
                    "treatment" = data_d$treatment)

#calculating uplift for specified k values
for (k in k_values) {
  reaction_nt = apply(untreated_nn, MARGIN = 2, FUN = function(x){
    mean(data_d$spend[x[1:k]])
  })
  
  reaction_t = apply(treated_nn, MARGIN = 2, FUN = function(x){
    mean(data_d$spend[x[1:k]])
  })
  
  uplift[paste("uplift_",k, sep = "")] = reaction_t - reaction_nt
  
  print(paste0("k = ", k))
}

head(uplift)
dim(treated_nn)

###transformed outcome
#propensity score
e_x = mean(data_d$treatment)

#apply function on all observations
trans_out = sapply(uplift$idx, FUN = function(x){
  transformed_outcome(e_x = e_x, w_i = data$treatment[x], target = data$spend[x])
})

uplift$trans_out = trans_out
rm(trans_out)

#transformed outcome loss
outcome_loss = data.frame("k" = k_values, "loss" = 0)

#find optimal k value
for (i in 1:length(k_values)){
  outcome_loss[i, 2] = mean((uplift$trans_out - uplift[, i + 2])^2)
}

#find minimal value
min(outcome_loss$loss)

outcome_loss$k[which.min(outcome_loss$loss)]

plot(outcome_loss$loss)

k_plot2 = ggplot(data = outcome_loss) +
  geom_point(aes(x = outcome_loss$k, y = outcome_loss$loss), size = 2, shape = 18) +
  geom_point(aes(x = outcome_loss$k[which.min(outcome_loss$loss)], y = min(outcome_loss$loss)), 
             size = 4, shape = 18, color = "red") +
  geom_text(aes(x = outcome_loss$k[which.min(outcome_loss$loss)], y = min(outcome_loss$loss)), 
            label = paste("K = ", outcome_loss$k[which.min(outcome_loss$loss)]), color = "black", size = 4, 
            nudge_x = 0, nudge_y = 0.15, check_overlap = TRUE) +
  stat_smooth(data = outcome_loss, aes(x = outcome_loss$k, y = outcome_loss$loss), method = "loess", 
              se = FALSE, span = 0.1) +
  labs(title="Parameter Optimization of Causal-KNN CATE-Estimation", x ="Value of K", y = "Outcome Loss Value") +
  theme_light()

k_plot2


#extract best uplift estimation
data_d$uplift = uplift[, 'uplift_2900']
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


ggplot(data = cate_sex, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#adc7db") +
  labs(title="CATE-Estimation for different Sexes", x ="Sex = Male", y = "CATE") +
  theme_light() +
  geom_text(aes(label = round(x, digits = 4)), vjust = 1.6, color = "black", size = 3.5)

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
  labs(title="CATE-Estimation for different Newbie Status", x ="Newbie", y = "CATE") +
  theme_light() +
  geom_text(aes(label = round(x, digits = 4)), vjust = 1.6, color = "black", size = 3.5)

cate_channel = aggregate(data_d$uplift, by=list(data$channel[1:nrow(data_d)]), FUN=mean)
cate_channel

ggplot(data = cate_channel, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#adc7db") +
  labs(title="CATE-Estimation for different Channels", x ="Channel", y = "CATE") +
  theme_light() +
  geom_text(aes(label = round(x, digits = 4)), vjust = 1.6, color = "black", size = 3.5)