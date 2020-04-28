###using causal knn as predictive model
test_set = createDummyFeatures(data[25001:30000, ])
test_set$idx = 1:nrow(test_set)


#splitting data set
#1 no treatment
data_nt = data_d[which(data_d$segment.No.E.Mail == 1),]

#2 treatment
data_t = data_d[which(data_d$segment.No.E.Mail == 0),]


#running causal knn for test set to calculate mse
#select target columns
drop_cols = c("visit", 
              "spend", 
              "conversion", 
              "idx", 
              "treatment", 
              "segment.Mens.E.Mail",
              "segment.Womens.E.Mail",
              "segment.No.E.Mail",
              "uplift")

#setting optimal k value from the parameter tuning above
k = outcome_loss$k[which.min(outcome_loss$loss)] + 1


#calculate indices of k treated nearest neighbours
treated_nn = get.knnx(data_t[, !(names(data_t) %in% drop_cols)], 
                      query = test_set[, !(names(test_set) %in% drop_cols)], 
                      k = k)
treated_nn = data.frame(treated_nn$nn.index)

#deleting first column
treated_nn = treated_nn[, 2:ncol(treated_nn)]


#calculate indices of k untreated nearest neighbours
untreated_nn = get.knnx(data_nt[, !(names(data_nt) %in% drop_cols)], 
                        query = test_set[, !(names(test_set) %in% drop_cols)], 
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

#transpose data frames
treated_nn = t(treated_nn)
untreated_nn = t(untreated_nn)

#preparing uplift data frame
uplift = data.frame("idx" = test_set$idx,
                    "treatment" = test_set$treatment)

reaction_nt = apply(untreated_nn, MARGIN = 2, FUN = function(x){
  mean(data_d$visit[x[1:k-1]])
})

reaction_t = apply(treated_nn, MARGIN = 2, FUN = function(x){
  mean(data_d$visit[x[1:k-1]])
})

uplift[paste("uplift_", k, sep = "")] = reaction_t - reaction_nt
