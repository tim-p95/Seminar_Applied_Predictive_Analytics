#select data partition for training
data_d = data_[1:25000, ]


###splitting data set
#1 no treatment
data_nt = data_d[which(data_d$segment.No.E.Mail == 1),]

#2 treatment
data_t = data_d[which(data_d$segment.No.E.Mail == 0),]

#count non-zero observations of the target variables
sum(data_d$visit != 0)
sum(data_d$conversion != 0)
sum(data_d$spend != 0)

###Causal KNN
#install.packages("FNN")
library(FNN)

#setting parameter k for the maximum number of nearest neighbours to test for
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

#transpose data frames
treated_nn = t(treated_nn)
untreated_nn = t(untreated_nn)


###parameter tuning to find optimal k value
#setting parameters for the number of neigbours
k_start = 50
k_end = 3000
steps = 50

#creating sequence of k values to test for
k_values = seq(from = k_start, to = k_end, by = steps)

#preparing uplift data frame
uplift = data.frame("idx" = data_d$idx,
                    "treatment" = data_d$treatment)

#calculating uplift for specified k values
for (k in k_values) {
  reaction_nt = apply(untreated_nn, MARGIN = 2, 
                      FUN = function(x){
                        mean(data_d$visit[x[1:k]])
                      }
  )
  
  reaction_t = apply(treated_nn, MARGIN = 2, 
                     FUN = function(x){
                       mean(data_d$visit[x[1:k]])
                     }
  )
  
  uplift[paste("uplift_",k, sep = "")] = reaction_t - reaction_nt
  
  print(paste("k = ", k))
}
