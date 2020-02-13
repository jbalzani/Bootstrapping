library(tidyverse)
library(caret)
library(dslabs)
set.seed(1995, sample.kind = "Rounding")

data("mnist_27")
#creates 10 bootstrap samples from mnist col
indexes <- createResample(mnist_27$train$y, 10)

y <- rnorm(100, 0, 1) #random dataset 100 samples, mean 0, std dev 1
qnorm(.75) #true 75th quantile
quantile(y, .75) #sample 75th quantile

#use 10 bootstrap samples to estimate expected
#value and std error of 75th quantile
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
#create list of indexes
q75_indexes_list <- createResample(y, 10)


sample_quantile <- rep(1, length(q75_indexes_list))

for (i in 1:length(q75_indexes_list)) {
  sample_quantile[i] <- quantile(y[q75_indexes_list[[i]]], .75)
}
mean(sample_quantile)
sd(sample_quantile)

#repeat but with 10k bootstrap samples
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")

indexes_list <- createResample(y, 10000)
quantile75_2 <- sapply(indexes_list, function(ind) {
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(quantile75_2)
sd(quantile75_2)