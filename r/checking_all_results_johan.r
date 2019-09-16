library(gplots)
library(graphics)
library(vcd)

dat <- read.csv(file.choose())
head(dat)
dat <- results.test_1
dat <- results.test_2
dat <- results.test_7
dat <- results.test_8
dat <- results.test_9
dat <- all_results_temp

dat[grep("texts_no_cull", dat[,3]),3] <- "Texts"
dat[grep("small_samp_no_cull", dat[,3]),3] <- "Small"
dat[grep("large_samp_no_cull", dat[,3]),3] <- "Large"

# descriptives through tables

t_sample <- table(dat$sample, dat$result_dumb)
prop.table(t_sample, 1)
summary(t_sample)

t_dist <- table(dat$dist_meas, dat$result_dumb)
prop.table(t_dist, 1)
summary(t_dist)


t_feature <- table(dat$feature, dat$result_dumb)
prop.table(t_feature, 1)
summary(t_feature)

t_feat_freq <- table(dat$feat_freq, dat$result_dumb)
prop.table(t_feat_freq, 1)
summary(t_feat_freq)

#one for all
par(mfrow=c(2,2))
mosaicplot(t_sample, shade = TRUE, las=2,
           main = "Sample")
mosaicplot(t_dist, shade = TRUE, las=2,
           main = "Measurements")
mosaicplot(t_feature, shade = TRUE, las=2,
           main = "Feature")
mosaicplot(t_feat_freq, shade = TRUE, las=2,
           main = "Frequency")
par(mfrow=c(1,1))


for(i in 1:5) {
  if(i == 1) {
    dat <- results.test_1
    dat[grep("texts_no_cull", dat[,3]),3] <- "Texts"
    dat[grep("small_samp_no_cull", dat[,3]),3] <- "Small"
    dat[grep("large_samp_no_cull", dat[,3]),3] <- "Large"
    t_sample_1 <- table(dat$sample, dat$result_dumb)
    t_dist_1 <- table(dat$dist_meas, dat$result_dumb)
    t_feature_1 <- table(dat$feature, dat$result_dumb)
    t_feat_freq_1 <- table(dat$feat_freq, dat$result_dumb)
  } else if (i == 2) {
    dat <- results.test_2
    dat[grep("texts_no_cull", dat[,3]),3] <- "Texts"
    dat[grep("small_samp_no_cull", dat[,3]),3] <- "Small"
    dat[grep("large_samp_no_cull", dat[,3]),3] <- "Large"
    t_sample_2 <- table(dat$sample, dat$result_dumb)
    t_dist_2 <- table(dat$dist_meas, dat$result_dumb)
    t_feature_2 <- table(dat$feature, dat$result_dumb)
    t_feat_freq_2 <- table(dat$feat_freq, dat$result_dumb)
  } else if (i == 3) {
    dat <- results.test_7
    dat[grep("texts_no_cull", dat[,3]),3] <- "Texts"
    dat[grep("small_samp_no_cull", dat[,3]),3] <- "Small"
    dat[grep("large_samp_no_cull", dat[,3]),3] <- "Large"
    t_sample_3 <- table(dat$sample, dat$result_dumb)
    t_dist_3 <- table(dat$dist_meas, dat$result_dumb)
    t_feature_3 <- table(dat$feature, dat$result_dumb)
    t_feat_freq_3 <- table(dat$feat_freq, dat$result_dumb)
  } else if (i == 4) {
    dat <- results.test_8
    dat[grep("texts_no_cull", dat[,3]),3] <- "Texts"
    dat[grep("small_samp_no_cull", dat[,3]),3] <- "Small"
    dat[grep("large_samp_no_cull", dat[,3]),3] <- "Large"
    t_sample_4 <- table(dat$sample, dat$result_dumb)
    t_dist_4 <- table(dat$dist_meas, dat$result_dumb)
    t_feature_4 <- table(dat$feature, dat$result_dumb)
    t_feat_freq_4 <- table(dat$feat_freq, dat$result_dumb)
  } else if (i == 5) {
    dat <- results.test_9
    dat[grep("texts_no_cull", dat[,3]),3] <- "Texts"
    dat[grep("small_samp_no_cull", dat[,3]),3] <- "Small"
    dat[grep("large_samp_no_cull", dat[,3]),3] <- "Large"
    t_sample_5 <- table(dat$sample, dat$result_dumb)
    t_dist_5 <- table(dat$dist_meas, dat$result_dumb)
    t_feature_5 <- table(dat$feature, dat$result_dumb)
    t_feat_freq_5 <- table(dat$feat_freq, dat$result_dumb)
  }
}  

#plot by test

#sample size
par(mfrow=c(2,3))
mosaicplot(t_sample_1, shade = TRUE, las=1,
           main = "Sample Size",
           sub = "Dataset 1")
mosaicplot(t_sample_2, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 2")
mosaicplot(t_sample_3, shade = TRUE, las=1,
           main = NULL, sub="Dataset 3")
mosaicplot(t_sample_4, shade = TRUE, las=1,
           main = NULL,sub="Dataset 4")
mosaicplot(t_sample_5, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 5")
par(mfrow=c(1,1))

#distance measurement
par(mfrow=c(2,3))
mosaicplot(t_dist_1, shade = TRUE, las=1,
           main = "Distance Measurement",
           sub = "Dataset 1")
mosaicplot(t_dist_2, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 2")
mosaicplot(t_dist_3, shade = TRUE, las=1,
           main = NULL, sub="Dataset 3")
mosaicplot(t_dist_4, shade = TRUE, las=1,
           main = NULL,sub="Dataset 4")
mosaicplot(t_dist_5, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 5")
par(mfrow=c(1,1))

#feature
par(mfrow=c(2,3))
mosaicplot(t_feature_1, shade = TRUE, las=1,
           main = "Feature",
           sub = "Dataset 1")
mosaicplot(t_feature_2, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 2")
mosaicplot(t_feature_3, shade = TRUE, las=1,
           main = NULL, sub="Dataset 3")
mosaicplot(t_feature_4, shade = TRUE, las=1,
           main = NULL,sub="Dataset 4")
mosaicplot(t_feature_5, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 5")
par(mfrow=c(1,1))

#freq
par(mfrow=c(2,3))
mosaicplot(t_feat_freq_1, shade = TRUE, las=1,
           main = "Frequency",
           sub = "Dataset 1")
mosaicplot(t_feat_freq_2, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 2")
mosaicplot(t_feat_freq_3, shade = TRUE, las=1,
           main = NULL, sub="Dataset 3")
mosaicplot(t_feat_freq_4, shade = TRUE, las=1,
           main = NULL,sub="Dataset 4")
mosaicplot(t_feat_freq_5, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 5")
par(mfrow=c(1,1))





par(mfrow=c(5,1))
mosaicplot(t_sample_1, shade = TRUE, las=1,
           main = "Sample",
           sub = "Dataset 1")
mosaicplot(t_sample_2, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 2")
mosaicplot(t_sample_3, shade = TRUE, las=1,
           main = NULL, sub="Dataset 3")
mosaicplot(t_sample_4, shade = TRUE, las=1,
           main = NULL,sub="Dataset 4")
mosaicplot(t_sample_5, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 5")
par(mfrow=c(1,1))

par(mfrow=c(2,3))
mosaicplot(t_sample_1, shade = TRUE, las=2,
           main = "Sample",
           sub = "Dataset 1")
mosaicplot(t_sample_2, shade = TRUE, las=2,
           main = NULL, sub = "Dataset 2")
mosaicplot(t_sample_3, shade = TRUE, las=2,
           main = NULL, sub="Dataset 3")
mosaicplot(t_sample_4, shade = TRUE, las=2,
           main = NULL,sub="Dataset 4")
mosaicplot(t_sample_5, shade = TRUE, las=2,
           main = NULL, sub = "Dataset 5")
par(mfrow=c(1,1))

par(mfrow=c(2,3))
mosaicplot(t_sample_1, shade = TRUE, las=1,
           main = "Sample Size",
           sub = "Dataset 1")
mosaicplot(t_sample_2, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 2")
mosaicplot(t_sample_3, shade = TRUE, las=1,
           main = NULL, sub="Dataset 3")
mosaicplot(t_sample_4, shade = TRUE, las=1,
           main = NULL,sub="Dataset 4")
mosaicplot(t_sample_5, shade = TRUE, las=1,
           main = NULL, sub = "Dataset 5")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
mosaicplot(t_sample, shade = TRUE, las=2,
           main = "Sample")
mosaicplot(t_dist, shade = TRUE, las=2,
           main = "Measurements")
mosaicplot(t_feature, shade = TRUE, las=2,
           main = "Feature")
mosaicplot(t_feat_freq, shade = TRUE, las=2,
           main = "Frequency")
par(mfrow=c(1,1))


#4 per page
test <- "test_1"
par(mfrow=c(2,2))
png(paste0("plots/", test, "/", test, "_plots.png")) 
t_sample <- table(dat$sample, dat$result_dumb)
mosaicplot(t_sample, shade = TRUE, las=2,
           main = "Sample")
t_dist <- table(dat$dist_meas, dat$result_dumb)
mosaicplot(t_dist, shade = TRUE, las=2,
           main = "Measurements")
t_feature <- table(dat$feature, dat$result_dumb)
mosaicplot(t_feature, shade = TRUE, las=2,
           main = "Feature")
t_feat_freq <- table(dat$feat_freq, dat$result_dumb)
mosaicplot(t_feat_freq, shade = TRUE, las=2,
           main = "Frequency")

dev.off()
par(mfrow=c(1,1))


test <- "test_1"
plots <- c("sample", "measure", "feature", "frequency")
for(i in 1:4) {
  pdf(paste0("plots/", test, "/", plots[i], ".pdf")) 
  if (i == 1) {
    t_sample <- table(dat$sample, dat$result_dumb)
    mosaicplot(t_sample, shade = TRUE, las=2,
               main = "Sample")
  } else if (i == 2) {
    t_dist <- table(dat$dist_meas, dat$result_dumb)
    mosaicplot(t_dist, shade = TRUE, las=2,
               main = "Measurements")
  } else if (i == 3) {
    t_feature <- table(dat$feature, dat$result_dumb)
    mosaicplot(t_feature, shade = TRUE, las=2,
               main = "Feature")
  } else if (i == 4) {
    t_feat_freq <- table(dat$feat_freq, dat$result_dumb)
    mosaicplot(t_feat_freq, shade = TRUE, las=2,
               main = "Frequency")
  }
  dev.off() 
}




balloonplot(t(t_sample), main ="Samples", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
balloonplot(t(t_dist), main ="Distance Measurements", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
balloonplot(t(t_feature), main ="Feature", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
balloonplot(t(t_feat_freq), main ="Feature Frequency", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)



dat <- all_results_temp
dat[grep("texts_no_cull", dat[,3]),3] <- "Texts"
dat[grep("small_samp_no_cull", dat[,3]),3] <- "Small"
dat[grep("large_samp_no_cull", dat[,3]),3] <- "Large"
# descriptives through tables
t_sample <- table(dat$sample, dat$result_dumb)
t_dist <- table(dat$dist_meas, dat$result_dumb)
t_feature <- table(dat$feature, dat$result_dumb)
t_feat_freq <- table(dat$feat_freq, dat$result_dumb)

par(mfrow=c(2,2))
mosaicplot(t_dist, shade = TRUE, las=1,
           main = "Distance Measurements")
mosaicplot(t_feature, shade = TRUE, las=1,
           main = "Feature")
mosaicplot(t_feat_freq, shade = TRUE, las=1,
           main = "Feature Frequency")
mosaicplot(t_sample, shade = TRUE, las=1,
           main = "Sample Sizes")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
mosaicplot(t_dist, shade = TRUE, las=1,
           main = "Distance Measurements")
mosaicplot(t_feature, shade = TRUE, las=1,
           main = "Feature")
mosaicplot(t_feat_freq, shade = TRUE, las=1,
           main = "Feature Frequency")
# mosaicplot(t_sample, shade = TRUE, las=1,
#            main = "Sample Sizes")
par(mfrow=c(1,1))

t_sample_1 <- t_sample
t_dist_1 <- t_dist
t_feature_1 <- t_feature
t_feat_freq_1 <- t_feat_freq

assoc(head(t_sample, 5), shade = TRUE, las=3)
assoc(head(t_dist, 5), shade = TRUE, las=3)

# Ok, there seems to be some significant variance in sample distance, a bit less in feat_freq, and not really in feature

# A basic regression equation looks like this

Y = alpha + B1*x1 + B2*x2
# plug in the preferred values for X1 and X2 and you get the predicted value of Y given this model


# a very simple model of your data could look like this:

model1 <- lm(result_dumb ~ factor(sample)+feat_freq, data=dat)
summary(model1)

# the eqaution can be written out as follows:

Y = 4.806e-01 + -4.925e-01(X1=2) + 1.975e-01(X1=3) + 1.808e-04*(X2)

# let's say you'd want to calculate the predicted value of result_dumb (Y), for small_samp_no_cull and 1000 feat_freq
# then just plug in the values in the equation as follows: X1 = 2 and X2 = 1000. 

Y= 4.806e-0 + 1.808e-04 * 1000 + 1.975e-01


# you can now redo this calculation for any model, and any values of the X-variable. One approach could then be to make a frequancy chart and who under what conditions the predictive value of Y is highest. 
# There definately will be a function to calculate everything automatically for you, if you look around. But its also good practice to learn how it works (under the hood so to say)




# this is the model binary logistic model
model1 <- glm(result_dumb ~ factor(sample) + factor(dist_meas) +feat_freq, data=dat, family = binomial)
summary(model1)

