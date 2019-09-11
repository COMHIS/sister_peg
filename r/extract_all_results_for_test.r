#build entire dataframe of results


#dataframe params
batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")
batch_tests <- c("large_samp_no_cull", "texts_no_cull")
batch_tests <- c("small_samp_no_cull")
#batch_tests <- c("small_samp_no_cull")
#batch_tests <- c("large_samp_no_cull")
#batch_tests <- c("texts_no_cull")
char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "3", "4", "5", "6", "7")
tests <- c("delta", "knn", "nb", "nsc", "svm")
#tests <- c("svm")
#tests <- c("delta", "knn", "nb", "nsc", "svm")
#accuracies <- c(100,95,90,85,80,79)
mffeatures <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
#features <- c("w1", "w2", "w3", "c3", "c4", "c5", "c6", "c7")
features <- c("1w", "2w", "3w", "3c", "4c", "5c", "6c", "7c")

results_dir <- "testing_test/ne/"
test_dirs <- c("test_1")
test_dirs <- c("test_2")
test_dirs <- c("test_7")
test_dirs <- c("test_8")
test_dirs <- c("test_9")

#test_dirs <- c("test_9")
#test_dirs <- c("test_2")
test_dirs <- c("test_1", "test_2", "test_7", "test_8", "test_9")
test_dirs <- c("test_1", "test_2")

total_tests <- (length(batch_tests) * length(char_word) * length(tests) * length(mffeatures))

all_results_temp <- data.frame(result=rep(0, total_tests), result_dumb=rep(0, total_tests), sample=rep(0, total_tests), dist_meas=rep(0, total_tests), feature=rep(0, total_tests), feat_freq=rep(0, total_tests))


current_line <- 0

for (test_dir_var in 1:length(test_dirs)) {
  for (i in 1:length(batch_tests)) {
    #get results
    result_files <- list.files(paste0(results_dir, test_dirs[test_dir_var], "/", batch_tests[i], "/"), full.names = TRUE)
    
    #results <- data.frame(is_100=rep(0, length(tests)), is_95=rep(0, length(tests)), is_90=rep(0, length(tests)), is_85=rep(0, length(tests)),
    #                      is_80=rep(0, length(tests)), is_79=rep(0, length(tests)), row.names = tests)
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      
      
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      
      #GET CURRENT DIST_MEAS
      for(z in 1:length(tests)) { 
        if(grepl(tests[z], result_files[x])) {
          #CURRENT DIST_MEAS
          current_test <- tests[z]
        }
      }
      
      #GET FEATURE
      for(z in 1:length(features)) { 
        if(grepl(features[z], result_files[x])) {
          #CURRENT FEATURE
          current_feautres <- features[z]
        }
      }
      
      #FIND RESULT
      temp_results <- grep("%", test$V2)
      for(result_i in 1:length(temp_results)) {
        
        
        current_line <- current_line+1
        
        current_accuracy <- test$V2[temp_results[result_i]]
        current_accuracy <- gsub("[(]", "", current_accuracy)
        current_accuracy <- gsub("[)]", "", current_accuracy)
        #CURRENT_RESULT
        current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
        
        #get result_dumb
        if(current_accuracy == 100) {
          current_dumb <- 1
        } else {
          current_dumb <- 0
        }
        
        #GET FEAT_FREQ
        current_mff <- test$V1[temp_results[result_i]]
        current_mff <- substr(current_mff, 1, 4)
        current_mff <- gsub(" ", "", current_mff, fixed = TRUE)
        current_mff <- as.numeric(current_mff)
        
        all_results_temp[current_line,1] <- current_accuracy
        all_results_temp[current_line,2] <- current_dumb
        all_results_temp[current_line,3] <- batch_tests[i]
        all_results_temp[current_line,4] <- current_test
        all_results_temp[current_line,5] <- current_feautres
        all_results_temp[current_line,6] <- current_mff
          
        
        
        # for (z in 1:length(accuracies)) {
        #   if(current_accuracy == 100) { 
        #     accuracy_group <- current_accuracy
        #     break
        #   } else if (current_accuracy < 80) {
        #     accuracy_group <- 79
        #     break
        #   } else {
        #     if (current_accuracy < accuracies[z-1] && current_accuracy >= accuracies[z]) {
        #       accuracy_group <- accuracies[z]
        #     }
        #   }
        # }
        
        
        
        # total_dataset_results[grep(batch_tests[i], row.names(total_dataset_results)),grep(paste0("is_", accuracy_group), names(total_dataset_results))] <-
        #   total_dataset_results[grep(batch_tests[i], row.names(total_dataset_results)),grep(paste0("is_", accuracy_group), names(total_dataset_results))] + 1
        # results[grep(current_test, row.names(results)),grep(paste0("is_", accuracy_group), names(results))] <-
        #   results[grep(current_test, row.names(results)),grep(paste0("is_", accuracy_group), names(results))] + 1
        # #add to total_model_results table
        # total_model_results[grep(current_test, row.names(total_model_results)),grep(paste0("is_", accuracy_group), names(total_model_results))] <-
        #   total_model_results[grep(current_test, row.names(total_model_results)),grep(paste0("is_", accuracy_group), names(total_model_results))] + 1
        # next
      }
      
    }
  }
  assign(paste0("results.", test_dirs[test_dir_var]), all_results_temp)
}




#FULL DF

total_tests <- (length(batch_tests) * length(char_word) * length(tests) * length(mffeatures))

long_results_temp <- data.frame(result=rep(0, total_tests), result_dumb=rep(0, total_tests), 
                                small_samp=rep(0, total_tests), large_samp=rep(0, total_tests), text_samp=rep(0, total_tests),
                                w1=rep(0, total_tests), w2=rep(0, total_tests), w3=rep(0, total_tests), c3=rep(0, total_tests),
                                c4=rep(0, total_tests), c5=rep(0, total_tests), c6=rep(0, total_tests), c7=rep(0, total_tests),
                                delta=rep(0, total_tests), knn=rep(0, total_tests), nb=rep(0, total_tests), 
                                nsc=rep(0, total_tests), svm=rep(0, total_tests),
                                f100=rep(0, total_tests), f200=rep(0, total_tests), f300=rep(0, total_tests), f400=rep(0, total_tests),
                                f500=rep(0, total_tests), f600=rep(0, total_tests), f700=rep(0, total_tests), f800=rep(0, total_tests),
                                f900=rep(0, total_tests), f1000=rep(0, total_tests))
                                
                               

current_line <- 0

for (test_dir_var in 1:length(test_dirs)) {
  for (i in 1:length(batch_tests)) {
    #get results
    result_files <- list.files(paste0(results_dir, test_dirs[test_dir_var], "/", batch_tests[i], "/"), full.names = TRUE)
    
    #results <- data.frame(is_100=rep(0, length(tests)), is_95=rep(0, length(tests)), is_90=rep(0, length(tests)), is_85=rep(0, length(tests)),
    #                      is_80=rep(0, length(tests)), is_79=rep(0, length(tests)), row.names = tests)
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      
      
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      
      #GET CURRENT DIST_MEAS
      for(z in 1:length(tests)) { 
        if(grepl(tests[z], result_files[x])) {
          #CURRENT DIST_MEAS
          current_test <- tests[z]
        }
      }
      
      #GET FEATURE
      for(z in 1:length(features)) { 
        if(grepl(features[z], result_files[x])) {
          #CURRENT FEATURE
          current_feautres <- features[z]
        }
      }
      
      #FIND RESULT
      temp_results <- grep("%", test$V2)
      for(result_i in 1:length(temp_results)) {
        
        
        current_line <- current_line+1
        
        current_accuracy <- test$V2[temp_results[result_i]]
        current_accuracy <- gsub("[(]", "", current_accuracy)
        current_accuracy <- gsub("[)]", "", current_accuracy)
        #CURRENT_RESULT
        current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
        
        #get result_dumb
        if(current_accuracy == 100) {
          current_dumb <- 1
        } else {
          current_dumb <- 0
        }
        
        #GET FEAT_FREQ
        current_mff <- test$V1[temp_results[result_i]]
        current_mff <- substr(current_mff, 1, 4)
        current_mff <- gsub(" ", "", current_mff, fixed = TRUE)
        current_mff <- as.numeric(current_mff)
        
        
        long_results_temp[current_line,1] <- current_accuracy
        long_results_temp[current_line,2] <- current_dumb
        #assign correct dummy var
        
        if(batch_tests[i] == "small_samp_no_cull") {
          long_results_temp[current_line,3] <- 1
        } else if (batch_tests[i] == "large_samp_no_cull") {
          long_results_temp[current_line,4] <- 1
        } else if (batch_tests[i] == "texts_no_cull") {
          long_results_temp[current_line,5] <- 1
        }
        
        if (current_feautres == "1w") {
          long_results_temp[current_line,6] <- 1
        } else if (current_feautres == "2w") {
          long_results_temp[current_line,7] <- 1
        } else if (current_feautres == "3w") {
          long_results_temp[current_line,8] <- 1
        } else if (current_feautres == "3c") {
          long_results_temp[current_line,9] <- 1
        } else if (current_feautres == "4c") {
          long_results_temp[current_line,10] <- 1
        } else if (current_feautres == "5c") {
          long_results_temp[current_line,11] <- 1
        } else if (current_feautres == "6c") {
          long_results_temp[current_line,12] <- 1
        } else if (current_feautres == "7c") {
          long_results_temp[current_line,13] <- 1
        }  
        
        if (current_test == "delta") {
          long_results_temp[current_line,14] <- 1
        } else if (current_test == "knn") {
          long_results_temp[current_line,15] <- 1
        } else if (current_test == "nb") {
          long_results_temp[current_line,16] <- 1
        } else if (current_test == "nsc") {
          long_results_temp[current_line,17] <- 1
        } else if (current_test == "svm") {
          long_results_temp[current_line,18] <- 1
        }
        
        if (current_mff == 100) {
          long_results_temp[current_line,19] <- 1
        } else if (current_mff == 200) {
          long_results_temp[current_line,20] <- 1
        } else if (current_mff == 300) {
          long_results_temp[current_line,21] <- 1
        } else if (current_mff == 400) {
          long_results_temp[current_line,22] <- 1
        } else if (current_mff == 500) {
          long_results_temp[current_line,23] <- 1
        } else if (current_mff == 600) {
          long_results_temp[current_line,24] <- 1
        } else if (current_mff == 700) {
          long_results_temp[current_line,25] <- 1
        } else if (current_mff == 800) {
          long_results_temp[current_line,26] <- 1
        } else if (current_mff == 900) {
          long_results_temp[current_line,27] <- 1
        } else if (current_mff == 1000) {
          long_results_temp[current_line,28] <- 1
        } 
      }
    }
  }
  assign(paste0("long_results.", test_dirs[test_dir_var]), long_results_temp)
}


test <- lm(result_dumb ~ .-result + .^2, data = long_results_temp)
summary(test)

test <- lm(result_dumb ~ .-result-text_samp-c7-svm-f1000, data = long_results_temp)
summary(test)

test <- lm(result_dumb ~ .-result-small_samp-w1-knn-f100, data = long_results_temp)
summary(test)

temp_df <- as.factor(all_results_temp)

temp_df <- all_results_temp
#temp_df$feat_freq <- as.factor(temp_df$feat_freq)
temp_df[,2] <- as.factor(temp_df[,2])
temp_df[,3] <- as.factor(temp_df[,3])
temp_df[,4] <- as.factor(temp_df[,4])
temp_df[,5] <- as.factor(temp_df[,5])
temp_df[,6] <- as.factor(temp_df[,6])

temp_df$sample <- relevel(temp_df$sample, ref="small_samp_no_cull")
temp_df$sample <- relevel(temp_df$sample, ref="large_samp_no_cull")
temp_df$sample <- relevel(temp_df$sample, ref="texts_no_cull")

length(intersect(which(temp_df$sample == "texts_no_cull"), which(temp_df$result_dumb == 1))) # BEST
length(intersect(which(temp_df$sample == "large_samp_no_cull"), which(temp_df$result_dumb == 1)))
length(intersect(which(temp_df$sample == "small_samp_no_cull"), which(temp_df$result_dumb == 1))) # WORST
temp_df$sample <- relevel(temp_df$sample, ref="small_samp_no_cull")
temp_df$sample <- relevel(temp_df$sample, ref="texts_no_cull")
temp_df$sample <- relevel(temp_df$sample, ref="large_samp_no_cull")

# I WILL WANT TO FIND OUT WHAT PARAMETERS GIVE BEST SMALL SAMP RESULTS

length(intersect(which(temp_df$dist_meas == "delta"), which(temp_df$result_dumb == 1))) # 104
length(intersect(which(temp_df$dist_meas == "knn"), which(temp_df$result_dumb == 1))) # WORST 48
length(intersect(which(temp_df$dist_meas == "nsc"), which(temp_df$result_dumb == 1))) # 118
length(intersect(which(temp_df$dist_meas == "nb"), which(temp_df$result_dumb == 1))) # 118
length(intersect(which(temp_df$dist_meas == "svm"), which(temp_df$result_dumb == 1))) # BEST 190
temp_df$dist_meas <- relevel(temp_df$dist_meas, ref="knn")
temp_df$dist_meas <- relevel(temp_df$dist_meas, ref="nsc")

length(intersect(which(temp_df$feature == "1w"), which(temp_df$result_dumb == 1))) # 73
length(intersect(which(temp_df$feature == "2w"), which(temp_df$result_dumb == 1))) # 83 BEST
length(intersect(which(temp_df$feature == "3w"), which(temp_df$result_dumb == 1))) # 62 WORST
length(intersect(which(temp_df$feature == "3c"), which(temp_df$result_dumb == 1))) # 78 
length(intersect(which(temp_df$feature == "4c"), which(temp_df$result_dumb == 1))) # 72
length(intersect(which(temp_df$feature == "5c"), which(temp_df$result_dumb == 1))) # 69
length(intersect(which(temp_df$feature == "6c"), which(temp_df$result_dumb == 1))) # 64
length(intersect(which(temp_df$feature == "7c"), which(temp_df$result_dumb == 1))) # 77
temp_df$feature <- relevel(temp_df$feature, ref="3w")
temp_df$feature <- relevel(temp_df$feature, ref="4c")

length(intersect(which(temp_df$feat_freq == "100"), which(temp_df$result_dumb == 1))) # 42 WORST
length(intersect(which(temp_df$feat_freq == "200"), which(temp_df$result_dumb == 1))) # 47
length(intersect(which(temp_df$feat_freq == "300"), which(temp_df$result_dumb == 1))) # 57
length(intersect(which(temp_df$feat_freq == "400"), which(temp_df$result_dumb == 1))) # 56
length(intersect(which(temp_df$feat_freq == "500"), which(temp_df$result_dumb == 1))) # 63
length(intersect(which(temp_df$feat_freq == "600"), which(temp_df$result_dumb == 1))) # 61
length(intersect(which(temp_df$feat_freq == "700"), which(temp_df$result_dumb == 1))) # 63
length(intersect(which(temp_df$feat_freq == "800"), which(temp_df$result_dumb == 1))) # 61
length(intersect(which(temp_df$feat_freq == "900"), which(temp_df$result_dumb == 1))) # 63
length(intersect(which(temp_df$feat_freq == "1000"), which(temp_df$result_dumb == 1))) # 65 BEST
temp_df$feat_freq <- relevel(temp_df$feat_freq, ref="100")
temp_df$feat_freq <- relevel(temp_df$feat_freq, ref="400")

temp_df <- temp_df[,-1]

test <- lm(result_dumb ~ ., data = temp_df)
summary(test)

test <- lm(result_dumb ~ . + .^2, data = temp_df)
summary(test)

test <- lm(result_dumb ~ . + .^2 + .^3, data = temp_df)
summary(test)

test <- lm(result_dumb ~ . + .:sample^2 + .:sample^3, data = temp_df)
summary(test)


test <- lm(result_dumb ~ .-result + .^2-result, data = temp_df)
test <- lm(result_dumb ~ . + .^2, data = temp_df)
test <- lm(result ~ .-result_dumb, data = temp_df)
test <- lm(result_dumb ~ dist_meas, data = temp_df)
summary(test)


test <- lm(result_dumb ~ .-result, data = all_results_temp)



test <- lm(result_dumb ~ .-result, data = all_results_temp)
summary(test)

library(dplyr)
long_results_temp <- mutate_if(long_results_temp, is.character, as.factor)
long_results_temp <- mutate_if(long_results_temp, is.numeric, as.factor)
long_results_temp$result_dumb <- as.numeric(long_results_temp$result_dumb)
test <- lm(result_dumb ~ .-result, data = long_results_temp)
summary(test)


