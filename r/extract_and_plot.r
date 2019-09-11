library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)

setwd("")

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")
batch_tests <- c("texts_no_cull", "texts_with_cull")
batch_tests <- c("texts_no_cull")

char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "3", "4", "5", "6", "7")

mffeatures <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

tests <- c("delta", "knn", "nb", "nsc", "svm")
tests <- c("svm")
accuracies <- c(100,95,90,85,80,79)

test_dirs <- c("test_1", "test_2", "test_3")
test_dirs <- c("test_1", "test_2")
test_dirs <- c("test_1")
test_dirs <- c("test_2")
test_dirs <- c("test_6")
test_dirs <- c("test_1", "test_2", "test_3", "test_5", "test_6")

#for (e_ne_var in 1:2) {
for (e_ne_var in 1:1) {
  if (e_ne_var == 1) { results_dir <- "testing_test/ne/" } else { results_dir <- "testing_test/e/" }
  
  total_model_results <- data.frame(hume_100=rep(0, length(tests)), ferg_100=rep(0, length(tests)), hume_95=rep(0, length(tests)),
                                    ferg_95=rep(0, length(tests)), hume_90=rep(0, length(tests)), ferg_90=rep(0, length(tests)),
                                    hume_85=rep(0, length(tests)), ferg_85=rep(0, length(tests)), hume_80=rep(0, length(tests)),
                                    ferg_80=rep(0, length(tests)),  hume_79=rep(0, length(tests)), ferg_79=rep(0, length(tests)), 
                                    row.names = tests)
  
  for (test_dir_var in 1:length(test_dirs)) {
    for (i in 1:length(batch_tests)) {
      #get results  
      result_files <- list.files(paste0(results_dir, test_dirs[test_dir_var], "/", batch_tests[i], "/"), full.names = TRUE)
      results <- data.frame(hume_100=rep(0, length(tests)), ferg_100=rep(0, length(tests)), hume_95=rep(0, length(tests)),
                            ferg_95=rep(0, length(tests)), hume_90=rep(0, length(tests)), ferg_90=rep(0, length(tests)),
                            hume_85=rep(0, length(tests)), ferg_85=rep(0, length(tests)), hume_80=rep(0, length(tests)),
                            ferg_80=rep(0, length(tests)),  hume_79=rep(0, length(tests)), ferg_79=rep(0, length(tests)), 
                            row.names = tests)
      to_test <- c()
      for(z in 1:length(tests)) { 
        to_test <- c(to_test, grep(tests[z], result_files))
      }
      result_files <- result_files[to_test]
      
      
      for (x in 1:length(result_files)) {
        cat("\r", x)
        test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
        
        #remove EB history
        if (any(grepl("EB_History", test$V1))) {
          test <- test[-c(grep("EB_History", test$V1)),]
        }
        
        
        for(z in 1:length(tests)) { 
          if(grepl(tests[z], result_files[x])) {
            current_test <- tests[z]
          }
        }
        
        
        
        
        
        #grepl(tests, result_files[x])
        testing <- TRUE
        test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
        test_cutoffs <- sort(test_cutoffs)
        test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
        while(testing == TRUE) {
          if(length(test_cutoffs) == 0) { 
            testing <- FALSE
            assign(paste0("results.", batch_tests[i]), results)
            next
          }
          temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
          # #remove non sister peg tests
          # if (length(grep("merged", temp_results$V1)) != 0) {
          #   temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
          #   temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
          # }
          # #remove non sister peg tests
          # if (length(grep("Hume_", temp_results$V1)) != 0) {
          #   temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
          # }
          # if (length(grep("Ferguson_", temp_results$V1)) != 0) {
          #   temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
          # }
          #remove non sister peg tests
          if (length(grep("Hume", temp_results$V1)) != 0) {
            temp_results <- temp_results[-c(grep("Hume", temp_results$V1)),]
          }
          if (length(grep("Ferg", temp_results$V1)) != 0) {
            temp_results <- temp_results[-c(grep("Ferg", temp_results$V1)),]
          }
          current_accuracy <- test$V2[test_cutoffs[2]+1]
          current_accuracy <- gsub("[(]", "", current_accuracy)
          current_accuracy <- gsub("[)]", "", current_accuracy)
          current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
          
          for (z in 1:length(accuracies)) {
            if(current_accuracy == 100) { 
              accuracy_group <- current_accuracy
              break
            } else if (current_accuracy < 80) {
              accuracy_group <- 79
              break
            } else {
              if (current_accuracy < accuracies[z-1] && current_accuracy >= accuracies[z]) {
                accuracy_group <- accuracies[z]
              }
            }
          }
          
          results[grep(current_test, row.names(results)),grep(paste0("hume_", accuracy_group), names(results))] <-
            results[grep(current_test, row.names(results)),grep(paste0("hume_", accuracy_group), names(results))] + length(grep("Hume", temp_results$V3))
          results[grep(current_test, row.names(results)),grep(paste0("ferg_", accuracy_group), names(results))] <-
            results[grep(current_test, row.names(results)),grep(paste0("ferg_", accuracy_group), names(results))] + length(grep("Ferg", temp_results$V3))
          test_cutoffs <- test_cutoffs[-c(1:2)]
          #add to total_model_results table
          total_model_results[grep(current_test, row.names(total_model_results)),grep(paste0("hume_", accuracy_group), names(total_model_results))] <-
            total_model_results[grep(current_test, row.names(total_model_results)),grep(paste0("hume_", accuracy_group), names(total_model_results))] + length(grep("Hume", temp_results$V3))
          total_model_results[grep(current_test, row.names(total_model_results)),grep(paste0("ferg_", accuracy_group), names(total_model_results))] <-
            total_model_results[grep(current_test, row.names(total_model_results)),grep(paste0("ferg_", accuracy_group), names(total_model_results))] + length(grep("Ferg", temp_results$V3))
          next
        }
      }
    }
  }
  
  if (e_ne_var == 1) { ne_model_results <- total_model_results } else { e_model_results <- total_model_results }
  
  ######################
  #create and load test by features
  
  for (i in 1:length(char_word)) {
    temp_results <- data.frame(hume_100=rep(0, length(tests)), ferg_100=rep(0, length(tests)), hume_95=rep(0, length(tests)),
                               ferg_95=rep(0, length(tests)), hume_90=rep(0, length(tests)), ferg_90=rep(0, length(tests)),
                               hume_85=rep(0, length(tests)), ferg_85=rep(0, length(tests)), hume_80=rep(0, length(tests)),
                               ferg_80=rep(0, length(tests)),  hume_79=rep(0, length(tests)), ferg_79=rep(0, length(tests)), 
                               row.names = tests) 
    assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
  }
  
  features <- c("w1", "w2", "w3", "c3", "c4", "c5", "c6", "c7")
  total_feature_results <- data.frame(hume_100=rep(0, length(features)), ferg_100=rep(0, length(features)), hume_95=rep(0, length(features)),
                                      ferg_95=rep(0, length(features)), hume_90=rep(0, length(features)), ferg_90=rep(0, length(features)),
                                      hume_85=rep(0, length(features)), ferg_85=rep(0, length(features)), hume_80=rep(0, length(features)),
                                      ferg_80=rep(0, length(features)),  hume_79=rep(0, length(features)), ferg_79=rep(0, length(features)),
                                      row.names = features)
  
  
  
  for (i in 1:length(batch_tests)) {
    #get results  
    result_files <- list.files(paste0(results_dir, test_dirs[test_dir_var], "/", batch_tests[i], "/"), full.names = TRUE)
    for (x in 1:length(features)) {
      temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
      cat("\r", x)
      file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
      for (y in 1:length(file_locs)) {
        test <- read.delim(result_files[file_locs[y]], header = FALSE, stringsAsFactors = FALSE)
        
        #remove EB history
        if (any(grepl("EB_History", test$V1))) {
          test <- test[-c(grep("EB_History", test$V1)),]
        }
        
        for(z in 1:length(tests)) { 
          if(grepl(tests[z], result_files[file_locs[y]])) {
            current_test <- tests[z]
          }
        }
        # for(z in 1:length(tests)) { 
        #   if(grepl(tests[z], result_files[file_locs[i]])) {
        #     current_test <- tests[z]
        #   }
        # }
        #grepl(tests, result_files[x])
        testing <- TRUE
        test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
        test_cutoffs <- sort(test_cutoffs)
        test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
        while(testing == TRUE) {
          if(length(test_cutoffs) == 0) { 
            testing <- FALSE 
            next
          }
          temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
          # #remove non sister peg tests
          # if (length(grep("merged", temp_results$V1)) != 0) {
          #   temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
          #   temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
          # }
          # #remove non sister peg tests
          # if (length(grep("Hume_", temp_results$V1)) != 0) {
          #   temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
          # }
          # if (length(grep("Ferguson_", temp_results$V1)) != 0) {
          #   temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
          # }
          #remove non sister peg tests
          if (length(grep("Hume", temp_results$V1)) != 0) {
            temp_results <- temp_results[-c(grep("Hume", temp_results$V1)),]
          }
          if (length(grep("Ferg", temp_results$V1)) != 0) {
            temp_results <- temp_results[-c(grep("Ferg", temp_results$V1)),]
          }
          current_accuracy <- test$V2[test_cutoffs[2]+1]
          current_accuracy <- gsub("[(]", "", current_accuracy)
          current_accuracy <- gsub("[)]", "", current_accuracy)
          current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
          
          for (z in 1:length(accuracies)) {
            if(current_accuracy == 100) { 
              accuracy_group <- current_accuracy
              break
            } else if (current_accuracy < 80) {
              accuracy_group <- 79
              break
            } else {
              if (current_accuracy < accuracies[z-1] && current_accuracy >= accuracies[z]) {
                accuracy_group <- accuracies[z]
              }
            }
          }
          
          temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] <-
            temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] + length(grep("Hume", temp_results$V3))
          
          temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] <-
            temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] + length(grep("Ferg", temp_results$V3))
          test_cutoffs <- test_cutoffs[-c(1:2)]
          
          assign(paste0(char_word[x], tok_num[x], "_results"), temp_total_results)
          
          
          total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("hume_", accuracy_group), names(total_feature_results))] <-
            total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("hume_", accuracy_group), names(total_feature_results))] + length(grep("Hume", temp_results$V3))
          
          total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("ferg_", accuracy_group), names(total_feature_results))] <-
            total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("ferg_", accuracy_group), names(total_feature_results))] + length(grep("Ferg", temp_results$V3))
          next
        }
      }
    }
  }
  
  if (e_ne_var == 1) { ne_feature_results <- total_feature_results } else { e_feature_results <- total_feature_results }
  
  ######################
  #create and load test by MFW results
  
  # for (i in 1:length(char_word)) {
  #   temp_results <- data.frame(hume_100=rep(0, 10), ferg_100=rep(0, 10), hume_95=rep(0, 10), ferg_95=rep(0, 10), 
  #                              hume_90=rep(0, 10), ferg_90=rep(0, 10), hume_85=rep(0, 10), ferg_85=rep(0, 10), 
  #                              hume_80=rep(0, 10), ferg_80=rep(0, 10),  hume_79=rep(0, 10), ferg_79=rep(0, 10), 
  #                              row.names = mffeatures) 
  #   assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
  # }
  
  
  
  
  total_mfw_results <- data.frame(hume_100=rep(0, 10), ferg_100=rep(0, 10), hume_95=rep(0, 10), ferg_95=rep(0, 10), 
                                  hume_90=rep(0, 10), ferg_90=rep(0, 10), hume_85=rep(0, 10), ferg_85=rep(0, 10), 
                                  hume_80=rep(0, 10), ferg_80=rep(0, 10),  hume_79=rep(0, 10), ferg_79=rep(0, 10), 
                                  row.names = mffeatures)
  
  for (i in 1:length(batch_tests)) {
    #get results  
    result_files <- list.files(paste0(results_dir, test_dirs[test_dir_var], "/", batch_tests[i], "/"), full.names = TRUE)
    for (x in 1:length(result_files)) {
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      
      #remove EB history
      if (any(grepl("EB_History", test$V1))) {
        test <- test[-c(grep("EB_History", test$V1)),]
      }
      
      testing <- TRUE
      mfw_tests <- grep("MFW", test$V1)
      mfw_tests <- mfw_tests[-length(mfw_tests)]
      test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
      test_cutoffs <- sort(test_cutoffs)
      test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
      while(testing == TRUE) {
        if(length(test_cutoffs) == 0) { 
          testing <- FALSE 
          next
        }
        temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
        #remove non sister peg tests
        # if (length(grep("merged", temp_results$V1)) != 0) {
        #   temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
        #   temp_results <- temp_results[-c(grep("Ferg_", temp_results$V1)),]
        # }
        #remove non sister peg tests
        if (length(grep("Hume", temp_results$V1)) != 0) {
          temp_results <- temp_results[-c(grep("Hume", temp_results$V1)),]
        }
        if (length(grep("Ferg", temp_results$V1)) != 0) {
          temp_results <- temp_results[-c(grep("Ferg", temp_results$V1)),]
        }
        current_accuracy <- test$V2[test_cutoffs[2]+1]
        current_accuracy <- gsub("[(]", "", current_accuracy)
        current_accuracy <- gsub("[)]", "", current_accuracy)
        current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
        current_mfw <- test$V1[test_cutoffs[2]+1]
        current_mfw <- gsub(" MFW ,.*", "", current_mfw)
        
        for (z in 1:length(accuracies)) {
          if(current_accuracy == 100) { 
            accuracy_group <- current_accuracy
            break
          } else if (current_accuracy < 80) {
            accuracy_group <- 79
            break
          } else {
            if (current_accuracy < accuracies[z-1] && current_accuracy >= accuracies[z]) {
              accuracy_group <- accuracies[z]
            }
          }
        }
        
        # temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] <-
        #   temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] + length(grep("Hume", temp_results$V3))
        # 
        # temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] <-
        #   temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] + length(grep("Ferg", temp_results$V3))
        # test_cutoffs <- test_cutoffs[-c(1:2)]
        # 
        # assign(paste0(char_word[x], tok_num[x], "_results"), temp_total_results)
        
        
        total_mfw_results[grep(paste0("^", current_mfw, "$"), row.names(total_mfw_results)),grep(paste0("hume_", accuracy_group), names(total_mfw_results))] <-
          total_mfw_results[grep(paste0("^", current_mfw, "$"), row.names(total_mfw_results)),grep(paste0("hume_", accuracy_group), names(total_mfw_results))] + length(grep("Hume", temp_results$V3))
        
        total_mfw_results[grep(paste0("^", current_mfw, "$"), row.names(total_mfw_results)),grep(paste0("ferg_", accuracy_group), names(total_mfw_results))] <-
          total_mfw_results[grep(paste0("^", current_mfw, "$"), row.names(total_mfw_results)),grep(paste0("ferg_", accuracy_group), names(total_mfw_results))] + length(grep("Ferg", temp_results$V3))
        test_cutoffs <- test_cutoffs[-c(1:2)]
        next
        
      }
    }
  }
  if (e_ne_var == 1) { ne_mfw_results <- total_mfw_results } else { e_mfw_results <- total_mfw_results }
  
  ######################
  #create and load test by datasets
  
  
  total_dataset_results <- data.frame(hume_100=rep(0, length(batch_tests)), ferg_100=rep(0, length(batch_tests)), hume_95=rep(0, length(batch_tests)), ferg_95=rep(0, length(batch_tests)),
                                      hume_90=rep(0, length(batch_tests)), ferg_90=rep(0, length(batch_tests)), hume_85=rep(0, length(batch_tests)), ferg_85=rep(0, length(batch_tests)),
                                      hume_80=rep(0, length(batch_tests)), ferg_80=rep(0, length(batch_tests)),  hume_79=rep(0, length(batch_tests)), ferg_79=rep(0, length(batch_tests)),
                                      row.names = batch_tests)
  
  # total_dataset_results <- data.frame(hume_100=rep(0, 6), ferg_100=rep(0, 6), hume_95=rep(0, 6), ferg_95=rep(0, 6), 
  #                                     hume_90=rep(0, 6), ferg_90=rep(0, 6), hume_85=rep(0, 6), ferg_85=rep(0, 6), 
  #                                     hume_80=rep(0, 6), ferg_80=rep(0, 6),  hume_79=rep(0, 6), ferg_79=rep(0, 6), 
  #                                     row.names = batch_tests)
  # 
  # total_dataset_results <- data.frame(hume_100=rep(0, 3), ferg_100=rep(0, 3), hume_95=rep(0, 3), ferg_95=rep(0, 3), 
  #                                     hume_90=rep(0, 3), ferg_90=rep(0, 3), hume_85=rep(0, 3), ferg_85=rep(0, 3), 
  #                                     hume_80=rep(0, 3), ferg_80=rep(0, 3),  hume_79=rep(0, 3), ferg_79=rep(0, 3), 
  #                                     row.names = batch_tests)
  # 
  
  for (i in 1:length(batch_tests)) {
    #get results  
    temp_results <- get(paste0("results.", batch_tests[i]))
    total_dataset_results$hume_100[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$hume_100)
    total_dataset_results$ferg_100[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$ferg_100)
    total_dataset_results$hume_95[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$hume_95)
    total_dataset_results$ferg_95[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$ferg_95)
    total_dataset_results$hume_90[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$hume_90)
    total_dataset_results$ferg_90[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$ferg_90)
    total_dataset_results$hume_85[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$hume_85)
    total_dataset_results$ferg_85[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$ferg_85)
    total_dataset_results$hume_80[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$hume_80)
    total_dataset_results$ferg_80[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$ferg_80)
    total_dataset_results$hume_79[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$hume_79)
    total_dataset_results$ferg_79[grep(batch_tests[i], row.names(total_dataset_results))] <- sum(temp_results$ferg_79)
  }
  if (e_ne_var == 1) { ne_dataset_results <- total_dataset_results } else { e_dataset_results <- total_dataset_results }
}


##########################################################
#plot results
##########################################################

#make df

model_df <- data.frame(
  attributions = c(sum(total_model_results[5,1], total_model_results[5,2]), sum(total_model_results[5,3], total_model_results[5,4]), 
                   sum(total_model_results[5,5], total_model_results[5,6]), sum(total_model_results[5,7], total_model_results[5,8]), 
                   sum(total_model_results[5,9], total_model_results[5,10]), sum(total_model_results[5,11], total_model_results[5,12])), Classification = as.character("SVM"),
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[1,1], total_model_results[1,2]), sum(total_model_results[1,3], total_model_results[1,4]), 
                                              sum(total_model_results[1,5], total_model_results[1,6]), sum(total_model_results[1,7], total_model_results[1,8]), 
                                              sum(total_model_results[1,9], total_model_results[1,10]), sum(total_model_results[1,11], total_model_results[1,12])), 
                             Classification = as.character("Delta"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[2,1], total_model_results[2,2]), sum(total_model_results[2,3], total_model_results[2,4]), 
                                              sum(total_model_results[2,5], total_model_results[2,6]), sum(total_model_results[2,7], total_model_results[2,8]), 
                                              sum(total_model_results[2,9], total_model_results[2,10]), sum(total_model_results[2,11], total_model_results[2,12])), 
                             Classification = as.character("KNN"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[3,1], total_model_results[3,2]), sum(total_model_results[3,3], total_model_results[3,4]), 
                                              sum(total_model_results[3,5], total_model_results[3,6]), sum(total_model_results[3,7], total_model_results[3,8]), 
                                              sum(total_model_results[3,9], total_model_results[3,10]), sum(total_model_results[3,11], total_model_results[3,12])), 
                             Classification = as.character("NB"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[4,1], total_model_results[4,2]), sum(total_model_results[4,3], total_model_results[4,4]), 
                                              sum(total_model_results[4,5], total_model_results[4,6]), sum(total_model_results[4,7], total_model_results[4,8]), 
                                              sum(total_model_results[4,9], total_model_results[4,10]), sum(total_model_results[4,11], total_model_results[4,12])), 
                             Classification = as.character("NSC"),
                             accuracy = c(100,95,90,85,80,79)))




ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Classification)) +
  geom_line(aes(colour = Classification), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  scale_fill_discrete(name = "New Legend Title")

classification_model_df <- model_df


############################################################
#ggplot of feature results
############################################################
# 
# # add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_feature_results %>% add_rownames(), 
#                     test2_total_feature_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_feature_results
# 
# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))

#make df

model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
                   sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
                   sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
  Feature = as.character("4C"),
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Feature = as.character("1W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Feature = as.character("2W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                                              sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                                              sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
                             Feature = as.character("3W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[4,1], total_feature_results_merged[4,2]), sum(total_feature_results_merged[4,3], total_feature_results_merged[4,4]), 
                                              sum(total_feature_results_merged[4,5], total_feature_results_merged[4,6]), sum(total_feature_results_merged[4,7], total_feature_results_merged[4,8]), 
                                              sum(total_feature_results_merged[4,9], total_feature_results_merged[4,10]), sum(total_feature_results_merged[4,11], total_feature_results_merged[4,12])), 
                             Feature = as.character("3C"),
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[6,1], total_feature_results_merged[6,2]), sum(total_feature_results_merged[6,3], total_feature_results_merged[6,4]), 
                                              sum(total_feature_results_merged[6,5], total_feature_results_merged[6,6]), sum(total_feature_results_merged[6,7], total_feature_results_merged[6,8]), 
                                              sum(total_feature_results_merged[6,9], total_feature_results_merged[6,10]), sum(total_feature_results_merged[6,11], total_feature_results_merged[6,12])), 
                             Feature = as.character("5C"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[7,1], total_feature_results_merged[7,2]), sum(total_feature_results_merged[7,3], total_feature_results_merged[7,4]), 
                                              sum(total_feature_results_merged[7,5], total_feature_results_merged[7,6]), sum(total_feature_results_merged[7,7], total_feature_results_merged[7,8]), 
                                              sum(total_feature_results_merged[7,9], total_feature_results_merged[7,10]), sum(total_feature_results_merged[7,11], total_feature_results_merged[7,12])), 
                             Feature = as.character("6C"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[8,1], total_feature_results_merged[8,2]), sum(total_feature_results_merged[8,3], total_feature_results_merged[8,4]), 
                                              sum(total_feature_results_merged[8,5], total_feature_results_merged[8,6]), sum(total_feature_results_merged[8,7], total_feature_results_merged[8,8]), 
                                              sum(total_feature_results_merged[8,9], total_feature_results_merged[8,10]), sum(total_feature_results_merged[8,11], total_feature_results_merged[8,12])), 
                             Feature = as.character("7C"),
                             accuracy = c(100,95,90,85,80,79)))


# temp <- model_df
# model_df <- temp
# 
# model_df <- temp[-(c(37:54)),]
# model_df <- temp[-(c(1:36)),]
# 
ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Feature)) +
  geom_line(aes(colour = Feature), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") 



feature_model_df <- model_df




############################################################
#ggplot of frequent results
############################################################

# # add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_mfw_results %>% add_rownames(), 
#                     test2_total_mfw_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_mfw_results

# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))

#make df

model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
                   sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
                   sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
  Frequency = 500,
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Frequency = 100,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Frequency = 200,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                                              sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                                              sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
                             Frequency = 300,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[4,1], total_feature_results_merged[4,2]), sum(total_feature_results_merged[4,3], total_feature_results_merged[4,4]), 
                                              sum(total_feature_results_merged[4,5], total_feature_results_merged[4,6]), sum(total_feature_results_merged[4,7], total_feature_results_merged[4,8]), 
                                              sum(total_feature_results_merged[4,9], total_feature_results_merged[4,10]), sum(total_feature_results_merged[4,11], total_feature_results_merged[4,12])), 
                             Frequency = 400,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[6,1], total_feature_results_merged[6,2]), sum(total_feature_results_merged[6,3], total_feature_results_merged[6,4]), 
                                              sum(total_feature_results_merged[6,5], total_feature_results_merged[6,6]), sum(total_feature_results_merged[6,7], total_feature_results_merged[6,8]), 
                                              sum(total_feature_results_merged[6,9], total_feature_results_merged[6,10]), sum(total_feature_results_merged[6,11], total_feature_results_merged[6,12])), 
                             Frequency = 600,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[7,1], total_feature_results_merged[7,2]), sum(total_feature_results_merged[7,3], total_feature_results_merged[7,4]), 
                                              sum(total_feature_results_merged[7,5], total_feature_results_merged[7,6]), sum(total_feature_results_merged[7,7], total_feature_results_merged[7,8]), 
                                              sum(total_feature_results_merged[7,9], total_feature_results_merged[7,10]), sum(total_feature_results_merged[7,11], total_feature_results_merged[7,12])), 
                             Frequency = 700,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[8,1], total_feature_results_merged[8,2]), sum(total_feature_results_merged[8,3], total_feature_results_merged[8,4]), 
                                              sum(total_feature_results_merged[8,5], total_feature_results_merged[8,6]), sum(total_feature_results_merged[8,7], total_feature_results_merged[8,8]), 
                                              sum(total_feature_results_merged[8,9], total_feature_results_merged[8,10]), sum(total_feature_results_merged[8,11], total_feature_results_merged[8,12])), 
                             Frequency = 800,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[9,1], total_feature_results_merged[9,2]), sum(total_feature_results_merged[9,3], total_feature_results_merged[9,4]), 
                                              sum(total_feature_results_merged[9,5], total_feature_results_merged[9,6]), sum(total_feature_results_merged[9,7], total_feature_results_merged[9,8]), 
                                              sum(total_feature_results_merged[9,9], total_feature_results_merged[9,10]), sum(total_feature_results_merged[9,11], total_feature_results_merged[9,12])), 
                             Frequency = 900,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[10,1], total_feature_results_merged[10,2]), sum(total_feature_results_merged[10,3], total_feature_results_merged[10,4]), 
                                              sum(total_feature_results_merged[10,5], total_feature_results_merged[10,6]), sum(total_feature_results_merged[10,7], total_feature_results_merged[10,8]), 
                                              sum(total_feature_results_merged[10,9], total_feature_results_merged[10,10]), sum(total_feature_results_merged[10,11], total_feature_results_merged[10,12])), 
                             Frequency = 1000,
                             accuracy = c(100,95,90,85,80,79)))

model_df$Frequency <- as.factor(model_df$Frequency)


ggplot(data = model_df, aes(x=accuracy, y=attributions, Frequency)) +
  geom_line(aes(colour = Frequency), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") 

frequency_model_df <- model_df


############################################################
#ggplot of dataset results
############################################################

# add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_dataset_results %>% add_rownames(), 
#                     test2_total_dataset_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_dataset_results

# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))


#####################################################
# TO NORMALIZE THE RESULTS
#####################################################

total_tests_large <- 3 * 10 * 45 * 2
total_tests_small <- 12 * 10 * 45 * 2
texts <- 1 * 10 * 45 * 2


#######################

for (i in 1:nrow(total_feature_results_merged)) {
  if (i == 1) {
    total <- total_tests_large
  } else if (i == 2) {
    total <- total_tests_small
  } else if (i == 3) {
    total <- texts
  }
  for (x in 1:ncol(total_feature_results_merged)) {
    total_feature_results_merged[i,x] <- total_feature_results_merged[i,x] / total * 100
  }
  
}

#####################################################


# #make df - FOR SIX DATASETS
# model_df <- data.frame(
#   attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
#                    sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
#                    sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
#   Dataset = as.character("Texts"),
#   accuracy = c(100,95,90,85,80,79))
# 
# model_df <- rbind(model_df,
#                   data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
#                                               sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
#                                               sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
#                              Dataset = as.character("Large samples"),
#                              accuracy = c(100,95,90,85,80,79)))
# model_df <- rbind(model_df,
#                   data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
#                                               sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
#                                               sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
#                              Dataset = as.character("Small samples"),
#                              accuracy = c(100,95,90,85,80,79)))
# 
# dataset_model_df <- model_df

#FOR THREE DATASETS
model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                   sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                   sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
  Dataset = as.character("Texts"),
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Dataset = as.character("Large samples"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Dataset = as.character("Small samples"),
                             accuracy = c(100,95,90,85,80,79)))

ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Dataset)) +
  geom_line(aes(colour = Dataset), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions %") 

dataset_model_df <- model_df

# ##############PLOTTING
# 
# p1<-ggplot(data = classification_model_df, aes(x=accuracy, y=attributions, group=Classification)) +
#   geom_line(aes(colour = Classification), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") +
#   scale_fill_discrete(name = "New Legend Title") +
#   theme(axis.title.x=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# p2<-ggplot(data = feature_model_df, aes(x=accuracy, y=attributions, group=Feature)) +
#   geom_line(aes(colour = Feature), size = 1) +
#   #xlab("Accuracy %") +
#   #ylab("Correct attributions") +
#   theme(axis.title.x=element_blank(),
#         #axis.text.x=element_blank(),
#         #axis.ticks.x=element_blank(),
#         axis.title.y=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# p3<-ggplot(data = frequency_model_df, aes(x=accuracy, y=attributions, Frequency)) +
#   geom_line(aes(colour = Frequency), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") +
#   theme(axis.title.y=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# 
# p4<-ggplot(data = dataset_model_df, aes(x=accuracy, y=attributions, group=Dataset)) +
#   geom_line(aes(colour = Dataset), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") 
# 
# 
# 
# grid.arrange(p1, p2, p4, p3, nrow = 2)
# 

##############PLOTTING

p1 <- classification_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Classification), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Classification, colour = Classification)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())

p2 <- feature_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Feature), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Feature, colour = Feature)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())


p3 <- frequency_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Frequency), NA_character_)) %>%
  # mutate(label = if_else(((accuracy == max(accuracy) & attributions == 486) | 
  #                           (accuracy == max(accuracy) & attributions == 465) |
  #                           (accuracy == max(accuracy) & attributions == 429) |
  #                           (accuracy == max(accuracy) & attributions == 173) |
  #                           (accuracy == max(accuracy) & attributions == 291) |
  #                           (accuracy == max(accuracy) & attributions == 334) )
  #                        
  #                       , as.character(Frequency), NA_character_)) %>%
  #mutate(label = if_else(attributions == c(486, 465, 429, 173, 291, 384), as.character(Frequency), NA_character_)) %>%
  
  ggplot(aes(x = accuracy, y = attributions, group = Frequency, colour = Frequency)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())

p4 <- dataset_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Dataset), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Dataset, colour = Dataset)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions as %") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


grid.arrange(p1, p2, p4, p3, nrow = 2)


##########################################################
##########################################################
##########################################################

##########################################################
#############################################################
##################################################################
########################################################
#################################################
##########################################################
#plot results as percentage of 100% correct
##########################################################

#make df

model_df <- data.frame(
  attributions = c(sum(total_model_results[5,1], total_model_results[5,2]), sum(total_model_results[5,3], total_model_results[5,4]), 
                   sum(total_model_results[5,5], total_model_results[5,6]), sum(total_model_results[5,7], total_model_results[5,8]), 
                   sum(total_model_results[5,9], total_model_results[5,10]), sum(total_model_results[5,11], total_model_results[5,12])), Classification = as.character("SVM"),
  accuracy = c(100,95,90,85,80,79))


model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[1,1], total_model_results[1,2]), sum(total_model_results[1,3], total_model_results[1,4]), 
                                              sum(total_model_results[1,5], total_model_results[1,6]), sum(total_model_results[1,7], total_model_results[1,8]), 
                                              sum(total_model_results[1,9], total_model_results[1,10]), sum(total_model_results[1,11], total_model_results[1,12])), 
                             Classification = as.character("Delta"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[2,1], total_model_results[2,2]), sum(total_model_results[2,3], total_model_results[2,4]), 
                                              sum(total_model_results[2,5], total_model_results[2,6]), sum(total_model_results[2,7], total_model_results[2,8]), 
                                              sum(total_model_results[2,9], total_model_results[2,10]), sum(total_model_results[2,11], total_model_results[2,12])), 
                             Classification = as.character("KNN"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[3,1], total_model_results[3,2]), sum(total_model_results[3,3], total_model_results[3,4]), 
                                              sum(total_model_results[3,5], total_model_results[3,6]), sum(total_model_results[3,7], total_model_results[3,8]), 
                                              sum(total_model_results[3,9], total_model_results[3,10]), sum(total_model_results[3,11], total_model_results[3,12])), 
                             Classification = as.character("NB"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[4,1], total_model_results[4,2]), sum(total_model_results[4,3], total_model_results[4,4]), 
                                              sum(total_model_results[4,5], total_model_results[4,6]), sum(total_model_results[4,7], total_model_results[4,8]), 
                                              sum(total_model_results[4,9], total_model_results[4,10]), sum(total_model_results[4,11], total_model_results[4,12])), 
                             Classification = as.character("NSC"),
                             accuracy = c(100,95,90,85,80,79)))

temp_model_df <- model_df
for(i in 1:nrow(model_df)) {
  temp_model_df$attributions[i] <- model_df$attributions[i] / sum(model_df$attributions[which(model_df$accuracy == model_df$accuracy[i])]) * 100
}
model_df <- temp_model_df

model_df <- model_df[-c(which(model_df$accuracy == 79)),]
model_df <- model_df[-c(which(model_df$accuracy == 80)),]

ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Classification)) +
  geom_line(aes(colour = Classification), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  scale_fill_discrete(name = "New Legend Title")

classification_model_df <- model_df


############################################################
#ggplot of feature results
############################################################
# 
# # add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_feature_results %>% add_rownames(), 
#                     test2_total_feature_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_feature_results
# 
# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))

#make df

model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
                   sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
                   sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
  Feature = as.character("4C"),
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Feature = as.character("1W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Feature = as.character("2W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                                              sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                                              sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
                             Feature = as.character("3W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[4,1], total_feature_results_merged[4,2]), sum(total_feature_results_merged[4,3], total_feature_results_merged[4,4]), 
                                              sum(total_feature_results_merged[4,5], total_feature_results_merged[4,6]), sum(total_feature_results_merged[4,7], total_feature_results_merged[4,8]), 
                                              sum(total_feature_results_merged[4,9], total_feature_results_merged[4,10]), sum(total_feature_results_merged[4,11], total_feature_results_merged[4,12])), 
                             Feature = as.character("3C"),
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[6,1], total_feature_results_merged[6,2]), sum(total_feature_results_merged[6,3], total_feature_results_merged[6,4]), 
                                              sum(total_feature_results_merged[6,5], total_feature_results_merged[6,6]), sum(total_feature_results_merged[6,7], total_feature_results_merged[6,8]), 
                                              sum(total_feature_results_merged[6,9], total_feature_results_merged[6,10]), sum(total_feature_results_merged[6,11], total_feature_results_merged[6,12])), 
                             Feature = as.character("5C"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[7,1], total_feature_results_merged[7,2]), sum(total_feature_results_merged[7,3], total_feature_results_merged[7,4]), 
                                              sum(total_feature_results_merged[7,5], total_feature_results_merged[7,6]), sum(total_feature_results_merged[7,7], total_feature_results_merged[7,8]), 
                                              sum(total_feature_results_merged[7,9], total_feature_results_merged[7,10]), sum(total_feature_results_merged[7,11], total_feature_results_merged[7,12])), 
                             Feature = as.character("6C"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[8,1], total_feature_results_merged[8,2]), sum(total_feature_results_merged[8,3], total_feature_results_merged[8,4]), 
                                              sum(total_feature_results_merged[8,5], total_feature_results_merged[8,6]), sum(total_feature_results_merged[8,7], total_feature_results_merged[8,8]), 
                                              sum(total_feature_results_merged[8,9], total_feature_results_merged[8,10]), sum(total_feature_results_merged[8,11], total_feature_results_merged[8,12])), 
                             Feature = as.character("7C"),
                             accuracy = c(100,95,90,85,80,79)))


temp_model_df <- model_df
for(i in 1:nrow(model_df)) {
  temp_model_df$attributions[i] <- model_df$attributions[i] / sum(model_df$attributions[which(model_df$accuracy == model_df$accuracy[i])]) * 100
}
model_df <- temp_model_df

model_df <- model_df[-c(which(model_df$accuracy == 79)),]
model_df <- model_df[-c(which(model_df$accuracy == 80)),]

# temp <- model_df
# model_df <- temp
# 
# model_df <- temp[-(c(37:54)),]
# model_df <- temp[-(c(1:36)),]
# 
ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Feature)) +
  geom_line(aes(colour = Feature), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") 



feature_model_df <- model_df




############################################################
#ggplot of frequent results
############################################################

# # add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_mfw_results %>% add_rownames(), 
#                     test2_total_mfw_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_mfw_results

# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))

#make df

model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
                   sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
                   sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
  Frequency = 500,
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Frequency = 100,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Frequency = 200,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                                              sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                                              sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
                             Frequency = 300,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[4,1], total_feature_results_merged[4,2]), sum(total_feature_results_merged[4,3], total_feature_results_merged[4,4]), 
                                              sum(total_feature_results_merged[4,5], total_feature_results_merged[4,6]), sum(total_feature_results_merged[4,7], total_feature_results_merged[4,8]), 
                                              sum(total_feature_results_merged[4,9], total_feature_results_merged[4,10]), sum(total_feature_results_merged[4,11], total_feature_results_merged[4,12])), 
                             Frequency = 400,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[6,1], total_feature_results_merged[6,2]), sum(total_feature_results_merged[6,3], total_feature_results_merged[6,4]), 
                                              sum(total_feature_results_merged[6,5], total_feature_results_merged[6,6]), sum(total_feature_results_merged[6,7], total_feature_results_merged[6,8]), 
                                              sum(total_feature_results_merged[6,9], total_feature_results_merged[6,10]), sum(total_feature_results_merged[6,11], total_feature_results_merged[6,12])), 
                             Frequency = 600,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[7,1], total_feature_results_merged[7,2]), sum(total_feature_results_merged[7,3], total_feature_results_merged[7,4]), 
                                              sum(total_feature_results_merged[7,5], total_feature_results_merged[7,6]), sum(total_feature_results_merged[7,7], total_feature_results_merged[7,8]), 
                                              sum(total_feature_results_merged[7,9], total_feature_results_merged[7,10]), sum(total_feature_results_merged[7,11], total_feature_results_merged[7,12])), 
                             Frequency = 700,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[8,1], total_feature_results_merged[8,2]), sum(total_feature_results_merged[8,3], total_feature_results_merged[8,4]), 
                                              sum(total_feature_results_merged[8,5], total_feature_results_merged[8,6]), sum(total_feature_results_merged[8,7], total_feature_results_merged[8,8]), 
                                              sum(total_feature_results_merged[8,9], total_feature_results_merged[8,10]), sum(total_feature_results_merged[8,11], total_feature_results_merged[8,12])), 
                             Frequency = 800,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[9,1], total_feature_results_merged[9,2]), sum(total_feature_results_merged[9,3], total_feature_results_merged[9,4]), 
                                              sum(total_feature_results_merged[9,5], total_feature_results_merged[9,6]), sum(total_feature_results_merged[9,7], total_feature_results_merged[9,8]), 
                                              sum(total_feature_results_merged[9,9], total_feature_results_merged[9,10]), sum(total_feature_results_merged[9,11], total_feature_results_merged[9,12])), 
                             Frequency = 900,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[10,1], total_feature_results_merged[10,2]), sum(total_feature_results_merged[10,3], total_feature_results_merged[10,4]), 
                                              sum(total_feature_results_merged[10,5], total_feature_results_merged[10,6]), sum(total_feature_results_merged[10,7], total_feature_results_merged[10,8]), 
                                              sum(total_feature_results_merged[10,9], total_feature_results_merged[10,10]), sum(total_feature_results_merged[10,11], total_feature_results_merged[10,12])), 
                             Frequency = 1000,
                             accuracy = c(100,95,90,85,80,79)))

temp_model_df <- model_df
for(i in 1:nrow(model_df)) {
  temp_model_df$attributions[i] <- model_df$attributions[i] / sum(model_df$attributions[which(model_df$accuracy == model_df$accuracy[i])]) * 100
}
model_df <- temp_model_df

model_df <- model_df[-c(which(model_df$accuracy == 79)),]
model_df <- model_df[-c(which(model_df$accuracy == 80)),]

model_df$Frequency <- as.factor(model_df$Frequency)


ggplot(data = model_df, aes(x=accuracy, y=attributions, Frequency)) +
  geom_line(aes(colour = Frequency), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") 

frequency_model_df <- model_df


############################################################
#ggplot of dataset results
############################################################

# add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_dataset_results %>% add_rownames(), 
#                     test2_total_dataset_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_dataset_results

# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))


#####################################################
# TO NORMALIZE THE RESULTS - THE OTHER METHOD CAN'T WORK BECAUSE DATASERTS ARE DIFFERENT SIZES, UNLIKE OTHER TESTS
#####################################################
# 

#40 by feature and test and 91/366/7 samples tested 10 times
total_tests_large <- 40 * 91 * 10
total_tests_small <- 40 * 366 * 10
texts <- 40 * 7 * 10
# 
# 
# #######################
# 
for (i in 1:nrow(total_feature_results_merged)) {
  if (i == 1) {
    total <- total_tests_large
  } else if (i == 2) {
    total <- total_tests_small
  } else if (i == 3) {
    total <- texts
  }
  for (x in 1:ncol(total_feature_results_merged)) {
    total_feature_results_merged[i,x] <- total_feature_results_merged[i,x] / total * 100
  }

}

#####################################################


# #make df - FOR SIX DATASETS
# model_df <- data.frame(
#   attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
#                    sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
#                    sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
#   Dataset = as.character("Texts"),
#   accuracy = c(100,95,90,85,80,79))
# 
# model_df <- rbind(model_df,
#                   data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
#                                               sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
#                                               sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
#                              Dataset = as.character("Large samples"),
#                              accuracy = c(100,95,90,85,80,79)))
# model_df <- rbind(model_df,
#                   data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
#                                               sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
#                                               sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
#                              Dataset = as.character("Small samples"),
#                              accuracy = c(100,95,90,85,80,79)))
# 
# dataset_model_df <- model_df

#FOR THREE DATASETS
model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                   sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                   sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
  Dataset = as.character("Texts"),
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Dataset = as.character("Large samples"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Dataset = as.character("Small samples"),
                             accuracy = c(100,95,90,85,80,79)))


model_df <- model_df[-c(which(model_df$accuracy == 79)),]
model_df <- model_df[-c(which(model_df$accuracy == 80)),]

ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Dataset)) +
  geom_line(aes(colour = Dataset), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions %") 

dataset_model_df <- model_df

# ##############PLOTTING
# 
# p1<-ggplot(data = classification_model_df, aes(x=accuracy, y=attributions, group=Classification)) +
#   geom_line(aes(colour = Classification), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") +
#   scale_fill_discrete(name = "New Legend Title") +
#   theme(axis.title.x=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# p2<-ggplot(data = feature_model_df, aes(x=accuracy, y=attributions, group=Feature)) +
#   geom_line(aes(colour = Feature), size = 1) +
#   #xlab("Accuracy %") +
#   #ylab("Correct attributions") +
#   theme(axis.title.x=element_blank(),
#         #axis.text.x=element_blank(),
#         #axis.ticks.x=element_blank(),
#         axis.title.y=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# p3<-ggplot(data = frequency_model_df, aes(x=accuracy, y=attributions, Frequency)) +
#   geom_line(aes(colour = Frequency), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") +
#   theme(axis.title.y=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# 
# p4<-ggplot(data = dataset_model_df, aes(x=accuracy, y=attributions, group=Dataset)) +
#   geom_line(aes(colour = Dataset), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") 
# 
# 
# 
# grid.arrange(p1, p2, p4, p3, nrow = 2)
# 

##############PLOTTING

p1 <- classification_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Classification), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Classification, colour = Classification)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())

p2 <- feature_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Feature), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Feature, colour = Feature)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())


p3 <- frequency_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Frequency), NA_character_)) %>%
  # mutate(label = if_else(((accuracy == max(accuracy) & attributions == 486) | 
  #                           (accuracy == max(accuracy) & attributions == 465) |
  #                           (accuracy == max(accuracy) & attributions == 429) |
  #                           (accuracy == max(accuracy) & attributions == 173) |
  #                           (accuracy == max(accuracy) & attributions == 291) |
  #                           (accuracy == max(accuracy) & attributions == 334) )
  #                        
  #                       , as.character(Frequency), NA_character_)) %>%
  #mutate(label = if_else(attributions == c(486, 465, 429, 173, 291, 384), as.character(Frequency), NA_character_)) %>%
  
  ggplot(aes(x = accuracy, y = attributions, group = Frequency, colour = Frequency)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())

p4 <- dataset_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Dataset), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Dataset, colour = Dataset)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions as %") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


grid.arrange(p1, p2, p4, p3, nrow = 2)





##########################################################
##########################################################
##########################################################

##########################################################
#############################################################
##################################################################
########################################################
#################################################
##########################################################
#plot results as percentage of 100% correct
##########################################################



#make df

model_df <- data.frame(
  attributions = c(sum(total_model_results[5,1], total_model_results[5,2]), sum(total_model_results[5,3], total_model_results[5,4]), 
                   sum(total_model_results[5,5], total_model_results[5,6]), sum(total_model_results[5,7], total_model_results[5,8]), 
                   sum(total_model_results[5,9], total_model_results[5,10]), sum(total_model_results[5,11], total_model_results[5,12])), Classification = as.character("SVM"),
  accuracy = c(100,95,90,85,80,79))


model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[1,1], total_model_results[1,2]), sum(total_model_results[1,3], total_model_results[1,4]), 
                                              sum(total_model_results[1,5], total_model_results[1,6]), sum(total_model_results[1,7], total_model_results[1,8]), 
                                              sum(total_model_results[1,9], total_model_results[1,10]), sum(total_model_results[1,11], total_model_results[1,12])), 
                             Classification = as.character("Delta"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[2,1], total_model_results[2,2]), sum(total_model_results[2,3], total_model_results[2,4]), 
                                              sum(total_model_results[2,5], total_model_results[2,6]), sum(total_model_results[2,7], total_model_results[2,8]), 
                                              sum(total_model_results[2,9], total_model_results[2,10]), sum(total_model_results[2,11], total_model_results[2,12])), 
                             Classification = as.character("KNN"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[3,1], total_model_results[3,2]), sum(total_model_results[3,3], total_model_results[3,4]), 
                                              sum(total_model_results[3,5], total_model_results[3,6]), sum(total_model_results[3,7], total_model_results[3,8]), 
                                              sum(total_model_results[3,9], total_model_results[3,10]), sum(total_model_results[3,11], total_model_results[3,12])), 
                             Classification = as.character("NB"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_model_results[4,1], total_model_results[4,2]), sum(total_model_results[4,3], total_model_results[4,4]), 
                                              sum(total_model_results[4,5], total_model_results[4,6]), sum(total_model_results[4,7], total_model_results[4,8]), 
                                              sum(total_model_results[4,9], total_model_results[4,10]), sum(total_model_results[4,11], total_model_results[4,12])), 
                             Classification = as.character("NSC"),
                             accuracy = c(100,95,90,85,80,79)))

# temp_model_df <- model_df
# for(i in 1:nrow(model_df)) {
#   temp_model_df$attributions[i] <- model_df$attributions[i] / sum(model_df$attributions[which(model_df$accuracy == model_df$accuracy[i])]) * 100
# }
# model_df <- temp_model_df
#model_df <- model_df[-c(which(model_df$accuracy == 79)),]
#model_df <- model_df[-c(which(model_df$accuracy == 80)),]


#####################################################
# TO NORMALIZE THE RESULTS - THE OTHER METHOD CAN'T WORK BECAUSE DATASERTS ARE DIFFERENT SIZES, UNLIKE OTHER TESTS
#####################################################
# 



#To get 100% it can be one of 10 mffeature tests on 8 feature tests on 3 datasets, with 3 large samps, 12

temp_model_df <- model_df
for(i in 1:nrow(model_df)) {
   temp_model_df$attributions[i] <- (model_df$attributions[i] / (10*8*3)) * 100
}
model_df <- temp_model_df

# total_tests_method <- 40 * 91 * 10
# total_tests_small <- 40 * 366 * 10
# texts <- 40 * 7 * 10
# 
# 
# #######################
# 
# for (i in 1:nrow(total_feature_results_merged)) {
#   if (i == 1) {
#     total <- total_tests_large
#   } else if (i == 2) {
#     total <- total_tests_small
#   } else if (i == 3) {
#     total <- texts
#   }
#   for (x in 1:ncol(total_feature_results_merged)) {
#     total_feature_results_merged[i,x] <- total_feature_results_merged[i,x] / total * 100
#   }
#   
# }

#####################################################

ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Classification)) +
  geom_line(aes(colour = Classification), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  scale_fill_discrete(name = "New Legend Title")

classification_model_df <- model_df


############################################################
#ggplot of feature results
############################################################
# 
# # add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_feature_results %>% add_rownames(), 
#                     test2_total_feature_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_feature_results
# 
# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))

#make df

model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
                   sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
                   sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
  Feature = as.character("4C"),
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Feature = as.character("1W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Feature = as.character("2W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                                              sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                                              sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
                             Feature = as.character("3W"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[4,1], total_feature_results_merged[4,2]), sum(total_feature_results_merged[4,3], total_feature_results_merged[4,4]), 
                                              sum(total_feature_results_merged[4,5], total_feature_results_merged[4,6]), sum(total_feature_results_merged[4,7], total_feature_results_merged[4,8]), 
                                              sum(total_feature_results_merged[4,9], total_feature_results_merged[4,10]), sum(total_feature_results_merged[4,11], total_feature_results_merged[4,12])), 
                             Feature = as.character("3C"),
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[6,1], total_feature_results_merged[6,2]), sum(total_feature_results_merged[6,3], total_feature_results_merged[6,4]), 
                                              sum(total_feature_results_merged[6,5], total_feature_results_merged[6,6]), sum(total_feature_results_merged[6,7], total_feature_results_merged[6,8]), 
                                              sum(total_feature_results_merged[6,9], total_feature_results_merged[6,10]), sum(total_feature_results_merged[6,11], total_feature_results_merged[6,12])), 
                             Feature = as.character("5C"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[7,1], total_feature_results_merged[7,2]), sum(total_feature_results_merged[7,3], total_feature_results_merged[7,4]), 
                                              sum(total_feature_results_merged[7,5], total_feature_results_merged[7,6]), sum(total_feature_results_merged[7,7], total_feature_results_merged[7,8]), 
                                              sum(total_feature_results_merged[7,9], total_feature_results_merged[7,10]), sum(total_feature_results_merged[7,11], total_feature_results_merged[7,12])), 
                             Feature = as.character("6C"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[8,1], total_feature_results_merged[8,2]), sum(total_feature_results_merged[8,3], total_feature_results_merged[8,4]), 
                                              sum(total_feature_results_merged[8,5], total_feature_results_merged[8,6]), sum(total_feature_results_merged[8,7], total_feature_results_merged[8,8]), 
                                              sum(total_feature_results_merged[8,9], total_feature_results_merged[8,10]), sum(total_feature_results_merged[8,11], total_feature_results_merged[8,12])), 
                             Feature = as.character("7C"),
                             accuracy = c(100,95,90,85,80,79)))


#by feature = 910 * 5 (tests) , 3660 * 5, 70 * 5

temp_model_df <- model_df
for(i in 1:nrow(model_df)) {
  temp_model_df$attributions[i] <- model_df$attributions[i] / (((5 * 910) + (5 * 3660) + (5 * 70)) * 3) * 100
}
model_df <- temp_model_df


# temp <- model_df
# model_df <- temp
# 
# model_df <- temp[-(c(37:54)),]
# model_df <- temp[-(c(1:36)),]
# 
ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Feature)) +
  geom_line(aes(colour = Feature), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") 



feature_model_df <- model_df




############################################################
#ggplot of frequent results
############################################################

# # add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_mfw_results %>% add_rownames(), 
#                     test2_total_mfw_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_mfw_results

# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))

#make df

model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
                   sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
                   sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
  Frequency = 500,
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Frequency = 100,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Frequency = 200,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                                              sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                                              sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
                             Frequency = 300,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[4,1], total_feature_results_merged[4,2]), sum(total_feature_results_merged[4,3], total_feature_results_merged[4,4]), 
                                              sum(total_feature_results_merged[4,5], total_feature_results_merged[4,6]), sum(total_feature_results_merged[4,7], total_feature_results_merged[4,8]), 
                                              sum(total_feature_results_merged[4,9], total_feature_results_merged[4,10]), sum(total_feature_results_merged[4,11], total_feature_results_merged[4,12])), 
                             Frequency = 400,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[6,1], total_feature_results_merged[6,2]), sum(total_feature_results_merged[6,3], total_feature_results_merged[6,4]), 
                                              sum(total_feature_results_merged[6,5], total_feature_results_merged[6,6]), sum(total_feature_results_merged[6,7], total_feature_results_merged[6,8]), 
                                              sum(total_feature_results_merged[6,9], total_feature_results_merged[6,10]), sum(total_feature_results_merged[6,11], total_feature_results_merged[6,12])), 
                             Frequency = 600,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[7,1], total_feature_results_merged[7,2]), sum(total_feature_results_merged[7,3], total_feature_results_merged[7,4]), 
                                              sum(total_feature_results_merged[7,5], total_feature_results_merged[7,6]), sum(total_feature_results_merged[7,7], total_feature_results_merged[7,8]), 
                                              sum(total_feature_results_merged[7,9], total_feature_results_merged[7,10]), sum(total_feature_results_merged[7,11], total_feature_results_merged[7,12])), 
                             Frequency = 700,
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[8,1], total_feature_results_merged[8,2]), sum(total_feature_results_merged[8,3], total_feature_results_merged[8,4]), 
                                              sum(total_feature_results_merged[8,5], total_feature_results_merged[8,6]), sum(total_feature_results_merged[8,7], total_feature_results_merged[8,8]), 
                                              sum(total_feature_results_merged[8,9], total_feature_results_merged[8,10]), sum(total_feature_results_merged[8,11], total_feature_results_merged[8,12])), 
                             Frequency = 800,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[9,1], total_feature_results_merged[9,2]), sum(total_feature_results_merged[9,3], total_feature_results_merged[9,4]), 
                                              sum(total_feature_results_merged[9,5], total_feature_results_merged[9,6]), sum(total_feature_results_merged[9,7], total_feature_results_merged[9,8]), 
                                              sum(total_feature_results_merged[9,9], total_feature_results_merged[9,10]), sum(total_feature_results_merged[9,11], total_feature_results_merged[9,12])), 
                             Frequency = 900,
                             accuracy = c(100,95,90,85,80,79)))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[10,1], total_feature_results_merged[10,2]), sum(total_feature_results_merged[10,3], total_feature_results_merged[10,4]), 
                                              sum(total_feature_results_merged[10,5], total_feature_results_merged[10,6]), sum(total_feature_results_merged[10,7], total_feature_results_merged[10,8]), 
                                              sum(total_feature_results_merged[10,9], total_feature_results_merged[10,10]), sum(total_feature_results_merged[10,11], total_feature_results_merged[10,12])), 
                             Frequency = 1000,
                             accuracy = c(100,95,90,85,80,79)))

temp_model_df <- model_df
for(i in 1:nrow(model_df)) {
  temp_model_df$attributions[i] <- model_df$attributions[i] / sum(model_df$attributions[which(model_df$accuracy == model_df$accuracy[i])]) * 100
}
model_df <- temp_model_df

model_df <- model_df[-c(which(model_df$accuracy == 79)),]
model_df <- model_df[-c(which(model_df$accuracy == 80)),]

model_df$Frequency <- as.factor(model_df$Frequency)


ggplot(data = model_df, aes(x=accuracy, y=attributions, Frequency)) +
  geom_line(aes(colour = Frequency), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions") 

frequency_model_df <- model_df


############################################################
#ggplot of dataset results
############################################################

# add rownames as a column in each data.frame and bind rows
# merged <- bind_rows(test1_total_dataset_results %>% add_rownames(), 
#                     test2_total_dataset_results %>% add_rownames()) %>% 
#   # evaluate following calls for each value in the rowname column
#   group_by(rowname) %>% 
#   # add all non-grouping variables
#   summarise_all(sum)
# 
# total_feature_results_merged <- merged

total_feature_results_merged <- total_dataset_results

# total_feature_results_merged <- total_feature_results_merged[,-1]
# row.names(total_feature_results_merged) <- unlist(c(merged[,1]))


#####################################################
# TO NORMALIZE THE RESULTS - THE OTHER METHOD CAN'T WORK BECAUSE DATASERTS ARE DIFFERENT SIZES, UNLIKE OTHER TESTS
#####################################################
# 

#40 by feature and test and 91/366/7 samples tested 10 times
total_tests_large <- 40 * 91 * 10
total_tests_small <- 40 * 366 * 10
texts <- 40 * 7 * 10
# 
# 
# #######################
# 
for (i in 1:nrow(total_feature_results_merged)) {
  if (i == 1) {
    total <- total_tests_large
  } else if (i == 2) {
    total <- total_tests_small
  } else if (i == 3) {
    total <- texts
  }
  for (x in 1:ncol(total_feature_results_merged)) {
    total_feature_results_merged[i,x] <- total_feature_results_merged[i,x] / total * 100
  }
  
}

#####################################################


# #make df - FOR SIX DATASETS
# model_df <- data.frame(
#   attributions = c(sum(total_feature_results_merged[5,1], total_feature_results_merged[5,2]), sum(total_feature_results_merged[5,3], total_feature_results_merged[5,4]), 
#                    sum(total_feature_results_merged[5,5], total_feature_results_merged[5,6]), sum(total_feature_results_merged[5,7], total_feature_results_merged[5,8]), 
#                    sum(total_feature_results_merged[5,9], total_feature_results_merged[5,10]), sum(total_feature_results_merged[5,11], total_feature_results_merged[5,12])), 
#   Dataset = as.character("Texts"),
#   accuracy = c(100,95,90,85,80,79))
# 
# model_df <- rbind(model_df,
#                   data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
#                                               sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
#                                               sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
#                              Dataset = as.character("Large samples"),
#                              accuracy = c(100,95,90,85,80,79)))
# model_df <- rbind(model_df,
#                   data.frame(attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
#                                               sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
#                                               sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
#                              Dataset = as.character("Small samples"),
#                              accuracy = c(100,95,90,85,80,79)))
# 
# dataset_model_df <- model_df

#FOR THREE DATASETS
model_df <- data.frame(
  attributions = c(sum(total_feature_results_merged[3,1], total_feature_results_merged[3,2]), sum(total_feature_results_merged[3,3], total_feature_results_merged[3,4]), 
                   sum(total_feature_results_merged[3,5], total_feature_results_merged[3,6]), sum(total_feature_results_merged[3,7], total_feature_results_merged[3,8]), 
                   sum(total_feature_results_merged[3,9], total_feature_results_merged[3,10]), sum(total_feature_results_merged[3,11], total_feature_results_merged[3,12])), 
  Dataset = as.character("Texts"),
  accuracy = c(100,95,90,85,80,79))

model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[1,1], total_feature_results_merged[1,2]), sum(total_feature_results_merged[1,3], total_feature_results_merged[1,4]), 
                                              sum(total_feature_results_merged[1,5], total_feature_results_merged[1,6]), sum(total_feature_results_merged[1,7], total_feature_results_merged[1,8]), 
                                              sum(total_feature_results_merged[1,9], total_feature_results_merged[1,10]), sum(total_feature_results_merged[1,11], total_feature_results_merged[1,12])), 
                             Dataset = as.character("Large samples"),
                             accuracy = c(100,95,90,85,80,79)))
model_df <- rbind(model_df,
                  data.frame(attributions = c(sum(total_feature_results_merged[2,1], total_feature_results_merged[2,2]), sum(total_feature_results_merged[2,3], total_feature_results_merged[2,4]), 
                                              sum(total_feature_results_merged[2,5], total_feature_results_merged[2,6]), sum(total_feature_results_merged[2,7], total_feature_results_merged[2,8]), 
                                              sum(total_feature_results_merged[2,9], total_feature_results_merged[2,10]), sum(total_feature_results_merged[2,11], total_feature_results_merged[2,12])), 
                             Dataset = as.character("Small samples"),
                             accuracy = c(100,95,90,85,80,79)))


model_df <- model_df[-c(which(model_df$accuracy == 79)),]
model_df <- model_df[-c(which(model_df$accuracy == 80)),]

ggplot(data = model_df, aes(x=accuracy, y=attributions, group=Dataset)) +
  geom_line(aes(colour = Dataset), size = 1) +
  xlab("Accuracy %") +
  ylab("Correct attributions %") 

dataset_model_df <- model_df

# ##############PLOTTING
# 
# p1<-ggplot(data = classification_model_df, aes(x=accuracy, y=attributions, group=Classification)) +
#   geom_line(aes(colour = Classification), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") +
#   scale_fill_discrete(name = "New Legend Title") +
#   theme(axis.title.x=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# p2<-ggplot(data = feature_model_df, aes(x=accuracy, y=attributions, group=Feature)) +
#   geom_line(aes(colour = Feature), size = 1) +
#   #xlab("Accuracy %") +
#   #ylab("Correct attributions") +
#   theme(axis.title.x=element_blank(),
#         #axis.text.x=element_blank(),
#         #axis.ticks.x=element_blank(),
#         axis.title.y=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# p3<-ggplot(data = frequency_model_df, aes(x=accuracy, y=attributions, Frequency)) +
#   geom_line(aes(colour = Frequency), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") +
#   theme(axis.title.y=element_blank()
#         #, axis.text.y=element_blank(),
#         #axis.ticks.y=element_blank()
#   )
# 
# 
# p4<-ggplot(data = dataset_model_df, aes(x=accuracy, y=attributions, group=Dataset)) +
#   geom_line(aes(colour = Dataset), size = 1) +
#   xlab("Accuracy %") +
#   ylab("Correct attributions") 
# 
# 
# 
# grid.arrange(p1, p2, p4, p3, nrow = 2)
# 

##############PLOTTING

p1 <- classification_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Classification), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Classification, colour = Classification)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())

p2 <- feature_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Feature), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Feature, colour = Feature)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())


p3 <- frequency_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Frequency), NA_character_)) %>%
  # mutate(label = if_else(((accuracy == max(accuracy) & attributions == 486) | 
  #                           (accuracy == max(accuracy) & attributions == 465) |
  #                           (accuracy == max(accuracy) & attributions == 429) |
  #                           (accuracy == max(accuracy) & attributions == 173) |
  #                           (accuracy == max(accuracy) & attributions == 291) |
  #                           (accuracy == max(accuracy) & attributions == 334) )
  #                        
  #                       , as.character(Frequency), NA_character_)) %>%
  #mutate(label = if_else(attributions == c(486, 465, 429, 173, 291, 384), as.character(Frequency), NA_character_)) %>%
  
  ggplot(aes(x = accuracy, y = attributions, group = Frequency, colour = Frequency)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions") +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank())

p4 <- dataset_model_df %>%
  mutate(label = if_else(accuracy == max(accuracy), as.character(Dataset), NA_character_)) %>%
  ggplot(aes(x = accuracy, y = attributions, group = Dataset, colour = Dataset)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position="none") +
  xlab("Accuracy %") +
  ylab("Correct attributions as %") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


grid.arrange(p1, p2, p4, p3, nrow = 2)








# 
#  
#   ###############################################
#   # Create table of results for chunks of Sister Peg
#   
#   batch_tests <- c("small_samp_no_cull", "small_samp_with_cull")
#   char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
#   tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
#   tests <- c("delta", "knn", "nb", "nsc", "svm_linear")
#   sp_chunks <- c(1:12)
#   accuracies <- c(100,95,90,85,80,79)
#   
#   
# total_sp_sample_results <- data.frame(hume_100=rep(0, 12), ferg_100=rep(0, 12), hume_95=rep(0, 12), ferg_95=rep(0, 12), 
#                                       hume_90=rep(0, 12), ferg_90=rep(0, 12), hume_85=rep(0, 12), ferg_85=rep(0, 12), 
#                                       hume_80=rep(0, 12), ferg_80=rep(0, 12),  hume_79=rep(0, 12), ferg_79=rep(0, 12), 
#                                       row.names = sp_chunks) 
# 
# for (i in 1:length(batch_tests)) {
#   #get results  
#   result_files <- list.files(paste0(results_dir, batch_tests[i], "/"), full.names = TRUE)
#   for (x in 1:length(features)) {
#     #temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
#     cat("\r", x)
#     file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
#     for (y in 1:length(file_locs)) {
#       test <- read.delim(result_files[file_locs[y]], header = FALSE, stringsAsFactors = FALSE)
#       # for(z in 1:length(tests)) { 
#       #   if(grepl(tests[z], result_files[file_locs[y]])) {
#       #     current_test <- tests[z]
#       #   }
#       # }
#       # for(z in 1:length(tests)) { 
#       #   if(grepl(tests[z], result_files[file_locs[i]])) {
#       #     current_test <- tests[z]
#       #   }
#       # }
#       #grepl(tests, result_files[x])
#       testing <- TRUE
#       test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
#       test_cutoffs <- sort(test_cutoffs)
#       test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
#       while(testing == TRUE) {
#         if(length(test_cutoffs) == 0) { 
#           testing <- FALSE 
#           next
#         }
#         temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
#         # #remove non sister peg tests
#         # if (length(grep("merged", temp_results$V1)) != 0) {
#         #   temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
#         #   temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
#         # }
#         # #remove non sister peg tests
#         # if (length(grep("Hume_", temp_results$V1)) != 0) {
#         #   temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
#         # }
#         # if (length(grep("Ferguson_", temp_results$V1)) != 0) {
#         #   temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
#         # }
#         #remove non sister peg tests
#         if (length(grep("Hume", temp_results$V1)) != 0) {
#           temp_results <- temp_results[-c(grep("Hume", temp_results$V1)),]
#         }
#         if (length(grep("Ferg", temp_results$V1)) != 0) {
#           temp_results <- temp_results[-c(grep("Ferg", temp_results$V1)),]
#         }
#         current_accuracy <- test$V2[test_cutoffs[2]+1]
#         current_accuracy <- gsub("[(]", "", current_accuracy)
#         current_accuracy <- gsub("[)]", "", current_accuracy)
#         current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
#         
#         for (z in 1:length(accuracies)) {
#           if(current_accuracy == 100) { 
#             accuracy_group <- current_accuracy
#             break
#           } else if (current_accuracy < 80) {
#             accuracy_group <- 79
#             break
#           } else {
#             if (current_accuracy < accuracies[z-1] && current_accuracy >= accuracies[z]) {
#               accuracy_group <- accuracies[z]
#             }
#           }
#         }
#         for (z in 1:length(sp_chunks)) {
#           cat("\r", z)
#           total_sp_sample_results[z,grep(paste0("hume_", accuracy_group), names(total_sp_sample_results))] <-
#             total_sp_sample_results[z,grep(paste0("hume_", accuracy_group), names(total_sp_sample_results))] + length(grep("Hume", temp_results$V3[z]))
#           
#           total_sp_sample_results[z,grep(paste0("ferg_", accuracy_group), names(total_sp_sample_results))] <-
#             total_sp_sample_results[z,grep(paste0("ferg_", accuracy_group), names(total_sp_sample_results))] + length(grep("Ferg", temp_results$V3[z]))
#           test_cutoffs <- test_cutoffs[-c(1:2)]
#           next
#         }
#       }
#     }
#   }
# }
# 
