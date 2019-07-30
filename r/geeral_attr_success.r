#general attributive success!

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)

setwd("")

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")
batch_tests <- c("texts_no_cull", "texts_with_cull")

char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "3", "4", "5", "6", "7")

mffeatures <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

tests <- c("delta", "knn", "nb", "nsc", "svm")
accuracies <- c(100,95,90,85,80,79)

test_dirs <- c("test_1", "test_2", "test_3")
test_dirs <- c("test_1", "test_2")
test_dirs <- c("test_1")
test_dirs <- c("test_2")
test_dirs <- c("test_1", "test_2", "test_3", "test_5", "test_6")

total_correct <- 0
total_of <- 0
for (i in 1:length(batch_tests)) {
  #get results  
  result_files <- list.files(paste0(results_dir, test_dirs[test_dir_var], "/", batch_tests[i], "/"), full.names = TRUE)
  
  if(i == 1) {
    small_total_correct <- 0
    small_total_of <- 0
    
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      results <- test$V1[grep("General attributive success:", test$V1)]
      results <- regmatches(results, gregexpr("[[:digit:]]+", results))
      temp_correct <- as.numeric(unlist(results)[1])
      temp_of <- as.numeric(unlist(results)[2])
      
      small_total_correct <- sum(small_total_correct, temp_correct)
      small_total_of <- sum(small_total_of, temp_of)
    }
  } else if (i == 2) {
    large_total_correct <- 0
    large_total_of <- 0
    
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      results <- test$V1[grep("General attributive success:", test$V1)]
      results <- regmatches(results, gregexpr("[[:digit:]]+", results))
      temp_correct <- as.numeric(unlist(results)[1])
      temp_of <- as.numeric(unlist(results)[2])
      
      large_total_correct <- sum(large_total_correct, temp_correct)
      large_total_of <- sum(large_total_of, temp_of)
    }
  } else if (i == 3) {
    texts_total_correct <- 0
    texts_total_of <- 0
    
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      results <- test$V1[grep("General attributive success:", test$V1)]
      results <- regmatches(results, gregexpr("[[:digit:]]+", results))
      temp_correct <- as.numeric(unlist(results)[1])
      temp_of <- as.numeric(unlist(results)[2])
      
      texts_total_correct <- sum(texts_total_correct, temp_correct)
      texts_total_of <- sum(texts_total_of, temp_of)
    }
  }
}


#METHOD

for (i in 1:length(tests)) {
  #get results  
  result_files <- list.files(paste0(results_dir, test_dirs[test_dir_var], "/", batch_tests[i], "/"), full.names = TRUE)
  
  if(i == 1) {
    small_total_correct <- 0
    small_total_of <- 0
    
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      results <- test$V1[grep("General attributive success:", test$V1)]
      results <- regmatches(results, gregexpr("[[:digit:]]+", results))
      temp_correct <- as.numeric(unlist(results)[1])
      temp_of <- as.numeric(unlist(results)[2])
      
      small_total_correct <- sum(small_total_correct, temp_correct)
      small_total_of <- sum(small_total_of, temp_of)
    }
  } else if (i == 2) {
    large_total_correct <- 0
    large_total_of <- 0
    
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      results <- test$V1[grep("General attributive success:", test$V1)]
      results <- regmatches(results, gregexpr("[[:digit:]]+", results))
      temp_correct <- as.numeric(unlist(results)[1])
      temp_of <- as.numeric(unlist(results)[2])
      
      large_total_correct <- sum(large_total_correct, temp_correct)
      large_total_of <- sum(large_total_of, temp_of)
    }
  } else if (i == 3) {
    texts_total_correct <- 0
    texts_total_of <- 0
    
    to_test <- c()
    for(z in 1:length(tests)) { 
      to_test <- c(to_test, grep(tests[z], result_files))
    }
    result_files <- result_files[to_test]
    
    for (x in 1:length(result_files)) {
      cat("\r", x)
      test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
      results <- test$V1[grep("General attributive success:", test$V1)]
      results <- regmatches(results, gregexpr("[[:digit:]]+", results))
      temp_correct <- as.numeric(unlist(results)[1])
      temp_of <- as.numeric(unlist(results)[2])
      
      texts_total_correct <- sum(texts_total_correct, temp_correct)
      texts_total_of <- sum(texts_total_of, temp_of)
    }
  }
}


  
  ######################
  #create and load test by features
  
  for (i in 1:length(char_word)) {
    temp_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), 
                               hume_90=rep(0, 5), ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), 
                               hume_80=rep(0, 5), ferg_80=rep(0, 5),  hume_79=rep(0, 5), ferg_79=rep(0, 5), 
                               row.names = tests) 
    assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
  }
  
  features <- c("w1", "w2", "w3", "c3", "c4", "c5", "c6", "c7")
  total_feature_results <- data.frame(hume_100=rep(0, 8), ferg_100=rep(0, 8), hume_95=rep(0, 8), ferg_95=rep(0, 8), 
                                      hume_90=rep(0, 8), ferg_90=rep(0, 8), hume_85=rep(0, 8), ferg_85=rep(0, 8), 
                                      hume_80=rep(0, 8), ferg_80=rep(0, 8),  hume_79=rep(0, 8), ferg_79=rep(0, 8), 
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
