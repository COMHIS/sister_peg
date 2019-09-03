#check all 100%

setwd("Dropbox/github/sister_peg/")


#### all tests


#check all 100%

setwd("Dropbox/github/sister_peg/")

batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")
batch_tests <- c("large_samp_no_cull", "texts_no_cull")
batch_tests <- c("large_samp_no_cull")
batch_tests <- c("small_samp_no_cull")
batch_tests <- c("texts_no_cull")



is_100 <- 0

features <- c("1w", "2w", "3w", "3c", "4c", "5c", "6c", "7c")
models <- c("delta", "knn", "nb", "nsc", "svm")


tests <- c("test_1", "test_2", "test_5", "test_6", "test_7", "test_8", "test_9")
tests <- c("test_1", "test_2", "test_7", "test_8", "test_9")

tests <- "test_1"
tests <- "test_2"
tests <- "test_7"
tests <- "test_8"
tests <- "test_9"

is_100 <- 0
total_test <- 0
for(del_var in 1:length(features)) {
  assign(paste0("f_", features[del_var], "_100"), 0)
}
for(del_var in 1:length(models)) {
  assign(paste0("m_", models[del_var], "_100"), 0)
}
for(ff_var in 1:10) {
  assign(paste0("ff_", ff_var * 100, "_100"), 0)
}


for (i in 1:length(tests)) {
  #remove tests with culling
  result_files <- list.files(paste0("testing_test/ne/", tests[i], "/"), full.names = TRUE, recursive = TRUE) 
  # if (any(grepl("with_cull", result_files) == TRUE)) {
  #   result_files <- result_files[-c(grep("with_cull", result_files))]
  # }
  # #remove small
  # if (any(grepl("small_", result_files) == TRUE)) {
  #   result_files <- result_files[-c(grep("small_", result_files))]
  # }
  #keep only small
   if (any(grepl("small_", result_files) == TRUE)) {
     result_files <- result_files[c(grep("small_", result_files))]
   }
   # #keep only large
  #if (any(grepl("large_", result_files) == TRUE)) {
  #   result_files <- result_files[c(grep("large_", result_files))]
  # }
  # #keep only texts
   #  if (any(grepl("texts_no", result_files) == TRUE)) {
   #   result_files <- result_files[c(grep("texts_no", result_files))]
   # }
  
  #models + FF
  for (model_var in 1:length(models)) {
    model_result_files <- result_files[grep(models[model_var], result_files)]
    
    # if(exists(paste0("f_", features[feature_var], "_100")) == FALSE) {
    #   assign(paste0("f_", features[feature_var], "_100"), 0)
    # }
    
    #run tests on specific result files per batch
    for (y in 1:length(model_result_files)) {
      cat("\n", tests[i], model_result_files[y])
      test <- read.delim(model_result_files[y], header = FALSE, stringsAsFactors = FALSE)
      
      testing <- TRUE
      test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
      test_cutoffs <- sort(test_cutoffs)
      test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
      MFW_num <- 0
      while(testing == TRUE) {
        if(length(test_cutoffs) == 0) { 
          testing <- FALSE 
          next
        }
        MFW_num <- MFW_num+1
        temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
        current_accuracy <- test$V2[test_cutoffs[2]+1]
        current_accuracy <- gsub("[(]", "", current_accuracy)
        current_accuracy <- gsub("[)]", "", current_accuracy)
        current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
        
        total_test <- total_test + 1
        if (current_accuracy != 100) {
          test_cutoffs <- test_cutoffs[-c(1:2)]
          next
        }
        is_100 <- is_100+1
        #feature_is_100 <- feature_is_100+1
        temp_count <- get(paste0("m_", models[model_var], "_100")) + 1
        assign(paste0("m_", models[model_var], "_100"), temp_count)
        
        
        temp_count <- get(paste0("ff_", MFW_num*100, "_100")) + 1
        assign(paste0("ff_", MFW_num*100, "_100"), temp_count)
        
        
        test_cutoffs <- test_cutoffs[-c(1:2)]
      }
    }
  }
  
  
  #feature
  for (feature_var in 1:length(features)) {
    feature_result_files <- result_files[grep(features[feature_var], result_files)]
    
    # if(exists(paste0("f_", features[feature_var], "_100")) == FALSE) {
    #   assign(paste0("f_", features[feature_var], "_100"), 0)
    # }
    
    #run tests on specific result files per batch
    for (y in 1:length(feature_result_files)) {
      cat("\n", tests[i], feature_result_files[y])
      test <- read.delim(feature_result_files[y], header = FALSE, stringsAsFactors = FALSE)
      
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
        current_accuracy <- test$V2[test_cutoffs[2]+1]
        current_accuracy <- gsub("[(]", "", current_accuracy)
        current_accuracy <- gsub("[)]", "", current_accuracy)
        current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
        
        total_test <- total_test + 1
        if (current_accuracy != 100) {
          test_cutoffs <- test_cutoffs[-c(1:2)]
          next
        }
        is_100 <- is_100+1
        #feature_is_100 <- feature_is_100+1
        temp_count <- get(paste0("f_", features[feature_var], "_100")) + 1
        assign(paste0("f_", features[feature_var], "_100"), temp_count)
        test_cutoffs <- test_cutoffs[-c(1:2)]
      }
    }
  }
}  



barplot(c(f_1w_100, f_2w_100, f_3w_100, f_3c_100, f_4c_100, f_5c_100, f_6c_100, f_7c_100))






### specific tests




batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")
batch_tests <- c("large_samp_no_cull")
batch_tests <- c("small_samp_no_cull")
batch_tests <- c("texts_no_cull")

batch_tests <- c("small_samp_with_cull", "large_samp_with_cull", "texts_with_cull")
batch_tests <- c("small_samp_with_cull", "large_samp_with_cull", "texts_with_cull")

ferg <- 0
hume <- 0
carl <- 0
blair <- 0
kames <- 0
smith <- 0
is_100 <- 0

for (i in 1:length(batch_tests)) {
  #result_files <- list.files(paste0("test1_results/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test2_results/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test3_results_car/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test4_results_others/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test5_results_scots/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test7_results_kames/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test6_results_blair/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("par_test/results/", batch_tests[i], "/"), full.names = TRUE)
  
  result_files <- list.files(paste0("testing_test/ne/test_1/", batch_tests[i], "/"), full.names = TRUE) #blair
  # result_files <- result_files[-c(which(!grepl("delta", result_files)))]
  # result_files <- result_files[-c(which(!grepl("knn", result_files)))]
  # result_files <- result_files[-c(which(!grepl("nsc", result_files)))]
  # result_files <- result_files[-c(which(!grepl("nb", result_files)))]
  # result_files <- result_files[-c(which(!grepl("svm", result_files)))]
  # #result_files <- list.files(paste0("testing_test/ne/test_2/", batch_tests[i], "/"), full.names = TRUE) #blair
  
  #result_files <- list.files(paste0("testing_test/ne/test_7/", batch_tests[i], "/"), full.names = TRUE) #blair
  #result_files <- list.files(paste0("testing_test/ne/test_8/", batch_tests[i], "/"), full.names = TRUE) #carlyle
  #result_files <- list.files(paste0("testing_test/ne/test_9/", batch_tests[i], "/"), full.names = TRUE) #kames
  for (y in 1:length(result_files)) {
    cat("\r", batch_tests[i], result_files[y])
    test <- read.delim(result_files[y], header = FALSE, stringsAsFactors = FALSE)
    
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
      current_accuracy <- test$V2[test_cutoffs[2]+1]
      current_accuracy <- gsub("[(]", "", current_accuracy)
      current_accuracy <- gsub("[)]", "", current_accuracy)
      current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
      
      #if (current_accuracy < 90) {
      #if (current_accuracy < 95) {
      #if (current_accuracy < 98) {
      if (current_accuracy != 100) {
        
        test_cutoffs <- test_cutoffs[-c(1:2)]
        next
      }
      is_100 <- is_100+1
      ###################
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
      
      ###########################
      #remove non-EB rsults
      # if (any(grepl("EB", temp_results$V1))) {
      #   temp_results <- temp_results[-c(which(!grepl("EB", temp_results$V1))),]
      # }
      # 
      ##########################################
      #remove non-SP rsults
      if (any(!grepl("sister", temp_results$V1) == TRUE)) {
        temp_results <- temp_results[-c(which(!grepl("sister", temp_results$V1))),]
      }
      
      
      hume <- hume + length(grep("Hume", temp_results$V3))
      ferg <- ferg + length(grep("Ferg", temp_results$V3))
      carl <- carl + length(grep("Carl", temp_results$V3))
      blair <- blair + length(grep("Blair", temp_results$V3))
      smith <- smith + length(grep("Smith", temp_results$V3))
      kames <- kames + length(grep("Kames", temp_results$V3))
      test_cutoffs <- test_cutoffs[-c(1:2)]
    }
  }
}

barplot(c(hume, ferg, carl, blair, smith, kames))
