#check results for specific tests and parameters

'%!in%' <- function(x,y)!('%in%'(x,y))

dataset_test <- c("test_1", "test_2", "test_7", "test_8", "test_9")
dataset_test <- c("test_1")
dataset_test <- c("test_2")
dataset_test <- c("test_7")
dataset_test <- c("test_8")
dataset_test <- c("test_9")

is_100 <- 0
ferg <- 0
hume <- 0
carl <- 0
blair <- 0
kames <- 0
smith <- 0
is_100 <- 0

for (i_tests in 1:length(dataset_test)) {
  if (dataset_test[i_tests] == "test_1") {
    batch_tests <- c("large_samp_no_cull", "texts_no_cull")
    #features <- c("1w", "2w", "3w", "3c", "4c", "5c", "6c", "7c")
    features <- c("1w", "2w", "3c", "7c")
    models <- c("svm")
    mfws <- c(500, 600, 700, 800, 900, 1000)
  } else if (dataset_test[i_tests] == "test_2") {
    batch_tests <- c("large_samp_no_cull", "texts_no_cull")
    #features <- c("1w", "2w", "3w", "3c", "4c", "5c", "6c", "7c")
    features <- c("1w", "2w", "3c")
    models <- c("svm")
    mfws <- c(400, 500, 600, 700, 800, 900, 1000)
  } else if (dataset_test[i_tests] == "test_7") {
    batch_tests <- c("large_samp_no_cull", "texts_no_cull")
    features <- c("3w", "3c",  "5c", "6c")
    models <- c("svm")
    mfws <- c(600, 700, 800, 900, 1000)
  } else if (dataset_test[i_tests] == "test_8") {
    batch_tests <- c("large_samp_no_cull")
    features <- c("2w", "3w", "5c", "6c")
    models <- c("svm")
    mfws <- c(200, 500, 800, 1000)
  } else if (dataset_test[i_tests] == "test_9") {
    batch_tests <- c("large_samp_no_cull", "texts_no_cull")
    features <- c("1w", "2w", "3w")
    models <- c("svm")
    mfws <- c(400, 500, 700, 800, 900, 1000)
  }
  
  for (i in 1:length(batch_tests)) {
    #get files for specific test
    result_files <- list.files(paste0("testing_test/ne/", dataset_test[i_tests], "/", batch_tests[i], "/"), full.names = TRUE)
    result_files_correct <- c()
    #remove files which don't meet criteria
    for (i_models in 1:length(models)) {
      result_files_correct <- c(result_files_correct, result_files[grep(models[i_models], result_files)])
    }
    result_files <- result_files_correct
    result_files_correct <- c()
    #remove features which don't meet criteria
    for (i_features in 1:length(features)) {
      result_files_correct <- c(result_files_correct, result_files[grep(features[i_features], result_files)])
    }
    #load file and get results
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
        #get current mfw
        current_mfw <- test$V1[test_cutoffs[2]+1]
        current_mfw <- as.numeric(substr(current_mfw, 1,4))
        if (current_mfw %!in% mfws) {
          test_cutoffs <- test_cutoffs[-c(1:2)]
          next
        }
        current_accuracy <- test$V2[test_cutoffs[2]+1]
        current_accuracy <- gsub("[(]", "", current_accuracy)
        current_accuracy <- gsub("[)]", "", current_accuracy)
        current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
        if (current_accuracy != 100) {
          test_cutoffs <- test_cutoffs[-c(1:2)]
          next
        }
        is_100 <- is_100+1
        
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
}


barplot(c(hume, ferg, carl, blair, smith, kames))

