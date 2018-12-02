#check all 100%

setwd("Dropbox/github/sister_peg/")

batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")

batch_tests <- c("small_samp_with_cull", "large_samp_with_cull", "texts_with_cull")

ferg <- 0
hume <- 0
carl <- 0
blair <- 0
kames <- 0
smith <- 0

for (i in 1:length(batch_tests)) {
  #result_files <- list.files(paste0("test1_results/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test2_results/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test3_results_car/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test4_results_others/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test5_results_scots/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("test7_results_kames/", batch_tests[i], "/"), full.names = TRUE)
  result_files <- list.files(paste0("test6_results_blair/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  #result_files <- list.files(paste0("par_test/results/", batch_tests[i], "/"), full.names = TRUE)
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
      
      #remove non-EB rsults
      if (any(grepl("EB", temp_results$V1))) {
        temp_results <- temp_results[-c(which(!grepl("EB", temp_results$V1))),]
      }
      # 
      #remove non-SP rsults
      #temp_results <- temp_results[-c(which(!grepl("sister", temp_results$V1))),]
      
      
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
