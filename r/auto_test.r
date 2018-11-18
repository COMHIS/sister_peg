# authorial attribtuion

require(readtext) 
require(stringi) 
require(stringr) 
require(magrittr) 
library(quanteda)
library(tm)
require(stringdist)
library(sm)
library(ggplot2)
library(stylo)

setwd("~/Dropbox/R/HF_PC/")
setwd("~/Dropbox/github/sister_peg/")  

options(scipen=999)  # turn off scientific notation like 1e+06
#options(stringsAsFactors = FALSE)

########################################################################################
# Script runs many tests using many measurements, methods, texts, etc... takes a while
###################################################################################

batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")
char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")

#for (x in 6:length(batch_tests)) {
for (x in 1:4) {
  if (grepl("no", batch_tests[x])) { culling.max.var <- 0 }
  if (grepl("with", batch_tests[x])) { culling.max.var <- 50 }
  if (grepl("small", batch_tests[x])) { samp.size.var <- 1988 }
  if (grepl("large", batch_tests[x])) { samp.size.var <- 7955 }
  if (grepl("texts", batch_tests[x])) { text.or.samp.var <- "FALSE"}
  if (grepl("samp", batch_tests[x])) { text.or.samp.var <- "normal.sampling"}
  if (grepl("texts", batch_tests[x])) { training.corpus.dir.var <- "primary_set/unmerged/"}
  if (grepl("samp", batch_tests[x])) { training.corpus.dir.var <- "primary_set/"}
  if (grepl("texts", batch_tests[x])) { test.corpus.dir.var <- "secondary_set/unmerged/"}
  if (grepl("samp", batch_tests[x])) { test.corpus.dir.var <- "secondary_set/"}
  
  
  for (i in 1:length(tok_num)) {
    cat("\nRunning test", i, " out of ", length(tok_num))
    #Delta test
    classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
             ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
             , sampling=text.or.samp.var, sample.size=samp.size.var
             , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
             , training.corpus.dir = training.corpus.dir.var, test.corpus.dir = test.corpus.dir.var
    )
    results_fn <- paste0("results/", batch_tests[x], "/delta_", paste0(tok_num[i], char_word[i]), ".txt")
    file.rename("final_results.txt", results_fn)
    
    #Knn test
    knn_test <- TRUE
    current_knn_test <- 1
    best_k <- 0
    highest_score <- 0
    extra_runs <- 0
    
    while(knn_test == TRUE) {
     
      knn_results <- classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
                              ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
                              k.value=current_knn_test,
                              use.existing.freq.tables = TRUE
                              , sampling=text.or.samp.var, sample.size=samp.size.var
                              , culling.min = 0, culling.max = culling.max.var, culling.incr = 10)
      if (current_knn_test == 1) { 
        success_rate <- knn_results$overall.success.rate 
        highest_score <- success_rate
        best_k <- current_knn_test
        current_knn_test <- current_knn_test + 1
        next
      } else { 
        success_rate <- knn_results$overall.success.rate 
        if (success_rate > highest_score) { 
          highest_score <- success_rate 
          best_k <- current_knn_test
          extra_runs <- 0
        } else if (success_rate < highest_score) {
          extra_runs <- extra_runs + 1
        }
        
        current_knn_test <- current_knn_test + 1
      }
      if (extra_runs == 10) { knn_test <- FALSE }
    }
    #actual KNN test
    knn_results <- classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
                            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
                            k.value=best_k,
                            use.existing.freq.tables = TRUE
                            , sampling=text.or.samp.var, sample.size=samp.size.var
                            , culling.min = 0, culling.max = culling.max.var, culling.incr = 10)
    results_fn <- paste0("results/", batch_tests[x], "/knn_", best_k, "_", paste0(tok_num[i], char_word[i]), ".txt")
    file.rename("final_results.txt", results_fn)
    
    # #NSC test
    # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
    #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
    #          use.existing.freq.tables = TRUE
    #          , sampling=text.or.samp.var, sample.size=samp.size.var
    #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
    # )
    # results_fn <- paste0("results/", batch_tests[x], "/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
    # file.rename("final_results.txt", results_fn)
    # #svm test
    # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
    #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
    #          svm.kernel="linear", svm.cost=1,
    #          use.existing.freq.tables = TRUE
    #          , sampling=text.or.samp.var, sample.size=samp.size.var
    #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
    # )
    # results_fn <- paste0("results/", batch_tests[x], "/svm_", paste0(tok_num[i], char_word[i]), ".txt")
    # file.rename("final_results.txt", results_fn)
    # #naivebayes test
    # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
    #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
    #          use.existing.freq.tables = TRUE
    #          , sampling=text.or.samp.var, sample.size=1988
    #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
    # )
    # results_fn <- paste0("results/", batch_tests[x], "/nb_", paste0(tok_num[i], char_word[i]), ".txt")
    # file.rename("final_results.txt", results_fn)
  }
}


######################################################################################
# Extracting data from previous tests for analysis
#####################################################################################

#TODO: 
#
#With and without culling?
#Sections


###############################
# create and load tests by model

batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")
char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
tests <- c("delta", "knn", "nb", "nsc", "svm")
accuracies <- c(100,95,90,85,80,79)

total_model_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
                      ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
                      hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)

for (i in 1:length(batch_tests)) {
  #get results  
  result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
                        ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
                        hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)
  

  for (x in 1:length(result_files)) {
    cat("\r", x)
    test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
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
      #remove non sister peg tests
      if (length(grep("merged", temp_results$V1)) != 0) {
        temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
        temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
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
            next
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

######################
#create and load test by features

char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
tests <- c("delta", "knn", "nb", "nsc", "svm")
accuracies <- c(100,95,90,85,80,79)
for (i in 1:length(char_word)) {
  temp_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
                             ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
                             hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)  
  assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
}

total_feature_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
                                    ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
                                    hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)  



for (i in 1:length(batch_tests)) {
  #get results  
  result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
  for (x in 1:length(char_word)) {
    temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
    cat("\r", x)
    file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
    for (y in 1:length(file_locs)) {
      test <- read.delim(result_files[file_locs[y]], header = FALSE, stringsAsFactors = FALSE)
      
      for(z in 1:length(tests)) { 
        if(grepl(tests[z], result_files[file_locs[i]])) {
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
          next
        }
        temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
        #remove non sister peg tests
        if (length(grep("Hume_", temp_results$V1)) != 0) {
          temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
        }
        if (length(grep("Ferguson_", temp_results$V1)) != 0) {
          temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
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
              next
            }
          }
        }
        
        temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] <-
          temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] + length(grep("Hume", temp_results$V3))
        
        temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] <-
          temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] + length(grep("Ferg", temp_results$V3))
        test_cutoffs <- test_cutoffs[-c(1:2)]
        
        assign(paste0(char_word[x], tok_num[x], "_results"), temp_total_results)
        
        
        total_feature_results[grep(current_test, row.names(total_feature_results)),grep(paste0("hume_", accuracy_group), names(total_feature_results))] <-
          total_feature_results[grep(current_test, row.names(total_feature_results)),grep(paste0("hume_", accuracy_group), names(total_feature_results))] + length(grep("Hume", temp_results$V3))
        
        total_feature_results[grep(current_test, row.names(total_feature_results)),grep(paste0("ferg_", accuracy_group), names(total_feature_results))] <-
          total_feature_results[grep(current_test, row.names(total_feature_results)),grep(paste0("ferg_", accuracy_group), names(total_feature_results))] + length(grep("Ferg", temp_results$V3))
        next
      }
    }
  }
}

######################
#create and load test by datasets


#plot accuracy levels of tests
accuracy_df <- data.frame("100"=rep(0,5), "95"=rep(0,5), "90"=rep(0,5), "85"=rep(0,5), "80"=rep(0,5), "79"=rep(0,5), row.names = tests)

for (i in 1:length(char_word)) {
  temp_total_results <- get(paste0(char_word[i], tok_num[i], "_results"))
  for (x in 1:length(accuracies)) {
    for (z in 1:length(tests)) {
      col_pull_from <- grep(accuracies[x], names(temp_total_results))
      row_pull_from <- grep(tests[z], row.names(temp_total_results))
      total <- sum(temp_total_results[row_pull_from, col_pull_from])
      col_push_to <- grep(accuracies[x], names(accuracy_df))
      row_push_to <- grep(tests[z], row.names(accuracy_df))
      accuracy_df[row_push_to, col_push_to] <- sum(accuracy_df[row_push_to, col_push_to], total)
    }
  }  
}

###############################################
# Create table of results for chunks of Sister Peg






#plot accuracy levels of features
feature_types <- c()
for (i in 1:length(char_word)) {
  feature_types <- c(feature_types, paste0(char_word[i], tok_num[i]))
}

feature_accuracy_df <- data.frame("100"=rep(0,8), "95"=rep(0,8), "90"=rep(0,8), "85"=rep(0,8), "80"=rep(0,8), "79"=rep(0,8), row.names = feature_types)

for (i in 1:length(char_word)) {
  temp_total_results <- get(paste0(char_word[i], tok_num[i], "_results"))
  row_to_push_to <- grep(paste0(char_word[i], tok_num[i]), row.names(feature_accuracy_df))
  for (x in 1:length(accuracies)) {
    col_pull_from <- grep(accuracies[x], names(temp_total_results))
    total <- sum(temp_total_results[,col_pull_from])
    col_push_to <- grep(accuracies[x], names(feature_accuracy_df))
    feature_accuracy_df[row_to_push_to, col_push_to] <- sum(feature_accuracy_df[row_to_push_to, col_push_to], total)
  }
}  
































############################# BELOW IS OLD


knn_test <- TRUE
current_knn_test <- 1
best_k <- 0
highest_score <- 0
ks <- c()
extra_runs <- 0


for(i in 1:10) {
  knn_results <- classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
                          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
                          k.value=current_knn_test,
                          use.existing.freq.tables = TRUE
                          , sampling=text.or.samp.var, sample.size=samp.size.var
                          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10)
  if (current_knn_test == 1) { 
    success_rate <- knn_results$overall.success.rate 
    current_knn_test <- current_knn_test + 1
    ks <- c(success_rate)
    next
  } else { 
    success_rate <- knn_results$overall.success.rate 
    if (success_rate > highest_score) { 
      highest_score <- success_rate 
      best_k <- current_knn_test
    } else if (success_rate < highest_score) {
      extra_runs <- extra_runs + 1
    }
    ks <- c(ks, success_rate)
    current_knn_test <- current_knn_test + 1
  }
  if (extra_runs == 3) { knn_test <- FALSE }
}






######################################################################################################
#RUN TESTS - SAMPLES NO CULLING
for (i in 1:length(tok_num)) {
  cat("\nRunning test", i, " out of ", length(tok_num))
  #Delta test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
           , sampling="normal.sampling", sample.size=1988
           )
  results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)

  #Knn test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
           k.value=10,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           )
  
  results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)

  #NSC test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           )
  results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #svm test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
           svm.kernel="linear", svm.cost=1,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           )
  results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #naivebayes test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           )
  results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
}

######################################################################################################
#RUN TESTS - SAMPLES WITH CULLING
for (i in 1:length(tok_num)) {
  cat("\nRunning test", i, " out of ", length(tok_num))
  #Delta test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
           , sampling="normal.sampling", sample.size=1988
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #Knn test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
           k.value=10,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  
  results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #NSC test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #svm test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
           svm.kernel="linear", svm.cost=1,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #naivebayes test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=1988
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
}




######################################################################################################
#RUN TESTS - LARGE SAMPLES NO CULLING
for (i in 1:length(tok_num)) {
  cat("\nRunning test", i, " out of ", length(tok_num))
  #Delta test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
           , sampling="normal.sampling", sample.size=7955
  )
  results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #Knn test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
           k.value=10,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
  )
  
  results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #NSC test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
  )
  results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #svm test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
           svm.kernel="linear", svm.cost=1,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
  )
  results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #naivebayes test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
  )
  results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
}

######################################################################################################
#RUN TESTS - LARGE SAMPLES WITH CULLING
for (i in 1:length(tok_num)) {
  cat("\nRunning test", i, " out of ", length(tok_num))
  #Delta test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
           , sampling="normal.sampling", sample.size=7955
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #Knn test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
           k.value=10,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  
  results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #NSC test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #svm test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
           svm.kernel="linear", svm.cost=1,
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #naivebayes test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
           use.existing.freq.tables = TRUE
           , sampling="normal.sampling", sample.size=7955
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
}





######################################################################################################
#RUN TESTS - NO SAMPLES WITH CULLING
for (i in 1:length(tok_num)) {
  cat("\nRunning test", i, " out of ", length(tok_num))
  #Delta test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #Knn test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
           k.value=1,
           use.existing.freq.tables = TRUE
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  
  results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #NSC test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #svm test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
           svm.kernel="linear", svm.cost=1,
           use.existing.freq.tables = TRUE
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #naivebayes test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
           use.existing.freq.tables = TRUE
           , culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
}

######################################################################################################
#RUN TESTS - NO SAMPLES NO CULLING
for (i in 1:length(tok_num)) {
  cat("\nRunning test", i, " out of ", length(tok_num))
  #Delta test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
           #, culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #Knn test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
           k.value=1,
           use.existing.freq.tables = TRUE
           #, culling.min = 0, culling.max = 40, culling.incr = 10
  )
  
  results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  #NSC test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE
           #, culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #svm test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
           svm.kernel="linear", svm.cost=1,
           use.existing.freq.tables = TRUE
           #, culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #naivebayes test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
           use.existing.freq.tables = TRUE
           #, culling.min = 0, culling.max = 40, culling.incr = 10
  )
  results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
}



FIND THE SECTIONS WHICH ARE MOST COMMONLY ATTRIBUTED TO HUME/FERG

Plot H v F 

#importing data

tests <- c("delta", "knn", "nb", "nsc", "svm")
accuracies <- c(100,95,90,85,80,79)
results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
                      ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
                      hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)

result_files <- list.files("results/", full.names = TRUE)

for (x in 1:length(result_files)) {
  cat("\r", x)
  test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
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
      next
    }
    temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
    #remove non sister peg tests
    if (length(grep("merged", temp_results$V1)) != 0) {
      temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
      temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
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
          next
        }
      }
    }
    
    results[grep(current_test, row.names(results)),grep(paste0("hume_", accuracy_group), names(results))] <-
      results[grep(current_test, row.names(results)),grep(paste0("hume_", accuracy_group), names(results))] + length(grep("Hume", temp_results$V3))
    
    results[grep(current_test, row.names(results)),grep(paste0("ferg_", accuracy_group), names(results))] <-
      results[grep(current_test, row.names(results)),grep(paste0("ferg_", accuracy_group), names(results))] + length(grep("Ferg", temp_results$V3))
    test_cutoffs <- test_cutoffs[-c(1:2)]
    next
  }
}


#load by feature test

char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
tests <- c("delta", "knn", "nb", "nsc", "svm")
accuracies <- c(100,95,90,85,80,79)
for (i in 1:length(char_word)) {
  temp_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
                        ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
                        hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)  
  assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
}


result_files <- list.files("results/", full.names = TRUE)

for (x in 1:length(char_word)) {
  temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
  cat("\r", x)
  file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
  for (i in 1:length(file_locs)) {
    test <- read.delim(result_files[file_locs[i]], header = FALSE, stringsAsFactors = FALSE)
    
    for(z in 1:length(tests)) { 
      if(grepl(tests[z], result_files[file_locs[i]])) {
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
        next
      }
      temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
      #remove non sister peg tests
      if (length(grep("Hume_", temp_results$V1)) != 0) {
        temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
      }
      if (length(grep("Ferguson_", temp_results$V1)) != 0) {
        temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
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
            next
          }
        }
      }
      
      temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] <-
        temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] + length(grep("Hume", temp_results$V3))
      
      temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] <-
        temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] + length(grep("Ferg", temp_results$V3))
      test_cutoffs <- test_cutoffs[-c(1:2)]
      
      assign(paste0(char_word[x], tok_num[x], "_results"), temp_total_results)
      next
    }
  }
}





#plot accuracy levels of tests
accuracy_df <- data.frame("100"=rep(0,5), "95"=rep(0,5), "90"=rep(0,5), "85"=rep(0,5), "80"=rep(0,5), "79"=rep(0,5), row.names = tests)

for (i in 1:length(char_word)) {
  temp_total_results <- get(paste0(char_word[i], tok_num[i], "_results"))
  for (x in 1:length(accuracies)) {
    for (z in 1:length(tests)) {
      col_pull_from <- grep(accuracies[x], names(temp_total_results))
      row_pull_from <- grep(tests[z], row.names(temp_total_results))
      total <- sum(temp_total_results[row_pull_from, col_pull_from])
      col_push_to <- grep(accuracies[x], names(accuracy_df))
      row_push_to <- grep(tests[z], row.names(accuracy_df))
      accuracy_df[row_push_to, col_push_to] <- sum(accuracy_df[row_push_to, col_push_to], total)
    }
  }  
}

#plot accuracy levels of features
feature_types <- c()
for (i in 1:length(char_word)) {
  feature_types <- c(feature_types, paste0(char_word[i], tok_num[i]))
}

feature_accuracy_df <- data.frame("100"=rep(0,8), "95"=rep(0,8), "90"=rep(0,8), "85"=rep(0,8), "80"=rep(0,8), "79"=rep(0,8), row.names = feature_types)

for (i in 1:length(char_word)) {
  temp_total_results <- get(paste0(char_word[i], tok_num[i], "_results"))
  row_to_push_to <- grep(paste0(char_word[i], tok_num[i]), row.names(feature_accuracy_df))
  for (x in 1:length(accuracies)) {
    col_pull_from <- grep(accuracies[x], names(temp_total_results))
    total <- sum(temp_total_results[,col_pull_from])
    col_push_to <- grep(accuracies[x], names(feature_accuracy_df))
    feature_accuracy_df[row_to_push_to, col_push_to] <- sum(feature_accuracy_df[row_to_push_to, col_push_to], total)
  }
}  








hume_results <- results[c(1,3,5,7,9,11)]
ferg_results <- results[c(2,4,6,7,8,12)]

plot(hume_results[1,], ferg_results[1,])

#importing data as one

bad_results <- data.frame(name=c("hume", "ferg"), results=0)
good_results <- data.frame(name=c("hume", "ferg"), results=0)
result_files <- list.files("results/", full.names = TRUE)
tests <- c("delta", "knn", "nb", "nsc", "svm")

for (x in 1:length(result_files)) {
  test <- read.delim(result_files[x], header = FALSE)
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
    #temp!
    #temp_results <- temp_results(
    if(any(grepl("Ferguson_history_eb", temp_results$V1))) { temp_results <- temp_results[-grepl("Ferguson_history_eb", temp_results$V1),] }
    #temp!
    if (any(grepl("merged", temp_results$V1))) {
      bad_results$results[1] <- bad_results$results[1] + length(grep("Hume", temp_results$V3))
      bad_results$results[2] <- bad_results$results[2] + length(grep("Ferguson", temp_results$V3))
      test_cutoffs <- test_cutoffs[-c(1:2)]
      next
    } else {
      good_results$results[1] <- good_results$results[1] + length(grep("Hume", temp_results$V3))
      good_results$results[2] <- good_results$results[2] + length(grep("Ferguson", temp_results$V3))
      test_cutoffs <- test_cutoffs[-c(1:2)]
      next
    }
  }
}








ggplot(plot_df) +
  geom_bar(stat="identity", aes(x = authors, y= f1_50), fill = "red") +
  geom_bar(stat="identity", aes(x = authors, y= f1_55), fill = "grey70") +
  geom_bar(stat="identity", aes(x = authors, y= f1_60), fill = "grey60") +
  geom_bar(stat="identity", aes(x = authors, y= f1_65), fill = "grey50") +
  geom_bar(stat="identity", aes(x = authors, y= f1_70), fill = "grey40") +
  geom_bar(stat="identity", aes(x = authors, y= f1_75), fill = "grey30") +
  geom_bar(stat="identity", aes(x = authors, y= f1_80), fill = "grey20") +
  geom_bar(stat="identity", aes(x = authors, y= f1_85), fill = "grey10") +
  geom_bar(stat="identity", aes(x = authors, y= f1_90), fill = "black") +
  #theme(axis.title.x = element_blank())  +
  labs(title = "Collocations in F1-Score Corpora") +
  ylab("Total collocations as ratio") +
  xlab("F1-Score Corpora") 
+
  coord_flip()

+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #FIRST ATTEMPT< NOT GOOD
  
  
  authors <- tail(names(sort(table(author_df$author))), 10)

#create author corpora
for (i in 1:10) {
  temp_df <- author_df[which(author_df$author == authors[i]),]
  temp_name <- paste0("author_corp_", i)
  assign(temp_name, temp_df)
}

#craete author sub-corpora
temp_names <- c()
for (i in 1:10) { temp_names <- c(temp_names, paste0("author_corp_", i)) }
f1s <- c("50", "60", "65", "70", "75", "80", "85", "90")

for (i in 1:10) {
  cat("\r", i)
  temp_total_corp <- get(temp_names[i])
  for (x in 1:length(f1s)) {
    if (x == length(f1s)) {
      temp_doc <- temp_total_corp[which(temp_total_corp$f1 >= (as.numeric(f1s[x])/100)),]
    } else if (x == 1) {
      temp_doc <- temp_total_corp[which(temp_total_corp$f1 <= (as.numeric(f1s[x])/100)),]
    } else {
      temp_doc <- temp_total_corp[which(temp_total_corp$f1 >= (as.numeric(f1s[x])/100) & temp_total_corp$f1 < (as.numeric(f1s[x+1])/100)),]
    }
    temp_doc_name <- paste0("author_", i, "_f1_", f1s[x])
    assign(temp_doc_name, temp_doc)
  }
}

#write txt files to be used by stylo
for (i in 1:length(authors)) {
  cat("\r", i)
  for (x in 1:length(f1s)) {
    temp_doc <- get(paste0("author_", i, "_f1_", f1s[x]))
    temp_text_ocr <- paste(temp_doc$ocr, collapse = " ")
    temp_text_tcp <- paste(temp_doc$tcp, collapse = " ")
    author_name <- gsub(", .*$", "", authors[i])
    temp_file_tcp <- paste0("data/aa/", i, "/", author_name, "_tcp_", f1s[x], ".txt")
    temp_file_ocr <- paste0("data/aa/", i, "/", author_name, "_ocr_", f1s[x], ".txt")
    fileConn<-file(temp_file_tcp)
    writeLines(temp_text_tcp, fileConn)
    close(fileConn)
    fileConn<-file(temp_file_ocr)
    writeLines(temp_text_ocr, fileConn)
    close(fileConn)
  }
}

authors_pages_df <- data.frame(author=character(80), author_id=character(80), f1_group=character(80), total_pages=numeric(80),
                               stringsAsFactors = FALSE)
for (i in 1:length(authors)) {
  for (x in 1:8) {
    if (i == 1) {
      authors_pages_df$author[x] <- authors[i]
      authors_pages_df$author_id[x] <- i
      authors_pages_df$f1_group[x] <- f1s[x]
      temp_doc <- get(paste0("author_", i, "_f1_", f1s[x]))
      authors_pages_df$total_pages[x] <- nrow(temp_doc)
    } else {
      authors_pages_df$author[(i-1)*8+x] <- authors[i]
      authors_pages_df$author_id[(i-1)*8+x] <- i
      authors_pages_df$f1_group[(i-1)*8+x] <- f1s[x]
      temp_doc <- get(paste0("author_", i, "_f1_", f1s[x]))
      authors_pages_df$total_pages[(i-1)*8+x] <- nrow(temp_doc)
    }
  }
}
write.csv(authors_pages_df, file="data/aa/authors_pages.csv")
plot(authors_pages_df$f1_group, authors_pages_df$total_pages)

plot_authors <- authors_pages_df
plot_authors$author_id <- as.factor(plot_authors$author)

ggplot(authors_pages_df, aes(f1_group, total_pages)) +
  geom_point(aes(color=author), size=5)


Randomly select pages to make up number of words FOR BOTH TCP AND OCR. 
Remove pages from TCP/OCR files
Save new text files for tcp/ocr
move all files into correct directors

2) Run Stylo variations


3) save results somewhere
