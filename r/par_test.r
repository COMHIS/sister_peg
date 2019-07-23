# authorial attribtuion - PARTICULAR(?) TESTS? This was, I think, to test the Bumbo stuff.

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

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")

batch_tests <- c("texts_no_cull")

batch_tests <- c("large_samp_no_cull")

#char_word <- c("w", "w", "w", "c", "c", "c", "c", "c", "c")
#tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6", "7")
char_word <- c("w")
tok_num <- c("1")


#########################################################
for (x in 1:length(batch_tests)) {
  ####################################################
  #for (x in 3:length(batch_tests)) {
  
  
  if (grepl("no", batch_tests[x])) { culling.max.var <- 0 }
  if (grepl("with", batch_tests[x])) { culling.max.var <- 50 }
  if (grepl("small", batch_tests[x])) { samp.size.var <- 1988 }
  if (grepl("large", batch_tests[x])) { samp.size.var <- 7955 }
  if (grepl("texts", batch_tests[x])) { text.or.samp.var <- "FALSE"}
  if (grepl("samp", batch_tests[x])) { text.or.samp.var <- "normal.sampling"}
  # if (grepl("texts", batch_tests[x])) { training.corpus.dir.var <- "par_test/primary_set/test4_unmerged/"}
  # if (grepl("samp", batch_tests[x])) { training.corpus.dir.var <- "par_test/primary_set/test4_merged/"}
  # if (grepl("texts", batch_tests[x])) { test.corpus.dir.var <- "par_test/secondary_set/test4_unmerged/"}
  # if (grepl("samp", batch_tests[x])) { test.corpus.dir.var <- "par_test/secondary_set/test4_merged/"}
  if (grepl("texts", batch_tests[x])) { training.corpus.dir.var <- "primary_set_backup/test2_unmerged/"}
  if (grepl("samp", batch_tests[x])) { training.corpus.dir.var <- "primary_set_backup/test2_merged/"}
  if (grepl("texts", batch_tests[x])) { test.corpus.dir.var <- "secondary_set_backup/test2_unmerged/"}
  if (grepl("samp", batch_tests[x])) { test.corpus.dir.var <- "secondary_set_backup/test2_merged/"}
  
  
  for (i in 1:length(tok_num)) {
    #for (i in 7:length(tok_num)) {
    
    
    cat("\nRunning test", i, " out of ", length(tok_num))
    #Delta test
    classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
             ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
             , sampling=text.or.samp.var, sample.size=samp.size.var
             , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
             , training.corpus.dir = training.corpus.dir.var, test.corpus.dir = test.corpus.dir.var
             #, path = "par_test/"
    )
    results_fn <- paste0("par_test/results/", batch_tests[x], "/delta_", paste0(tok_num[i], char_word[i]), ".txt")
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
                              , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
                              #, path = "par_test/"
                              )
      if (current_knn_test == 1) {
        success_rate <- knn_results$overall.success.rate
        highest_score <- success_rate
        best_k <- current_knn_test
        results_fn <- paste0("par_test/results/", batch_tests[x], "/knn_", best_k, "_", paste0(tok_num[i], char_word[i]), ".txt")
        file.rename("final_results.txt", results_fn)
        current_knn_test <- current_knn_test + 1
        next
      } else {
        success_rate <- knn_results$overall.success.rate
        if (success_rate > highest_score) {
          highest_score <- success_rate
          best_k <- current_knn_test
          extra_runs <- 0
          file.remove(results_fn)
          results_fn <- paste0("par_test/results/", batch_tests[x], "/knn_", best_k, "_", paste0(tok_num[i], char_word[i]), ".txt")
          file.rename("final_results.txt", results_fn)
          current_knn_test <- current_knn_test + 1
        } else if (success_rate <= highest_score) {
          extra_runs <- extra_runs + 1
          current_knn_test <- current_knn_test + 1
        }
      }
      if (extra_runs == 10) { knn_test <- FALSE }
    }
    # #actual KNN test
    # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
    #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn",
    #          k.value=best_k,
    #          use.existing.freq.tables = TRUE
    #          , sampling=text.or.samp.var, sample.size=samp.size.var
    #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10)
    # results_fn <- paste0("results/", batch_tests[x], "/knn_", best_k, "_", paste0(tok_num[i], char_word[i]), ".txt")
    # file.rename("final_results.txt", results_fn)
    
    #NSC test
    classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
             ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc",
             use.existing.freq.tables = TRUE
             , sampling=text.or.samp.var, sample.size=samp.size.var
             , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
             #, path = "par_test/"
    )
    results_fn <- paste0("par_test/results/", batch_tests[x], "/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
    file.rename("final_results.txt", results_fn)
    
    #svm test
    classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
             ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm",
             svm.kernel="linear", svm.cost=1,
             use.existing.freq.tables = TRUE
             , sampling=text.or.samp.var, sample.size=samp.size.var
             , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
             #, path = "par_test/"
    )
    results_fn <- paste0("par_test/results/", batch_tests[x], "/svm_linear_", paste0(tok_num[i], char_word[i]), ".txt")
    file.rename("final_results.txt", results_fn)
    
    # #svm test with poly kernel
    # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
    #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm",
    #          svm.kernel="polynomial", svm.cost=1,
    #          use.existing.freq.tables = TRUE
    #          , sampling=text.or.samp.var, sample.size=samp.size.var
    #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10,
    #          path = "par_test/"
    # )
    # results_fn <- paste0("par_test/results/", batch_tests[x], "/svm_polynomial_", paste0(tok_num[i], char_word[i]), ".txt")
    # file.rename("final_results.txt", results_fn)
    # 
    # #SVM with radial kernel
    # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
    #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm",
    #          svm.kernel="radial", svm.cost=1,
    #          use.existing.freq.tables = TRUE
    #          , sampling=text.or.samp.var, sample.size=samp.size.var
    #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10,
    #          path = "par_test/"
    # )
    # results_fn <- paste0("par_test/results/", batch_tests[x], "/svm_radial_", paste0(tok_num[i], char_word[i]), ".txt")
    # file.rename("final_results.txt", results_fn)
    
      
    #naivebayes test
    classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
             ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes",
             use.existing.freq.tables = TRUE
             , sampling=text.or.samp.var, sample.size=samp.size.var
             , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
             #, path = "par_test/"
    )
    results_fn <- paste0("par_test/results/", batch_tests[x], "/nb_", paste0(tok_num[i], char_word[i]), ".txt")
    file.rename("final_results.txt", results_fn)
  }
}



#for final test

char_word <- c("w", "c")
tok_num <- c("1", "7")
batch_tests <- c("large_samp_no_cull", "texts_no_cull")


char_word <- c("c")
tok_num <- c("7")

batch_tests <- c("texts_no_cull")

for (x in 1:length(batch_tests)) {
  #for (x in 3:length(batch_tests)) {
  
  
  if (grepl("no", batch_tests[x])) { culling.max.var <- 0 }
  if (grepl("with", batch_tests[x])) { culling.max.var <- 50 }
  if (grepl("small", batch_tests[x])) { samp.size.var <- 1988 }
  if (grepl("large", batch_tests[x])) { samp.size.var <- 7955 }
  if (grepl("texts", batch_tests[x])) { text.or.samp.var <- "FALSE"}
  if (grepl("samp", batch_tests[x])) { text.or.samp.var <- "normal.sampling"}
  # if (grepl("texts", batch_tests[x])) { training.corpus.dir.var <- "par_test/primary_set/test4_unmerged/"}
  # if (grepl("samp", batch_tests[x])) { training.corpus.dir.var <- "par_test/primary_set/test4_merged/"}
  # if (grepl("texts", batch_tests[x])) { test.corpus.dir.var <- "par_test/secondary_set/test4_unmerged/"}
  # if (grepl("samp", batch_tests[x])) { test.corpus.dir.var <- "par_test/secondary_set/test4_merged/"}
  if (grepl("texts", batch_tests[x])) { training.corpus.dir.var <- "primary_set_backup/test2_unmerged/"}
  if (grepl("samp", batch_tests[x])) { training.corpus.dir.var <- "primary_set_backup/test2_merged/"}
  if (grepl("texts", batch_tests[x])) { test.corpus.dir.var <- "secondary_set_backup/test2_unmerged/"}
  if (grepl("samp", batch_tests[x])) { test.corpus.dir.var <- "secondary_set_backup//test2_merged/"}
  
  
  for (i in 1:length(tok_num)) {
    #for (i in 7:length(tok_num)) {
    
    #svm test
    classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
             ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm",
             svm.kernel="linear", svm.cost=1,
             use.existing.freq.tables = FALSE
             , training.corpus.dir = training.corpus.dir.var, test.corpus.dir = test.corpus.dir.var
             , sampling=text.or.samp.var, sample.size=samp.size.var
             , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
             #, path = "par_test/"
    )
    results_fn <- paste0("par_test/results/", batch_tests[x], "/svm_linear_", paste0(tok_num[i], char_word[i]), ".txt")
    file.rename("final_results.txt", results_fn)
    
    
  }
}
