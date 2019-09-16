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
setwd("github/sister_peg/")

options(scipen=999)  # turn off scientific notation like 1e+06
#options(stringsAsFactors = FALSE)

########################################################################################
# Script runs many tests using many measurements, methods, texts, etc... takes a while
###################################################################################

batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull", "texts_with_cull")

batch_tests <- c("small_samp_no_cull", "large_samp_no_cull", "texts_no_cull")
batch_tests <- c("small_samp_no_cull", "texts_no_cull")

char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "3", "4", "5", "6", "7")

#tests 1-6

# primary_sets_merged <- c("back_up_test_data/primary_set_backup/test1_merged/", "back_up_test_data/primary_set_backup/test2_merged/", "back_up_test_data/primary_set_backup/test3_merged/",
#                          "back_up_test_data/primary_set_backup/test4_merged/", "back_up_test_data/primary_set_backup/test5_merged/", "back_up_test_data/primary_set_backup/test6_merged/")
# primary_sets_unmerged <- c("back_up_test_data/primary_set_backup/test1_unmerged/", "back_up_test_data/primary_set_backup/test2_unmerged/", "back_up_test_data/primary_set_backup/test3_unmerged/",
#                            "back_up_test_data/primary_set_backup/test4_unmerged/", "back_up_test_data/primary_set_backup/test5_unmerged/", "back_up_test_data/primary_set_backup/test6_unmerged/")
# 
# secondary_sets_merged <- c("back_up_test_data/secondary_set_backup/test1_merged/", "back_up_test_data/secondary_set_backup/test2_merged/", "back_up_test_data/secondary_set_backup/test3_merged/",
#                            "back_up_test_data/secondary_set_backup/test4_merged/", "back_up_test_data/secondary_set_backup/test5_merged/", "back_up_test_data/secondary_set_backup/test6_merged/")
# secondary_sets_unmerged <- c("back_up_test_data/secondary_set_backup/test1_unmerged/", "back_up_test_data/secondary_set_backup/test2_unmerged/", "back_up_test_data/secondary_set_backup/test3_unmerged/", 
#                              "back_up_test_data/secondary_set_backup/test4_unmerged/", "back_up_test_data/secondary_set_backup/test5_unmerged/", "back_up_test_data/secondary_set_backup/test6_unmerged/")
# 
# primary_sets_merged_edited <- c("back_up_test_data/primary_set_backup_edited/test1_merged/", "back_up_test_data/primary_set_backup_edited/test2_merged/", "back_up_test_data/primary_set_backup_edited/test3_merged/", 
#                                 "back_up_test_data/primary_set_backup_edited/test4_merged/", "back_up_test_data/primary_set_backup_edited/test5_merged/", "back_up_test_data/primary_set_backup_edited/test6_merged/")
# primary_sets_unmerged_edited <- c("back_up_test_data/primary_set_backup_edited/test1_unmerged/", "back_up_test_data/primary_set_backup_edited/test2_unmerged/", "back_up_test_data/primary_set_backup_edited/test3_unmerged/", 
#                                   "back_up_test_data/primary_set_backup_edited/test4_unmerged/", "back_up_test_data/primary_set_backup_edited/test5_unmerged/", "back_up_test_data/primary_set_backup_edited/test6_unmerged/")
# secondary_sets_merged_edited <- c("back_up_test_data/secondary_set_backup_edited/test1_merged/", "back_up_test_data/secondary_set_backup_edited/test2_merged/", "back_up_test_data/secondary_set_backup_edited/test3_merged/",
#                                   "back_up_test_data/secondary_set_backup_edited/test4_merged/", "back_up_test_data/secondary_set_backup_edited/test5_merged/", "back_up_test_data/secondary_set_backup_edited/test6_merged/")
# secondary_sets_unmerged_edited <- c("back_up_test_data/secondary_set_backup_edited/test1_unmerged/", "back_up_test_data/secondary_set_backup_edited/test2_unmerged/", "back_up_test_data/secondary_set_backup_edited/test3_unmerged/", 
#                                     "back_up_test_data/secondary_set_backup_edited/test4_unmerged/", "back_up_test_data/secondary_set_backup_edited/test5_unmerged/", "back_up_test_data/secondary_set_backup_edited/test6_unmerged/")

#tests 7 - 9

# primary_sets_merged <- c("back_up_test_data/primary_set_backup/test7_merged/", "back_up_test_data/primary_set_backup/test8_merged/", "back_up_test_data/primary_set_backup/test9_merged/")
# primary_sets_unmerged <- c("back_up_test_data/primary_set_backup/test7_unmerged/", "back_up_test_data/primary_set_backup/test8_unmerged/", "back_up_test_data/primary_set_backup/test9_unmerged/")
# 
# secondary_sets_merged <- c("back_up_test_data/secondary_set_backup/test7_merged/", "back_up_test_data/secondary_set_backup/test8_merged/", "back_up_test_data/secondary_set_backup/test9_merged/")
# secondary_sets_unmerged <- c("back_up_test_data/secondary_set_backup/test7_unmerged/", "back_up_test_data/secondary_set_backup/test8_unmerged/", "back_up_test_data/secondary_set_backup/test9_unmerged/")
# 
# primary_sets_merged_edited <- c("back_up_test_data/primary_set_backup_edited/test7_merged/", "back_up_test_data/primary_set_backup_edited/test8_merged/", "back_up_test_data/primary_set_backup_edited/test9_merged/")
# primary_sets_unmerged_edited <- c("back_up_test_data/primary_set_backup_edited/test7_unmerged/", "back_up_test_data/primary_set_backup_edited/test8_unmerged/", "back_up_test_data/primary_set_backup_edited/test9_unmerged/")
# 
# secondary_sets_merged_edited <- c("back_up_test_data/secondary_set_backup_edited/test7_merged/", "back_up_test_data/secondary_set_backup_edited/test8_merged/", "back_up_test_data/secondary_set_backup_edited/test9_merged/")
# secondary_sets_unmerged_edited <- c("back_up_test_data/secondary_set_backup_edited/test7_unmerged/", "back_up_test_data/secondary_set_backup_edited/test8_unmerged/", "back_up_test_data/secondary_set_backup_edited/test9_unmerged/")

#tests 10 - 11

primary_sets_merged <- c("back_up_test_data/primary_set_backup/test10_merged/", "back_up_test_data/primary_set_backup/test11_merged/")
primary_sets_unmerged <- c("back_up_test_data/primary_set_backup/test10_unmerged/", "back_up_test_data/primary_set_backup/test11_unmerged/")

secondary_sets_merged <- c("back_up_test_data/secondary_set_backup/test10_merged/", "back_up_test_data/secondary_set_backup/test11_merged/")
secondary_sets_unmerged <- c("back_up_test_data/secondary_set_backup/test10_unmerged/", "back_up_test_data/secondary_set_backup/test11_unmerged/")

# primary_sets_merged_edited <- c("back_up_test_data/primary_set_backup_edited/test7_merged/", "back_up_test_data/primary_set_backup_edited/test8_merged/", "back_up_test_data/primary_set_backup_edited/test9_merged/")
# primary_sets_unmerged_edited <- c("back_up_test_data/primary_set_backup_edited/test7_unmerged/", "back_up_test_data/primary_set_backup_edited/test8_unmerged/", "back_up_test_data/primary_set_backup_edited/test9_unmerged/")
# 
# secondary_sets_merged_edited <- c("back_up_test_data/secondary_set_backup_edited/test7_merged/", "back_up_test_data/secondary_set_backup_edited/test8_merged/", "back_up_test_data/secondary_set_backup_edited/test9_merged/")
# secondary_sets_unmerged_edited <- c("back_up_test_data/secondary_set_backup_edited/test7_unmerged/", "back_up_test_data/secondary_set_backup_edited/test8_unmerged/", "back_up_test_data/secondary_set_backup_edited/test9_unmerged/")


for (num_of_test in 1:length(primary_sets_unmerged)) {
  #####################################################
  #1 for unedited, 2 for edident! CHANGE IF RERUNNING
  #######################################################
  for (mod_text in 1:2) {
  #prepare test data
    if (mod_text == 1) {
      #delete old files
      to_delete <- list.files("primary_set/", full.names = TRUE, recursive = TRUE)
      file.remove(c(to_delete))
      to_delete <- list.files("secondary_set/", full.names = TRUE, recursive = TRUE)
      file.remove(c(to_delete))
      
      #create dir
      dir.create("primary_set/unmerged")
      dir.create("secondary_set/unmerged")
      
      #move test data
      files_move <- list.files(primary_sets_merged[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "primary_set/")
      
      files_move <- list.files(primary_sets_unmerged[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "primary_set/unmerged/")
      
      files_move <- list.files(secondary_sets_merged[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "secondary_set/")
      
      files_move <- list.files(secondary_sets_unmerged[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "secondary_set/unmerged/")
      
    } else if (mod_text == 2) {
      #delete old files
      to_delete <- list.files("primary_set/", full.names = TRUE)
      file.remove(c(to_delete))
      to_delete <- list.files("secondary_set/", full.names = TRUE)
      file.remove(c(to_delete))
      
      #create dir
      dir.create("primary_set/unmerged")
      dir.create("secondary_set/unmerged")
      
      #move test data
      files_move <- list.files(primary_sets_merged_edited[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "primary_set/")
      
      files_move <- list.files(primary_sets_unmerged_edited[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "primary_set/unmerged/")
      
      files_move <- list.files(secondary_sets_merged_edited[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "secondary_set/")
      
      files_move <- list.files(secondary_sets_unmerged_edited[num_of_test], full.names = TRUE)
      file.copy(c(files_move), "secondary_set/unmerged/")
    }
    for (x in 1:length(batch_tests)) {
      
      
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
      #for (i in 7:length(tok_num)) {
      
      
        cat("\nRunning test", i, " out of ", length(tok_num))
        #Delta test
        
        classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
                 ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
                 , sampling=text.or.samp.var, sample.size=samp.size.var
                 , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
                 , training.corpus.dir = training.corpus.dir.var, test.corpus.dir = test.corpus.dir.var
        )
        if(mod_text == 1) {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/ne_delta_", paste0(tok_num[i], char_word[i]), ".txt")
        } else {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/e_delta_", paste0(tok_num[i], char_word[i]), ".txt")
        }
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
            if(mod_text == 1) {
              results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/ne_knn_", paste0(tok_num[i], char_word[i]), "_", best_k, "ks.txt")
            } else {
              results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/e_knn_", paste0(tok_num[i], char_word[i]), "_", best_k, "ks.txt")
            }
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
              if(mod_text == 1) {
                results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/ne_knn_", paste0(tok_num[i], char_word[i]), "_", best_k, "ks.txt")
              } else {
                results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/e_knn_", paste0(tok_num[i], char_word[i]), "_", best_k, "ks.txt")
              }
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
        )
        if(mod_text == 1) {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/ne_nsc_", paste0(tok_num[i], char_word[i]), ".txt")
        } else {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/e_nsc_", paste0(tok_num[i], char_word[i]), ".txt")
        }
        file.rename("final_results.txt", results_fn)
        
        #svm test
        classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
                 ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm",
                 svm.kernel="linear", svm.cost=1,
                 #####################################
                 use.existing.freq.tables = TRUE
                 #use.existing.freq.tables = FALSE
                 , sampling=text.or.samp.var, sample.size=samp.size.var
                 , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
        )
        if(mod_text == 1) {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/ne_svm_", paste0(tok_num[i], char_word[i]), ".txt")
        } else {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/e_svm_", paste0(tok_num[i], char_word[i]), ".txt")
        }
        file.rename("final_results.txt", results_fn)
        
        # #svm test with poly kernel
        # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
        #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm",
        #          svm.kernel="polynomial", svm.cost=1,
        #          use.existing.freq.tables = TRUE
        #          , sampling=text.or.samp.var, sample.size=samp.size.var
        #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
        # )
        # results_fn <- paste0("results/", batch_tests[x], "/svm_polynomial_", paste0(tok_num[i], char_word[i]), ".txt")
        # file.rename("final_results.txt", results_fn)
        # 
        # #SVM with radial kernel
        # classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
        #          ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm",
        #          svm.kernel="radial", svm.cost=1,
        #          use.existing.freq.tables = TRUE
        #          , sampling=text.or.samp.var, sample.size=samp.size.var
        #          , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
        # )
        # results_fn <- paste0("results/", batch_tests[x], "/svm_radial_", paste0(tok_num[i], char_word[i]), ".txt")
        # file.rename("final_results.txt", results_fn)
    
    
        #naivebayes test
        classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i],
                 ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes",
                 use.existing.freq.tables = TRUE
                 , sampling=text.or.samp.var, sample.size=samp.size.var
                 , culling.min = 0, culling.max = culling.max.var, culling.incr = 10
        )
        if(mod_text == 1) {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/ne_nb_", paste0(tok_num[i], char_word[i]), ".txt")
        } else {
          results_fn <- paste0("results/", "test_", num_of_test, "/", batch_tests[x], "/e_nb_", paste0(tok_num[i], char_word[i]), ".txt")
        }

        file.rename("final_results.txt", results_fn)
      }
    }
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
tests <- c("delta", "knn", "nb", "nsc", "svm_linear")
accuracies <- c(100,95,90,85,80,79)

total_model_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), 
                                  hume_90=rep(0, 5), ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), 
                                  hume_80=rep(0, 5), ferg_80=rep(0, 5),  hume_79=rep(0, 5), ferg_79=rep(0, 5), 
                                  row.names = tests)

for (i in 1:length(batch_tests)) {
  #get results  
  result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), 
                        hume_90=rep(0, 5), ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), 
                        hume_80=rep(0, 5), ferg_80=rep(0, 5),  hume_79=rep(0, 5), ferg_79=rep(0, 5), 
                        row.names = tests)
  to_test <- c()
  for(z in 1:length(tests)) { 
    to_test <- c(to_test, grep(tests[z], result_files))
  }
  result_files <- result_files[to_test]
  
  
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

    
######################
#create and load test by features

char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
tests <- c("delta", "knn", "nb", "nsc", "svm_linear")
accuracies <- c(100,95,90,85,80,79)
for (i in 1:length(char_word)) {
  temp_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), 
                             hume_90=rep(0, 5), ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), 
                             hume_80=rep(0, 5), ferg_80=rep(0, 5),  hume_79=rep(0, 5), ferg_79=rep(0, 5), 
                             row.names = tests) 
  assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
}

features <- c("w1", "w2", "w3", "c2", "c3", "c4", "c5", "c6")
total_feature_results <- data.frame(hume_100=rep(0, 8), ferg_100=rep(0, 8), hume_95=rep(0, 8), ferg_95=rep(0, 8), 
                                    hume_90=rep(0, 8), ferg_90=rep(0, 8), hume_85=rep(0, 8), ferg_85=rep(0, 8), 
                                    hume_80=rep(0, 8), ferg_80=rep(0, 8),  hume_79=rep(0, 8), ferg_79=rep(0, 8), 
                                    row.names = features)

for (i in 1:length(batch_tests)) {
  #get results  
  result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  for (x in 1:length(features)) {
    temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
    cat("\r", x)
    file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
    for (y in 1:length(file_locs)) {
      test <- read.delim(result_files[file_locs[y]], header = FALSE, stringsAsFactors = FALSE)
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


######################
#create and load test by MFW results

char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
tests <- c("delta", "knn", "nb", "nsc", "svm_linear")
mffeatures <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
accuracies <- c(100,95,90,85,80,79)
 # for (i in 1:length(char_word)) {
 #   temp_results <- data.frame(hume_100=rep(0, 10), ferg_100=rep(0, 10), hume_95=rep(0, 10), ferg_95=rep(0, 10), 
 #                              hume_90=rep(0, 10), ferg_90=rep(0, 10), hume_85=rep(0, 10), ferg_85=rep(0, 10), 
 #                              hume_80=rep(0, 10), ferg_80=rep(0, 10),  hume_79=rep(0, 10), ferg_79=rep(0, 10), 
 #                              row.names = mffeatures) 
 #   assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
 # }


batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")


total_mfw_results <- data.frame(hume_100=rep(0, 10), ferg_100=rep(0, 10), hume_95=rep(0, 10), ferg_95=rep(0, 10), 
                                    hume_90=rep(0, 10), ferg_90=rep(0, 10), hume_85=rep(0, 10), ferg_85=rep(0, 10), 
                                    hume_80=rep(0, 10), ferg_80=rep(0, 10),  hume_79=rep(0, 10), ferg_79=rep(0, 10), 
                                    row.names = mffeatures)

for (i in 1:length(batch_tests)) {
  #get results  
  result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  for (x in 1:length(result_files)) {
    test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
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
  
#   
#   for (x in 1:length(total_mfw_results)) {
#     temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
#     cat("\r", x)
#     file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
#     for (y in 1:length(file_locs)) {
#       test <- read.delim(result_files[file_locs[y]], header = FALSE, stringsAsFactors = FALSE)
#       for(z in 1:length(tests)) { 
#         if(grepl(tests[z], result_files[file_locs[y]])) {
#           current_test <- tests[z]
#         }
#       }
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
#         
#         temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] <-
#           temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] + length(grep("Hume", temp_results$V3))
#         
#         temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] <-
#           temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] + length(grep("Ferg", temp_results$V3))
#         test_cutoffs <- test_cutoffs[-c(1:2)]
#         
#         assign(paste0(char_word[x], tok_num[x], "_results"), temp_total_results)
#         
#         
#         total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("hume_", accuracy_group), names(total_feature_results))] <-
#           total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("hume_", accuracy_group), names(total_feature_results))] + length(grep("Hume", temp_results$V3))
#         
#         total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("ferg_", accuracy_group), names(total_feature_results))] <-
#           total_feature_results[grep(features[x], row.names(total_feature_results)),grep(paste0("ferg_", accuracy_group), names(total_feature_results))] + length(grep("Ferg", temp_results$V3))
#         next
#       }
#     }
#   }
# }




######################
#create and load test by datasets


batch_tests <- c("small_samp_no_cull", "small_samp_with_cull", "large_samp_no_cull", "large_samp_with_cull",
                 "texts_no_cull", "texts_with_cull")
char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
tests <- c("delta", "knn", "nb", "nsc", "svm_linear", "svm_radial", "svm_polynomial")
accuracies <- c(100,95,90,85,80,79)

total_dataset_results <- data.frame(hume_100=rep(0, 6), ferg_100=rep(0, 6), hume_95=rep(0, 6), ferg_95=rep(0, 6), 
                                  hume_90=rep(0, 6), ferg_90=rep(0, 6), hume_85=rep(0, 6), ferg_85=rep(0, 6), 
                                  hume_80=rep(0, 6), ferg_80=rep(0, 6),  hume_79=rep(0, 6), ferg_79=rep(0, 6), 
                                  row.names = batch_tests)

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


###############################################
# Create table of results for chunks of Sister Peg

batch_tests <- c("small_samp_no_cull", "small_samp_with_cull")
char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
tests <- c("delta", "knn", "nb", "nsc", "svm_linear")
sp_chunks <- c(1:12)
accuracies <- c(100,95,90,85,80,79)


total_sp_sample_results <- data.frame(hume_100=rep(0, 12), ferg_100=rep(0, 12), hume_95=rep(0, 12), ferg_95=rep(0, 12), 
                                    hume_90=rep(0, 12), ferg_90=rep(0, 12), hume_85=rep(0, 12), ferg_85=rep(0, 12), 
                                    hume_80=rep(0, 12), ferg_80=rep(0, 12),  hume_79=rep(0, 12), ferg_79=rep(0, 12), 
                                    row.names = sp_chunks) 

for (i in 1:length(batch_tests)) {
  #get results  
  result_files <- list.files(paste0("results/", batch_tests[i], "/"), full.names = TRUE)
  for (x in 1:length(features)) {
    #temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
    cat("\r", x)
    file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
    for (y in 1:length(file_locs)) {
      test <- read.delim(result_files[file_locs[y]], header = FALSE, stringsAsFactors = FALSE)
      # for(z in 1:length(tests)) { 
      #   if(grepl(tests[z], result_files[file_locs[y]])) {
      #     current_test <- tests[z]
      #   }
      # }
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
        for (z in 1:length(sp_chunks)) {
          cat("\r", z)
          total_sp_sample_results[z,grep(paste0("hume_", accuracy_group), names(total_sp_sample_results))] <-
            total_sp_sample_results[z,grep(paste0("hume_", accuracy_group), names(total_sp_sample_results))] + length(grep("Hume", temp_results$V3[z]))
          
          total_sp_sample_results[z,grep(paste0("ferg_", accuracy_group), names(total_sp_sample_results))] <-
            total_sp_sample_results[z,grep(paste0("ferg_", accuracy_group), names(total_sp_sample_results))] + length(grep("Ferg", temp_results$V3[z]))
          test_cutoffs <- test_cutoffs[-c(1:2)]
          next
        }
      }
    }
  }
}




#plot accuracy levels of tests
plot(total_sp_sample_results$hume_100, type = "l", col = "red")
lines(total_sp_sample_results$ferg_100, type = "l", col = "blue")

#plot feature results
plot((unlist(total_feature_results[1,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "w1")
lines((unlist(total_feature_results[1,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_feature_results[2,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "w2")
lines((unlist(total_feature_results[2,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_feature_results[3,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "w3")
lines((unlist(total_feature_results[3,c(1,3,5,7,9,11)])), type = "l", col = "red")

#all token tests
plot((unlist(total_feature_results[1,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "Tokens")
lines((unlist(total_feature_results[1,c(1,3,5,7,9,11)])), type = "l", col = "red")
lines((unlist(total_feature_results[2,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "w2", lty=2)
lines((unlist(total_feature_results[2,c(1,3,5,7,9,11)])), type = "l", col = "red", lty=2)
lines((unlist(total_feature_results[3,c(1,3,5,7,9,11)])), type = "l", col = "red", main = "w3", lty=3)
lines((unlist(total_feature_results[3,c(2,4,6,8,10,12)])), type = "l", col = "blue", lty=3)

#character
plot((unlist(total_feature_results[4,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "c2")
lines((unlist(total_feature_results[4,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_feature_results[5,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "c3")
lines((unlist(total_feature_results[5,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_feature_results[6,c(1,3,5,7,9,11)])), type = "l", col = "red", main = "c4")
lines((unlist(total_feature_results[6,c(2,4,6,8,10,12)])), type = "l", col = "blue")

plot((unlist(total_feature_results[7,c(1,3,5,7,9,11)])), type = "l", col = "red", main = "c5")
lines((unlist(total_feature_results[7,c(2,4,6,8,10,12)])), type = "l", col = "blue")

plot((unlist(total_feature_results[8,c(1,3,5,7,9,11)])), type = "l", col = "red", main = "c6")
lines((unlist(total_feature_results[8,c(2,4,6,8,10,12)])), type = "l", col = "blue")


#character


plot((unlist(total_feature_results[7,c(1,3,5,7,9,11)])), type = "l", col = "red", lty=4)
lines((unlist(total_feature_results[7,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "c5", lty=4)

lines((unlist(total_feature_results[4,c(1,3,5,7,9,11)])), type = "l", col = "red")
lines((unlist(total_feature_results[4,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "c2")

lines((unlist(total_feature_results[5,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "c3", lty=2)
lines((unlist(total_feature_results[5,c(1,3,5,7,9,11)])), type = "l", col = "red", lty=2)

lines((unlist(total_feature_results[6,c(1,3,5,7,9,11)])), type = "l", col = "red", lty=3)
lines((unlist(total_feature_results[6,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "c4", lty=3)

lines((unlist(total_feature_results[8,c(1,3,5,7,9,11)])), type = "l", col = "red", lty=5)
lines((unlist(total_feature_results[8,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = "c6", lty=5)

#data results
plot((unlist(total_dataset_results[1,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_dataset_results)[1])
lines((unlist(total_dataset_results[1,c(2,4,6,8,10,12)])), type = "l", col = "blue")

plot((unlist(total_dataset_results[2,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_dataset_results)[2])
lines((unlist(total_dataset_results[2,c(2,4,6,8,10,12)])), type = "l", col = "blue")

plot((unlist(total_dataset_results[3,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = row.names(total_dataset_results)[3])
lines((unlist(total_dataset_results[3,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_dataset_results[4,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = row.names(total_dataset_results)[4])
lines((unlist(total_dataset_results[4,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_dataset_results[5,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = row.names(total_dataset_results)[5])
lines((unlist(total_dataset_results[5,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_dataset_results[6,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = row.names(total_dataset_results)[6])
lines((unlist(total_dataset_results[6,c(1,3,5,7,9,11)])), type = "l", col = "red")


#plot by models

plot((unlist(total_model_results[1,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = row.names(total_model_results)[1])
lines((unlist(total_model_results[1,c(1,3,5,7,9,11)])), type = "l", col = "red")

plot((unlist(total_model_results[2,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_model_results)[2])
lines((unlist(total_model_results[2,c(2,4,6,8,10,12)])), type = "l", col = "blue")

plot((unlist(total_model_results[3,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_model_results)[3])
lines((unlist(total_model_results[3,c(2,4,6,8,10,12)])), type = "l", col = "blue")

plot((unlist(total_model_results[4,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_model_results)[4])
lines((unlist(total_model_results[4,c(2,4,6,8,10,12)])), type = "l", col = "blue")

plot((unlist(total_model_results[5,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = row.names(total_model_results)[5])
lines((unlist(total_model_results[5,c(1,3,5,7,9,11)])), type = "l", col = "red")


#plot by models
plot((unlist(total_model_results[5,c(2,4,6,8,10,12)])), type = "l", col = "blue", main = row.names(total_model_results)[5])
lines((unlist(total_model_results[5,c(1,3,5,7,9,11)])), type = "l", col = "red")

lines((unlist(total_model_results[1,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_model_results)[1], lty=2)
lines((unlist(total_model_results[1,c(2,4,6,8,10,12)])), type = "l", col = "blue", lty=2)

lines((unlist(total_model_results[2,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_model_results)[2], lty=3)
lines((unlist(total_model_results[2,c(2,4,6,8,10,12)])), type = "l", col = "blue", lty=3)

lines((unlist(total_model_results[3,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_model_results)[3], lty=2)
lines((unlist(total_model_results[3,c(2,4,6,8,10,12)])), type = "l", col = "blue", lty=2)

lines((unlist(total_model_results[4,c(1,3,5,7,9,11)])), type = "l", col = "red", main = row.names(total_model_results)[4], lty=2)
lines((unlist(total_model_results[4,c(2,4,6,8,10,12)])), type = "l", col = "blue", lty=2)

#Total by model

plot(c(sum(total_model_results[5,1], total_model_results[5,2]), sum(total_model_results[5,3], total_model_results[5,4]), 
       sum(total_model_results[5,5], total_model_results[5,6]), sum(total_model_results[5,7], total_model_results[5,8]), 
       sum(total_model_results[5,9], total_model_results[5,10]), sum(total_model_results[5,11], total_model_results[5,12])),
     type = "l", ylab = NA, col="blue")

lines(c(sum(total_model_results[1,1], total_model_results[1,2]), sum(total_model_results[1,3], total_model_results[1,4]), 
       sum(total_model_results[1,5], total_model_results[1,6]), sum(total_model_results[1,7], total_model_results[1,8]), 
       sum(total_model_results[1,9], total_model_results[1,10]), sum(total_model_results[1,11], total_model_results[1,12])),
      type = "l", ylab = NA, col="red")

lines(c(sum(total_model_results[2,1], total_model_results[2,2]), sum(total_model_results[2,3], total_model_results[2,4]), 
       sum(total_model_results[2,5], total_model_results[2,6]), sum(total_model_results[2,7], total_model_results[2,8]), 
       sum(total_model_results[2,9], total_model_results[2,10]), sum(total_model_results[2,11], total_model_results[2,12])),
     type = "l", ylab = NA, col="pink")

lines(c(sum(total_model_results[3,1], total_model_results[3,2]), sum(total_model_results[3,3], total_model_results[3,4]), 
        sum(total_model_results[3,5], total_model_results[3,6]), sum(total_model_results[3,7], total_model_results[3,8]), 
        sum(total_model_results[3,9], total_model_results[3,10]), sum(total_model_results[3,11], total_model_results[3,12])),
      type = "l", ylab = NA, lty=3, col="green")

lines(c(sum(total_model_results[4,1], total_model_results[4,2]), sum(total_model_results[4,3], total_model_results[4,4]), 
        sum(total_model_results[4,5], total_model_results[4,6]), sum(total_model_results[4,7], total_model_results[4,8]), 
        sum(total_model_results[4,9], total_model_results[4,10]), sum(total_model_results[4,11], total_model_results[4,12])),
      type = "l", ylab = NA, col="purple")

#Total by feature

plot(c(sum(total_feature_results[7,1], total_feature_results[7,2]), sum(total_feature_results[7,3], total_feature_results[7,4]), 
       sum(total_feature_results[7,5], total_feature_results[7,6]), sum(total_feature_results[7,7], total_feature_results[7,8]), 
       sum(total_feature_results[7,9], total_feature_results[7,10]), sum(total_feature_results[7,11], total_feature_results[7,12])),
     type = "l", ylab = NA, col="purple")

lines(c(sum(total_feature_results[5,1], total_feature_results[5,2]), sum(total_feature_results[5,3], total_feature_results[5,4]), 
       sum(total_feature_results[5,5], total_feature_results[5,6]), sum(total_feature_results[5,7], total_feature_results[5,8]), 
       sum(total_feature_results[5,9], total_feature_results[5,10]), sum(total_feature_results[5,11], total_feature_results[5,12])),
     type = "l", ylab = NA, col="blue")

lines(c(sum(total_feature_results[1,1], total_feature_results[1,2]), sum(total_feature_results[1,3], total_feature_results[1,4]), 
        sum(total_feature_results[1,5], total_feature_results[1,6]), sum(total_feature_results[1,7], total_feature_results[1,8]), 
        sum(total_feature_results[1,9], total_feature_results[1,10]), sum(total_feature_results[1,11], total_feature_results[1,12])), 
      type = "l", ylab = NA, col="green")

lines(c(sum(total_feature_results[2,1], total_feature_results[2,2]), sum(total_feature_results[2,3], total_feature_results[2,4]), 
        sum(total_feature_results[2,5], total_feature_results[2,6]), sum(total_feature_results[2,7], total_feature_results[2,8]), 
        sum(total_feature_results[2,9], total_feature_results[2,10]), sum(total_feature_results[2,11], total_feature_results[2,12])),
      type = "l", ylab = NA, col="orange")

lines(c(sum(total_feature_results[3,1], total_feature_results[3,2]), sum(total_feature_results[3,3], total_feature_results[3,4]), 
        sum(total_feature_results[3,5], total_feature_results[3,6]), sum(total_feature_results[3,7], total_feature_results[3,8]), 
        sum(total_feature_results[3,9], total_feature_results[3,10]), sum(total_feature_results[3,11], total_feature_results[3,12])),
      type = "l", ylab = NA, col="red")

lines(c(sum(total_feature_results[4,1], total_feature_results[4,2]), sum(total_feature_results[4,3], total_feature_results[4,4]), 
        sum(total_feature_results[4,5], total_feature_results[4,6]), sum(total_feature_results[4,7], total_feature_results[4,8]), 
        sum(total_feature_results[4,9], total_feature_results[4,10]), sum(total_feature_results[4,11], total_feature_results[4,12])),
      type = "l", ylab = NA, col="brown")


lines(c(sum(total_feature_results[6,1], total_feature_results[6,2]), sum(total_feature_results[6,3], total_feature_results[6,4]), 
        sum(total_feature_results[6,5], total_feature_results[6,6]), sum(total_feature_results[6,7], total_feature_results[6,8]), 
        sum(total_feature_results[6,9], total_feature_results[6,10]), sum(total_feature_results[6,11], total_feature_results[6,12])),
      type = "l", ylab = NA, col="dark green")

lines(c(sum(total_feature_results[8,1], total_feature_results[8,2]), sum(total_feature_results[8,3], total_feature_results[8,4]), 
        sum(total_feature_results[8,5], total_feature_results[8,6]), sum(total_feature_results[8,7], total_feature_results[8,8]), 
        sum(total_feature_results[8,9], total_feature_results[8,10]), sum(total_feature_results[8,11], total_feature_results[8,12])),
      type = "l", ylab = NA, col="pink")



#Total by MF features
i_order <- c(5, 1, 2, 3, 4, 6, 7, 8, 9, 10)
ran_col <- c("red", "pink", "blue", "black", "orange", "purple", "green", "dark green", "light blue", "orange")
for (i in 1:10) {
  if (i == 1) {
    plot(c(sum(total_mfw_results[i_order[i],1], total_mfw_results[i_order[i],2]), 
           sum(total_mfw_results[i_order[i],3], total_mfw_results[i_order[i],4]), 
           sum(total_mfw_results[i_order[i],5], total_mfw_results[i_order[i],6]), 
           sum(total_mfw_results[i_order[i],7], total_mfw_results[i_order[i],8]), 
           sum(total_mfw_results[i_order[i],9], total_mfw_results[i_order[i],10]), 
           sum(total_mfw_results[i_order[i],11], total_mfw_results[i_order[i],12])),
         type = "l", ylab = NA, col=ran_col[i]) }
  else {
    lines(c(sum(total_mfw_results[i_order[i],1], total_mfw_results[i_order[i],2]), 
            sum(total_mfw_results[i_order[i],3], total_mfw_results[i_order[i],4]), 
            sum(total_mfw_results[i_order[i],5], total_mfw_results[i_order[i],6]), 
            sum(total_mfw_results[i_order[i],7], total_mfw_results[i_order[i],8]), 
            sum(total_mfw_results[i_order[i],9], total_mfw_results[i_order[i],10]), 
            sum(total_mfw_results[i_order[i],11], total_mfw_results[i_order[i],12])),
          type = "l", ylab = NA, col=ran_col[i]) }
}




########################## NORMALIZED RESULTS

#Total by dataset normalized

plot(c(sum(total_dataset_results[2,1], total_dataset_results[2,2]), sum(total_dataset_results[2,3], total_dataset_results[2,4]), 
       sum(total_dataset_results[2,5], total_dataset_results[2,6]), sum(total_dataset_results[2,7], total_dataset_results[2,8]), 
       sum(total_dataset_results[2,9], total_dataset_results[2,10]), sum(total_dataset_results[2,11], total_dataset_results[2,12])),
     type = "l", ylab = NA, col="pink")

lines(c(sum(total_dataset_results[5,1], total_dataset_results[5,2]), sum(total_dataset_results[5,3], total_dataset_results[5,4]), 
        sum(total_dataset_results[5,5], total_dataset_results[5,6]), sum(total_dataset_results[5,7], total_dataset_results[5,8]), 
        sum(total_dataset_results[5,9], total_dataset_results[5,10]), sum(total_dataset_results[5,11], total_dataset_results[5,12])),
      type = "l", ylab = NA, col="blue")

lines(c(sum(total_dataset_results[1,1], total_dataset_results[1,2]), sum(total_dataset_results[1,3], total_dataset_results[1,4]), 
        sum(total_dataset_results[1,5], total_dataset_results[1,6]), sum(total_dataset_results[1,7], total_dataset_results[1,8]), 
        sum(total_dataset_results[1,9], total_dataset_results[1,10]), sum(total_dataset_results[1,11], total_dataset_results[1,12])),
      type = "l", ylab = NA, col="red")


lines(c(sum(total_dataset_results[3,1], total_dataset_results[3,2]), sum(total_dataset_results[3,3], total_dataset_results[3,4]), 
        sum(total_dataset_results[3,5], total_dataset_results[3,6]), sum(total_dataset_results[3,7], total_dataset_results[3,8]), 
        sum(total_dataset_results[3,9], total_dataset_results[3,10]), sum(total_dataset_results[3,11], total_dataset_results[3,12])),
      type = "l", ylab = NA, lty=3, col="green")

lines(c(sum(total_dataset_results[4,1], total_dataset_results[4,2]), sum(total_dataset_results[4,3], total_dataset_results[4,4]), 
        sum(total_dataset_results[4,5], total_dataset_results[4,6]), sum(total_dataset_results[4,7], total_dataset_results[4,8]), 
        sum(total_dataset_results[4,9], total_dataset_results[4,10]), sum(total_dataset_results[4,11], total_dataset_results[4,12])),
      type = "l", ylab = NA, col="purple")


lines(c(sum(total_dataset_results[6,1], total_dataset_results[6,2]), sum(total_dataset_results[6,3], total_dataset_results[6,4]), 
        sum(total_dataset_results[6,5], total_dataset_results[6,6]), sum(total_dataset_results[6,7], total_dataset_results[6,8]), 
        sum(total_dataset_results[6,9], total_dataset_results[6,10]), sum(total_dataset_results[6,11], total_dataset_results[6,12])),
      type = "l", ylab = NA, col="purple")




plot(total_model_results[1,], type = "l", col = "red")
lines(total_model_results$ferg_100, type = "l", col = "blue")

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




TAKE EVERY 100% RESULTS AND COMPARE























# 
# 
# 
# 
# ############################# BELOW IS OLD
# 
# 
# knn_test <- TRUE
# current_knn_test <- 1
# best_k <- 0
# highest_score <- 0
# ks <- c()
# extra_runs <- 0
# 
# 
# for(i in 1:10) {
#   knn_results <- classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#                           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
#                           k.value=current_knn_test,
#                           use.existing.freq.tables = TRUE
#                           , sampling=text.or.samp.var, sample.size=samp.size.var
#                           , culling.min = 0, culling.max = culling.max.var, culling.incr = 10)
#   if (current_knn_test == 1) { 
#     success_rate <- knn_results$overall.success.rate 
#     current_knn_test <- current_knn_test + 1
#     ks <- c(success_rate)
#     next
#   } else { 
#     success_rate <- knn_results$overall.success.rate 
#     if (success_rate > highest_score) { 
#       highest_score <- success_rate 
#       best_k <- current_knn_test
#     } else if (success_rate < highest_score) {
#       extra_runs <- extra_runs + 1
#     }
#     ks <- c(ks, success_rate)
#     current_knn_test <- current_knn_test + 1
#   }
#   if (extra_runs == 3) { knn_test <- FALSE }
# }
# 
# 
# 
# 
# 
# 
# ######################################################################################################
# #RUN TESTS - SAMPLES NO CULLING
# for (i in 1:length(tok_num)) {
#   cat("\nRunning test", i, " out of ", length(tok_num))
#   #Delta test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
#            , sampling="normal.sampling", sample.size=1988
#            )
#   results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# 
#   #Knn test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
#            k.value=10,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            )
#   
#   results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# 
#   #NSC test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            )
#   results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #svm test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
#            svm.kernel="linear", svm.cost=1,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            )
#   results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #naivebayes test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            )
#   results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# }
# 
# ######################################################################################################
# #RUN TESTS - SAMPLES WITH CULLING
# for (i in 1:length(tok_num)) {
#   cat("\nRunning test", i, " out of ", length(tok_num))
#   #Delta test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
#            , sampling="normal.sampling", sample.size=1988
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #Knn test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
#            k.value=10,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   
#   results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #NSC test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #svm test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
#            svm.kernel="linear", svm.cost=1,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #naivebayes test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=1988
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# }
# 
# 
# 
# 
# ######################################################################################################
# #RUN TESTS - LARGE SAMPLES NO CULLING
# for (i in 1:length(tok_num)) {
#   cat("\nRunning test", i, " out of ", length(tok_num))
#   #Delta test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
#            , sampling="normal.sampling", sample.size=7955
#   )
#   results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #Knn test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
#            k.value=10,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#   )
#   
#   results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #NSC test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#   )
#   results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #svm test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
#            svm.kernel="linear", svm.cost=1,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#   )
#   results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #naivebayes test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#   )
#   results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# }
# 
# ######################################################################################################
# #RUN TESTS - LARGE SAMPLES WITH CULLING
# for (i in 1:length(tok_num)) {
#   cat("\nRunning test", i, " out of ", length(tok_num))
#   #Delta test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
#            , sampling="normal.sampling", sample.size=7955
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #Knn test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
#            k.value=10,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   
#   results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #NSC test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #svm test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
#            svm.kernel="linear", svm.cost=1,
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #naivebayes test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
#            use.existing.freq.tables = TRUE
#            , sampling="normal.sampling", sample.size=7955
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# }
# 
# 
# 
# 
# 
# ######################################################################################################
# #RUN TESTS - NO SAMPLES WITH CULLING
# for (i in 1:length(tok_num)) {
#   cat("\nRunning test", i, " out of ", length(tok_num))
#   #Delta test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #Knn test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
#            k.value=1,
#            use.existing.freq.tables = TRUE
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   
#   results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #NSC test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
#            use.existing.freq.tables = TRUE
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #svm test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
#            svm.kernel="linear", svm.cost=1,
#            use.existing.freq.tables = TRUE
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #naivebayes test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
#            use.existing.freq.tables = TRUE
#            , culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# }
# 
# ######################################################################################################
# #RUN TESTS - NO SAMPLES NO CULLING
# for (i in 1:length(tok_num)) {
#   cat("\nRunning test", i, " out of ", length(tok_num))
#   #Delta test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta"
#            #, culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #Knn test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
#            k.value=1,
#            use.existing.freq.tables = TRUE
#            #, culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   
#   results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   
#   #NSC test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
#            use.existing.freq.tables = TRUE
#            #, culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #svm test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
#            svm.kernel="linear", svm.cost=1,
#            use.existing.freq.tables = TRUE
#            #, culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
#   #naivebayes test
#   classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
#            ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
#            use.existing.freq.tables = TRUE
#            #, culling.min = 0, culling.max = 40, culling.incr = 10
#   )
#   results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
#   file.rename("final_results.txt", results_fn)
# }
# 
# 
# 
# FIND THE SECTIONS WHICH ARE MOST COMMONLY ATTRIBUTED TO HUME/FERG
# 
# Plot H v F 
# 
# #importing data
# 
# tests <- c("delta", "knn", "nb", "nsc", "svm")
# accuracies <- c(100,95,90,85,80,79)
# results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
#                       ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
#                       hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)
# 
# result_files <- list.files("results/", full.names = TRUE)
# 
# for (x in 1:length(result_files)) {
#   cat("\r", x)
#   test <- read.delim(result_files[x], header = FALSE, stringsAsFactors = FALSE)
#   for(z in 1:length(tests)) { 
#     if(grepl(tests[z], result_files[x])) {
#       current_test <- tests[z]
#     }
#   }
#   #grepl(tests, result_files[x])
#   testing <- TRUE
#   test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
#   test_cutoffs <- sort(test_cutoffs)
#   test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
#   while(testing == TRUE) {
#     if(length(test_cutoffs) == 0) { 
#       testing <- FALSE 
#       next
#     }
#     temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
#     #remove non sister peg tests
#     if (length(grep("merged", temp_results$V1)) != 0) {
#       temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
#       temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
#     }
#     current_accuracy <- test$V2[test_cutoffs[2]+1]
#     current_accuracy <- gsub("[(]", "", current_accuracy)
#     current_accuracy <- gsub("[)]", "", current_accuracy)
#     current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
#     
#     for (z in 1:length(accuracies)) {
#       if(current_accuracy == 100) { 
#         accuracy_group <- current_accuracy
#         break
#       } else if (current_accuracy < 80) {
#         accuracy_group <- 79
#         break
#       } else {
#         if (current_accuracy < accuracies[z-1] && current_accuracy >= accuracies[z]) {
#           accuracy_group <- accuracies[z]
#           next
#         }
#       }
#     }
#     
#     results[grep(current_test, row.names(results)),grep(paste0("hume_", accuracy_group), names(results))] <-
#       results[grep(current_test, row.names(results)),grep(paste0("hume_", accuracy_group), names(results))] + length(grep("Hume", temp_results$V3))
#     
#     results[grep(current_test, row.names(results)),grep(paste0("ferg_", accuracy_group), names(results))] <-
#       results[grep(current_test, row.names(results)),grep(paste0("ferg_", accuracy_group), names(results))] + length(grep("Ferg", temp_results$V3))
#     test_cutoffs <- test_cutoffs[-c(1:2)]
#     next
#   }
# }
# 
# 
# #load by feature test
# 
# char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
# tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")
# tests <- c("delta", "knn", "nb", "nsc", "svm")
# accuracies <- c(100,95,90,85,80,79)
# for (i in 1:length(char_word)) {
#   temp_results <- data.frame(hume_100=rep(0, 5), ferg_100=rep(0, 5), hume_95=rep(0, 5), ferg_95=rep(0, 5), hume_90=rep(0, 5), 
#                         ferg_90=rep(0, 5), hume_85=rep(0, 5), ferg_85=rep(0, 5), hume_80=rep(0, 5), ferg_80=rep(0, 5),  
#                         hume_79=rep(0, 5), ferg_79=rep(0, 5), row.names = tests)  
#   assign(paste0(char_word[i], tok_num[i], "_results"), temp_results)
# }
# 
# 
# result_files <- list.files("results/", full.names = TRUE)
# 
# for (x in 1:length(char_word)) {
#   temp_total_results <- get(paste0(char_word[x], tok_num[x], "_results"))
#   cat("\r", x)
#   file_locs <- grep(paste0(tok_num[x], char_word[x]), result_files)
#   for (i in 1:length(file_locs)) {
#     test <- read.delim(result_files[file_locs[i]], header = FALSE, stringsAsFactors = FALSE)
#     
#     for(z in 1:length(tests)) { 
#       if(grepl(tests[z], result_files[file_locs[i]])) {
#         current_test <- tests[z]
#       }
#     }
#     #grepl(tests, result_files[x])
#     testing <- TRUE
#     test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
#     test_cutoffs <- sort(test_cutoffs)
#     test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
#     while(testing == TRUE) {
#       if(length(test_cutoffs) == 0) { 
#         testing <- FALSE 
#         next
#       }
#       temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
#       #remove non sister peg tests
#       if (length(grep("Hume_", temp_results$V1)) != 0) {
#         temp_results <- temp_results[-c(grep("Hume_", temp_results$V1)),]
#       }
#       if (length(grep("Ferguson_", temp_results$V1)) != 0) {
#         temp_results <- temp_results[-c(grep("Ferguson_", temp_results$V1)),]
#       }
#       current_accuracy <- test$V2[test_cutoffs[2]+1]
#       current_accuracy <- gsub("[(]", "", current_accuracy)
#       current_accuracy <- gsub("[)]", "", current_accuracy)
#       current_accuracy <- as.numeric(gsub("%", "", current_accuracy))
#       
#       for (z in 1:length(accuracies)) {
#         if(current_accuracy == 100) { 
#           accuracy_group <- current_accuracy
#           break
#         } else if (current_accuracy < 80) {
#           accuracy_group <- 79
#           break
#         } else {
#           if (current_accuracy < accuracies[z-1] && current_accuracy >= accuracies[z]) {
#             accuracy_group <- accuracies[z]
#             next
#           }
#         }
#       }
#       
#       temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] <-
#         temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("hume_", accuracy_group), names(temp_total_results))] + length(grep("Hume", temp_results$V3))
#       
#       temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] <-
#         temp_total_results[grep(current_test, row.names(temp_total_results)),grep(paste0("ferg_", accuracy_group), names(temp_total_results))] + length(grep("Ferg", temp_results$V3))
#       test_cutoffs <- test_cutoffs[-c(1:2)]
#       
#       assign(paste0(char_word[x], tok_num[x], "_results"), temp_total_results)
#       next
#     }
#   }
# }
# 
# 
# 
# 
# 
# #plot accuracy levels of tests
# accuracy_df <- data.frame("100"=rep(0,5), "95"=rep(0,5), "90"=rep(0,5), "85"=rep(0,5), "80"=rep(0,5), "79"=rep(0,5), row.names = tests)
# 
# for (i in 1:length(char_word)) {
#   temp_total_results <- get(paste0(char_word[i], tok_num[i], "_results"))
#   for (x in 1:length(accuracies)) {
#     for (z in 1:length(tests)) {
#       col_pull_from <- grep(accuracies[x], names(temp_total_results))
#       row_pull_from <- grep(tests[z], row.names(temp_total_results))
#       total <- sum(temp_total_results[row_pull_from, col_pull_from])
#       col_push_to <- grep(accuracies[x], names(accuracy_df))
#       row_push_to <- grep(tests[z], row.names(accuracy_df))
#       accuracy_df[row_push_to, col_push_to] <- sum(accuracy_df[row_push_to, col_push_to], total)
#     }
#   }  
# }
# 
# #plot accuracy levels of features
# feature_types <- c()
# for (i in 1:length(char_word)) {
#   feature_types <- c(feature_types, paste0(char_word[i], tok_num[i]))
# }
# 
# feature_accuracy_df <- data.frame("100"=rep(0,8), "95"=rep(0,8), "90"=rep(0,8), "85"=rep(0,8), "80"=rep(0,8), "79"=rep(0,8), row.names = feature_types)
# 
# for (i in 1:length(char_word)) {
#   temp_total_results <- get(paste0(char_word[i], tok_num[i], "_results"))
#   row_to_push_to <- grep(paste0(char_word[i], tok_num[i]), row.names(feature_accuracy_df))
#   for (x in 1:length(accuracies)) {
#     col_pull_from <- grep(accuracies[x], names(temp_total_results))
#     total <- sum(temp_total_results[,col_pull_from])
#     col_push_to <- grep(accuracies[x], names(feature_accuracy_df))
#     feature_accuracy_df[row_to_push_to, col_push_to] <- sum(feature_accuracy_df[row_to_push_to, col_push_to], total)
#   }
# }  
# 
# 
# 
# 
# 
# 
# 
# 
# hume_results <- results[c(1,3,5,7,9,11)]
# ferg_results <- results[c(2,4,6,7,8,12)]
# 
# plot(hume_results[1,], ferg_results[1,])
# 
# #importing data as one
# 
# bad_results <- data.frame(name=c("hume", "ferg"), results=0)
# good_results <- data.frame(name=c("hume", "ferg"), results=0)
# result_files <- list.files("results/", full.names = TRUE)
# tests <- c("delta", "knn", "nb", "nsc", "svm")
# 
# for (x in 1:length(result_files)) {
#   test <- read.delim(result_files[x], header = FALSE)
#   #grepl(tests, result_files[x])
#   testing <- TRUE
#   test_cutoffs <- c(1, grep("MFW", test$V1)-1, grep("MFW", test$V1)+1)
#   test_cutoffs <- sort(test_cutoffs)
#   test_cutoffs <- test_cutoffs[-c((length(test_cutoffs)),(length(test_cutoffs))-1,(length(test_cutoffs))-2)]
#   while(testing == TRUE) {
#     if(length(test_cutoffs) == 0) { 
#       testing <- FALSE 
#       next
#     }
#     temp_results <- test[test_cutoffs[1]:test_cutoffs[2],]
#     #temp!
#     #temp_results <- temp_results(
#     if(any(grepl("Ferguson_history_eb", temp_results$V1))) { temp_results <- temp_results[-grepl("Ferguson_history_eb", temp_results$V1),] }
#     #temp!
#     if (any(grepl("merged", temp_results$V1))) {
#       bad_results$results[1] <- bad_results$results[1] + length(grep("Hume", temp_results$V3))
#       bad_results$results[2] <- bad_results$results[2] + length(grep("Ferguson", temp_results$V3))
#       test_cutoffs <- test_cutoffs[-c(1:2)]
#       next
#     } else {
#       good_results$results[1] <- good_results$results[1] + length(grep("Hume", temp_results$V3))
#       good_results$results[2] <- good_results$results[2] + length(grep("Ferguson", temp_results$V3))
#       test_cutoffs <- test_cutoffs[-c(1:2)]
#       next
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(plot_df) +
#   geom_bar(stat="identity", aes(x = authors, y= f1_50), fill = "red") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_55), fill = "grey70") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_60), fill = "grey60") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_65), fill = "grey50") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_70), fill = "grey40") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_75), fill = "grey30") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_80), fill = "grey20") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_85), fill = "grey10") +
#   geom_bar(stat="identity", aes(x = authors, y= f1_90), fill = "black") +
#   #theme(axis.title.x = element_blank())  +
#   labs(title = "Collocations in F1-Score Corpora") +
#   ylab("Total collocations as ratio") +
#   xlab("F1-Score Corpora") 
# +
#   coord_flip()
# 
# +
#   #theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   
#   #FIRST ATTEMPT< NOT GOOD
#   
#   
#   authors <- tail(names(sort(table(author_df$author))), 10)
# 
# #create author corpora
# for (i in 1:10) {
#   temp_df <- author_df[which(author_df$author == authors[i]),]
#   temp_name <- paste0("author_corp_", i)
#   assign(temp_name, temp_df)
# }
# 
# #craete author sub-corpora
# temp_names <- c()
# for (i in 1:10) { temp_names <- c(temp_names, paste0("author_corp_", i)) }
# f1s <- c("50", "60", "65", "70", "75", "80", "85", "90")
# 
# for (i in 1:10) {
#   cat("\r", i)
#   temp_total_corp <- get(temp_names[i])
#   for (x in 1:length(f1s)) {
#     if (x == length(f1s)) {
#       temp_doc <- temp_total_corp[which(temp_total_corp$f1 >= (as.numeric(f1s[x])/100)),]
#     } else if (x == 1) {
#       temp_doc <- temp_total_corp[which(temp_total_corp$f1 <= (as.numeric(f1s[x])/100)),]
#     } else {
#       temp_doc <- temp_total_corp[which(temp_total_corp$f1 >= (as.numeric(f1s[x])/100) & temp_total_corp$f1 < (as.numeric(f1s[x+1])/100)),]
#     }
#     temp_doc_name <- paste0("author_", i, "_f1_", f1s[x])
#     assign(temp_doc_name, temp_doc)
#   }
# }
# 
# #write txt files to be used by stylo
# for (i in 1:length(authors)) {
#   cat("\r", i)
#   for (x in 1:length(f1s)) {
#     temp_doc <- get(paste0("author_", i, "_f1_", f1s[x]))
#     temp_text_ocr <- paste(temp_doc$ocr, collapse = " ")
#     temp_text_tcp <- paste(temp_doc$tcp, collapse = " ")
#     author_name <- gsub(", .*$", "", authors[i])
#     temp_file_tcp <- paste0("data/aa/", i, "/", author_name, "_tcp_", f1s[x], ".txt")
#     temp_file_ocr <- paste0("data/aa/", i, "/", author_name, "_ocr_", f1s[x], ".txt")
#     fileConn<-file(temp_file_tcp)
#     writeLines(temp_text_tcp, fileConn)
#     close(fileConn)
#     fileConn<-file(temp_file_ocr)
#     writeLines(temp_text_ocr, fileConn)
#     close(fileConn)
#   }
# }
# 
# authors_pages_df <- data.frame(author=character(80), author_id=character(80), f1_group=character(80), total_pages=numeric(80),
#                                stringsAsFactors = FALSE)
# for (i in 1:length(authors)) {
#   for (x in 1:8) {
#     if (i == 1) {
#       authors_pages_df$author[x] <- authors[i]
#       authors_pages_df$author_id[x] <- i
#       authors_pages_df$f1_group[x] <- f1s[x]
#       temp_doc <- get(paste0("author_", i, "_f1_", f1s[x]))
#       authors_pages_df$total_pages[x] <- nrow(temp_doc)
#     } else {
#       authors_pages_df$author[(i-1)*8+x] <- authors[i]
#       authors_pages_df$author_id[(i-1)*8+x] <- i
#       authors_pages_df$f1_group[(i-1)*8+x] <- f1s[x]
#       temp_doc <- get(paste0("author_", i, "_f1_", f1s[x]))
#       authors_pages_df$total_pages[(i-1)*8+x] <- nrow(temp_doc)
#     }
#   }
# }
# write.csv(authors_pages_df, file="data/aa/authors_pages.csv")
# plot(authors_pages_df$f1_group, authors_pages_df$total_pages)
# 
# plot_authors <- authors_pages_df
# plot_authors$author_id <- as.factor(plot_authors$author)
# 
# ggplot(authors_pages_df, aes(f1_group, total_pages)) +
#   geom_point(aes(color=author), size=5)
# 
# 
# Randomly select pages to make up number of words FOR BOTH TCP AND OCR. 
# Remove pages from TCP/OCR files
# Save new text files for tcp/ocr
# move all files into correct directors
# 
# 2) Run Stylo variations
# 
# 
# 3) save results somewhere
