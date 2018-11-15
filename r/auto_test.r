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

#tests <- c("delta", "nsc", "svm", "naivebayes")
char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
tok_num <- c("1", "2", "3", "2", "3", "4", "5", "6")

TRY CULLING


#RUN TESTS
for (i in 1:length(tok_num)) {
  cat("\nRunning test", i, " out of ", length(tok_num))
  #Delta test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="delta",
           sampling="normal.sampling", sample.size=5000)
  results_fn <- paste0("results/delta_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)

  #Knn test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="knn", 
           k.value=10,
           use.existing.freq.tables = TRUE,
           sampling="normal.sampling", sample.size=5000)
  
  results_fn <- paste0("results/knn_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)

  #NSC test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE,
           sampling="normal.sampling", sample.size=5000)
  results_fn <- paste0("results/nsc_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #svm test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="svm", 
           svm.kernel="linear", svm.cost=1,
           use.existing.freq.tables = TRUE,
           sampling="normal.sampling", sample.size=1500)
  results_fn <- paste0("results/svm_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
  #naivebayes test
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[i], 
           ngram.size=as.numeric(tok_num[i]), encoding = "UTF-8", classification.method="naivebayes", 
           use.existing.freq.tables = TRUE
           )
  results_fn <- paste0("results/nb_", paste0(tok_num[i], char_word[i]), ".txt")
  file.rename("final_results.txt", results_fn)
}


#importing data

bad_results <- data.frame(name=c("hume", "ferg"), results=0)
good_results <- data.frame(name=c("hume", "ferg"), results=0)
result_files <- list.files("results/", full.names = TRUE)
tests <- c("delta", "knn", "nb", "nsc", "svm")

for (x in 1:length(result_files)) {
  test <- read.delim(result_files[x], header = FALSE)
  grepl(tests, result_files[x])
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
