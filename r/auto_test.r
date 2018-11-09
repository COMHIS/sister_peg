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

options(scipen=999)  # turn off scientific notation like 1e+06
options(stringsAsFactors = FALSE)

tests <- c("delta", "nsc", "svm", "naivebayes")
char_word <- c("w", "w", "w", "c", "c", "c", "c", "c")
word_num <- c("1", "2", "3", "2", "3", "4", "5", "6")


#RUN TESTS
for (test_list in 1:length(word_num)) {
  cat("\nRunning test", test_list, " out of ", length(word_num))
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[test_list], 
           ngram.size=as.numeric(word_num[test_list]), encoding = "UTF-8", classification.method="delta")
  results_fn <- paste0("data/aa/", author_name, f1s[x], "_delta_", page_counts[z], "_", 
                       paste0(word_num[test_list], char_word[test_list]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[test_list], 
           ngram.size=as.numeric(word_num[test_list]), encoding = "UTF-8", classification.method="knn", 
           use.existing.freq.tables = TRUE)
  
  results_fn <- paste0("data/aa/", author_name, f1s[x], "_knn_", page_counts[z], "_", 
                       paste0(word_num[test_list], char_word[test_list]), ".txt")
  file.rename("final_results.txt", results_fn)
  
  classify(gui = FALSE, mfw.min = 100, mfw.max = 1000, analyzed.features=char_word[test_list], 
           ngram.size=as.numeric(word_num[test_list]), encoding = "UTF-8", classification.method="nsc", 
           use.existing.freq.tables = TRUE)
  results_fn <- paste0("data/aa/", author_name, f1s[x], "_nsc_", page_counts[z], "_", 
                       paste0(word_num[test_list], char_word[test_list]), ".txt")
  file.rename("final_results.txt", results_fn)
  
}



#create author and corpora size plot
f1s <- c(50, 55, 60, 65, 70, 75, 80, 85, 90)
f1_50 <- c()
f1_55 <- c()
f1_60 <- c()
f1_65 <- c()
f1_70 <- c()
f1_75 <- c()
f1_80 <- c()
f1_85 <- c()
f1_90 <- c()
total_pages <- c()
total_docs <- c()
authors <- tail(names(sort(table(author_df$author))),25)
for (i in 1:length(authors)) {
  cat("\nWorking on", authors[i], "\n")
  temp_df <- author_df[which(author_df$author == authors[i]),]
  total_pages <- c(total_pages, nrow(temp_df))
  total_docs <- c(total_docs, length(unique(temp_df$id)))
  for (x in 1:length(f1s)) {
    if (x == length(f1s)) {
      f1_90 <- c(f1_90, length(which(temp_df$f1 >= (as.numeric(f1s[x])/100))))
    } else if (x == 1) {
      f1_50 <- c(f1_50, length(which(temp_df$f1 <= (as.numeric(f1s[x])/100))))
    } else {
      temp_length <- get(paste0("f1_", f1s[x]))
      temp_length <- c(temp_length, length(which(temp_df$f1 >= (as.numeric(f1s[x])/100) & temp_df$f1 < (as.numeric(f1s[x+1])/100))))
      assign(paste0("f1_", f1s[x]), temp_length)
    }
  }
}

authors <- gsub("[.]", "", authors)
authors[5] <- "Griffith, Elizabeth, 1727-1793"
authors[7] <- "Home, Henry (Lord Kames), 1696-1782"
authors[8] <- "Lennox, Charlotte, c. 1729-1804"
authors[11] <- "Leclerc (comte de Buffon), 1707-1788"
authors[22] <- "Defoe, Daniel, 1660-1731"
authors[23] <- "Pratt, Samuel Jackson, 1749-1814"
authors[25] <- "Goldsmith, Oliver, 1728-1774"


plot_df <- data.frame(authors, total_pages, total_docs, f1_50,f1_55 ,f1_60, f1_65, f1_70,
                      f1_75, f1_80, f1_85,  f1_90, stringsAsFactors = FALSE)

colnames(plot_df) <- c("authors", "total_pages", "total_docs", "<55%", "55-60%", "60-65%", "65-70%",      
                       "70-75%", "75-80%", "80-85%", "85-90%", ">90%")

plot_df <- plot_df[order(-plot_df$total_pages),]
plot_df$authors <- factor(plot_df$authors, levels=unique(as.character(plot_df$authors)) )




mm <- plot_df[,-c(2,3)]
mm <- melt(mm, id='authors')


#this one is the working one I Think
ggplot(mm)  + 
  geom_bar(aes(x=authors, y=value, fill=variable),stat="identity",position = position_stack(reverse = TRUE))+
  
  labs(title="Total F1 ranked pages per author") +
  scale_fill_grey(start = .9, end = .2)+
  ylab("Pages") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_blank()) +
  
  coord_flip()



############################################################################################
#pull list of too shrot corps


authors <- tail(names(sort(table(author_df$author))),25)
f1s <- c(50, 55, 60, 65, 70, 75, 80, 85, 90)
page_counts <- c(1000, 2500, 5000, 10000)
short_corps <- c()

#1) Create corpus
#for (i in 6:length(authors)) {
for (i in 1:length(authors)) {
  #for (i in 13:13) {
  cat("\nWorking on", authors[i], "\n")
  temp_df <- author_df[which(author_df$author == authors[i]),]
  #specific f1
  
  for (x in 1:length(f1s)) {
    #for (x in 2:length(f1s)) {
    
    cat("Working on", f1s[x], "F1 range\n")
    #subset corp by F1 score
    if (x == length(f1s)) {
      temp_doc <- temp_df[which(temp_df$f1 >= (as.numeric(f1s[x])/100)),]
    } else if (x == 1) {
      temp_doc <- temp_df[which(temp_df$f1 <= (as.numeric(f1s[x])/100)),]
    } else {
      temp_doc <- temp_df[which(temp_df$f1 >= (as.numeric(f1s[x])/100) & temp_df$f1 < (as.numeric(f1s[x+1])/100)),]
    }
    #get specific number of tokens and create corpora
    
    for (z in 1:length(page_counts)) {
      #for (z in 3:length(page_counts)) {
      
      
      number_of_pages <- mean(ntoken(temp_doc$tcp))
      number_of_pages <- page_counts[z] / number_of_pages
      number_of_pages <- ceiling(number_of_pages)
      if (sum(ntoken(temp_doc$tcp)) < page_counts[z]) {
        temp <- paste0(authors[i], "_", page_counts[z], "_", f1s[x])
        short_corps <- c(short_corps, temp)
        cat("x")
      }
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
