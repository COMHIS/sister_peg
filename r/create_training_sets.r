#corps for aa for hume-ferg


author_df <- read.csv("../../../Data/code_from_projects/cmlhts_leicester/data/total_corp_with_f1.csv", stringsAsFactors = FALSE)
author_df$tcp <- gsub("ſ", "s", author_df$tcp)
author_df$tcp <- gsub("〈.*?〉", "", author_df$tcp)
authors <- tail(names(sort(table(author_df$author))),25)
#Don't want Hume - already in test
authors[which(authors == "Hume, David, 1711-1776.")] <- "Smith, Adam, 1723-1790."

#create training sets
for (i in 1:length(authors)) {
  temp_df <- author_df[which(author_df$author == authors[i]),]
  docs <- unique(temp_df$id)
  author_name <- gsub(", .*$", "", authors[i])
  for (num_docs in 1:length(docs)) {
    cat("\r", i, "    -     ", num_docs, "           ")
    temp_doc <- temp_df$tcp[which(temp_df$id == docs[num_docs])]
    temp_primary_tcp <- paste0("data/others/", author_name, "_", docs[num_docs], "_tcp.txt")
    fileConn<-file(temp_primary_tcp)
    writeLines(temp_doc, fileConn)
    close(fileConn)
  }
}

