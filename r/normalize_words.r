#correct words in dataset

library(readtext)

setwd("/mnt/d/HDD Stuff/github/sister_peg/")

#clean
to_edit <- list.files("back_up_test_data/unmerged_unedited/", full.names = TRUE)
to_save <- list.files("back_up_test_data/unmerged_unedited/", full.names = FALSE)


to_delete <- c("p",
              "c",
              "lib",
              "vol",
              "de",
              "h",
              "ii",
              "s",
              "m",
              "o",
              "t",
              "sect",
              "d",
              "l",
              "v",
              "chron",
              "w",
              "chap",
              "th",
              "r",
              "e",
              "b",
              "ibid",
              "x",
              "n",
              "ed",
              "iv",
              "re-",
              "liv",
              "tion",
              "f",
              "k",
              "vi",
              "con-",
              "ing",
              "ad",
              "in-",
              "hist",
              "pro-",
              "warwic",
              "de-",
              "u",
              "y",
              "vii",
              "id",
              "viii",
              "fide",
              "g",
              "j",
              "la",
              "xv",
              "z",
              "abb",
              "dif-",
              "pre-",
              "be-",
              "com-"
              
)

to_replace <- c("tho",
               "tis",
               "antient",
               "betwixt",
               "wou\\^d",
               "connexion",
               "deriv\\^d",
                "shou\\^d",
                "thro",
                "queen\\^s",
                "henry\\^s",
                "suppos\\^d",
                "partizans",
                "cou\\^d",
                "consider\\^d",
                "enterprizes",
                "observ\\^d",
                "man\\^s",
                "allow\\^d",
                "establish\\^d",
                "woud",
                "'twill",
                "'twoud",
                "twou'd",
                "continu\\^d",
                "mary\\^s",
                "esteem\\^d",
                "produc\\^d",
                "edward\\^s",
                "plac\\^d",
                "determin\\^d",
                "explain\\^d",
                "conjoin\\^d",
                "employ\\^d",
                "receiv\\^d",
                "pope\\^s",
                "ihould",
                "heav'n",
                "king\\^s"
)

to_replace_with <- c("though",
                     "it is",
                     "ancient",
                     "between",
                     "would",
                     "connection",
                     "derived",
                     "should",
                     "through",
                     "queen's",
                     "henry's",
                     "supposed",
                     "partisans",
                     "could",
                     "considered",
                     "enterprises",
                     "observed",
                     "man's",
                     "allowed",
                     "established",
                     "would",
                     "it will",
                     "it would",
                     "it would",
                     "continued",
                     "mary's",
                     "esteemed",
                     "produced",
                     "edward's",
                     "placed",
                     "determined",
                     "explained",
                     "conjoined",
                     "employed",
                     "received",
                     "pope's",
                     "should",
                     "heaven",
                     "king's"
)

for (file_edit in 1:length(to_edit)) {
  cat("\r", file_edit)
  edit_data <- read.delim(to_edit[file_edit], header = FALSE, stringsAsFactors = FALSE)
  edit_data$V1 <- gsub("'d", "ed", edit_data$V1, ignore.case = FALSE)
  for (i in 1:length(to_replace)) {
    edit_data$V1 <- gsub(c(paste0("(\\s|[[:punct:]])", to_replace[i], "(\\s|[[:punct:]])")), c(paste0(" ", to_replace_with[i], " ")), edit_data$V1, ignore.case = TRUE)
  }
  for (i in 1:length(to_delete)) {
    edit_data$V1 <- gsub(c(paste0("(\\s|[[:punct:]])", to_delete[i], "(\\s|[[:punct:]])")), " ", edit_data$V1, ignore.case = TRUE)
  }
  edit_data$V1 <- gsub("^s", "'s", edit_data$V1, ignore.case = FALSE)
  write.table(edit_data, file = paste0("back_up_test_data/unmerged_edited/", to_save[file_edit]), append = FALSE,
              row.names = FALSE, col.names = FALSE)
}

# for (i in 1:length(to_delete)) { 
#   cat("\n", i)
#   cat(gsub(c(paste0("\\s", to_delete[i], "\\s")), " ", edit_data$V1[1], ignore.case = TRUE))
# }

#create dirs with data

dirs <- list.dirs(path = "back_up_test_data/primary_set_backup_edited/")
dirs <- dirs[-c(grep("_merg", dirs))]
dirs <- dirs[-c(1)]

for(move_files in 1:length(dirs)) {
  cat("\r", move_files)
  to_copy <- list.files(dirs[move_files])
  file.copy(c(paste0("back_up_test_data/unmerged_edited/", to_copy)), dirs[move_files], overwrite = TRUE)
}

  
dirs <- list.dirs(path = "back_up_test_data/secondary_set_backup_edited/")
dirs <- dirs[-c(grep("_merg", dirs))]
dirs <- dirs[-c(1)]

for(move_files in 1:length(dirs)) {
  cat("\r", move_files)
  to_copy <- list.files(dirs[move_files])
  file.copy(c(paste0("back_up_test_data/unmerged_edited/", to_copy)), dirs[move_files], overwrite = TRUE)
}



########################
#merge data
merge_dirs <- list.dirs(path = "back_up_test_data/secondary_set_backup_edited/")
merge_dirs <- merge_dirs[-c(grep("_unmerg", merge_dirs))]
merge_dirs <- merge_dirs[-c(1)]
unmerge_dirs <- list.dirs(path = "back_up_test_data/secondary_set_backup_edited/")
unmerge_dirs <- unmerge_dirs[-c(grep("_merg", unmerge_dirs))]
unmerge_dirs <- unmerge_dirs[-c(1)]

for(move_files in 1:length(unmerge_dirs)) {
  cat("\r", move_files)
  to_merge <- list.files(unmerge_dirs[move_files])
  authors <- c("Hume", "Ferguson", "Carlyle", "Addison", "Blair", "Burke", "Kames", "Lennox", "Mcpherson", "Smith")
  for (i in 1:length(authors)) {
    if(any(grepl(authors[i], to_merge) == TRUE)) { 
      system(paste0("cat ", unmerge_dirs[move_files], "/", authors[i], "*.txt > ", merge_dirs[move_files], "/", authors[i], "_test.txt"))
    } else { next }
  }
  file.copy("back_up_test_data/unmerged_edited/EB_History.txt", merge_dirs[move_files])
  file.copy("back_up_test_data/unmerged_edited/sister_peg_checked.txt", merge_dirs[move_files])
  file.copy("back_up_test_data/unmerged_edited/not_bumbo.txt", merge_dirs[move_files])
  file.copy("back_up_test_data/unmerged_edited/peg_bumbo.txt", merge_dirs[move_files])
}

merge_dirs <- list.dirs(path = "back_up_test_data/primary_set_backup_edited//")
merge_dirs <- merge_dirs[-c(grep("_unmerg", merge_dirs))]
merge_dirs <- merge_dirs[-c(1)]
unmerge_dirs <- list.dirs(path = "back_up_test_data/primary_set_backup_edited//")
unmerge_dirs <- unmerge_dirs[-c(grep("_merg", unmerge_dirs))]
unmerge_dirs <- unmerge_dirs[-c(1)]

for(move_files in 1:length(unmerge_dirs)) {
  cat("\r", move_files)
  to_merge <- list.files(unmerge_dirs[move_files])
  authors <- c("Hume", "Ferguson", "Carlyle", "Addison", "Blair", "Burke", "Kames", "Lennox", "Mcpherson", "Smith_")
  for (i in 1:length(authors)) {
    if(any(grepl(authors[i], to_merge) == TRUE)) { 
      system(paste0("cat ", unmerge_dirs[move_files], "/", authors[i], "*.txt > ", merge_dirs[move_files], "/", authors[i], "_primary.txt"))
    } else { next }
  }
}


#remerge old data

#merge data
merge_dirs <- list.dirs(path = "back_up_test_data/secondary_set_backup/")
merge_dirs <- merge_dirs[-c(grep("_unmerg", merge_dirs))]
merge_dirs <- merge_dirs[-c(1)]
unmerge_dirs <- list.dirs(path = "back_up_test_data/secondary_set_backup/")
unmerge_dirs <- unmerge_dirs[-c(grep("_merg", unmerge_dirs))]
unmerge_dirs <- unmerge_dirs[-c(1)]

for(move_files in 1:length(unmerge_dirs)) {
  cat("\r", move_files)
  to_merge <- list.files(unmerge_dirs[move_files])
  authors <- c("Hume", "Ferguson", "Carlyle", "Addison", "Blair", "Burke", "Kames", "Lennox", "Mcpherson", "Smith")
  for (i in 1:length(authors)) {
    if(any(grepl(authors[i], to_merge) == TRUE)) { 
      system(paste0("cat ", unmerge_dirs[move_files], "/", authors[i], "*.txt > ", merge_dirs[move_files], "/", authors[i], "_test.txt"))
    } else { next }
  }
  file.copy("back_up_test_data/unmerged_unedited/EB_History.txt", merge_dirs[move_files])
  file.copy("back_up_test_data/unmerged_unedited/sister_peg_checked.txt", merge_dirs[move_files])
  file.copy("back_up_test_data/unmerged_unedited/not_bumbo.txt", merge_dirs[move_files])
  file.copy("back_up_test_data/unmerged_unedited/peg_bumbo.txt", merge_dirs[move_files])
}

merge_dirs <- list.dirs(path = "back_up_test_data/primary_set_backup/")
merge_dirs <- merge_dirs[-c(grep("_unmerg", merge_dirs))]
merge_dirs <- merge_dirs[-c(1)]
unmerge_dirs <- list.dirs(path = "back_up_test_data/primary_set_backup/")
unmerge_dirs <- unmerge_dirs[-c(grep("_merg", unmerge_dirs))]
unmerge_dirs <- unmerge_dirs[-c(1)]

for(move_files in 1:length(unmerge_dirs)) {
  cat("\r", move_files)
  to_merge <- list.files(unmerge_dirs[move_files])
  authors <- c("Hume", "Ferguson", "Carlyle", "Addison", "Blair", "Burke", "Kames", "Lennox", "Mcpherson", "Smith_")
  for (i in 1:length(authors)) {
    if(any(grepl(authors[i], to_merge) == TRUE)) { 
      system(paste0("cat ", unmerge_dirs[move_files], "/", authors[i], "*.txt > ", merge_dirs[move_files], "/", authors[i], "_primary.txt"))
    } else { next }
  }
}




