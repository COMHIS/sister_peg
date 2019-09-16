#rolling tests
library(stylo)

char_word <- c("W", "W", "W", "C", "C", "C", "C", "C")
tok_num <- c(1, 2, 3, 3, 4, 5, 6, 7)

mffeatures <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

#ALL SAMP ALL DATASETS
char_word <- c("W", "C", "W")
tok_num <- c(2, 3, 3)

mffeatures <- c(500, 600, 700, 800, 900, 1000)

#LARGE SAMP ALL DATASETS
char_word <- c("W", "W")
tok_num <- c(2, 3)

mffeatures <- c(400, 500, 600, 700, 800, 900, 1000)

#SMAL SAMP ALL DATASETS
char_word <- c("W", "C", "C", "C")
tok_num <- c(2, 3, 4, 5)

mffeatures <- c(600, 700, 800, 900, 1000)

#SMAL SAMP H+F DATASETS
char_word <- c("W", "C", "C")
tok_num <- c(1, 3, 4)
mffeatures <- c(500, 600, 700, 800, 900, 1000)

#LARGE/TEXTS SAMP H+F DATASETS (SAME FOR ALL SAMPS)
char_word <- c("W", "W", "C", "C")
tok_num <- c(1, 2, 3, 7)
mffeatures <- c(400, 500, 600, 700, 800, 900, 1000)



slice.var <- 4000
overlap.var <- 100

300 = 2282
150 = 1082
100 = 682

#all - first, feature and ngram
for (feature_test in 1:length(char_word)) {
  cat("\r", char_word[feature_test], tok_num[feature_test])
  #second, sample size
  for (x in 6:6) {
    if (x == 1) {
      slice.var <- 5000 #1382
      overlap.var <- 500
    } else if (x == 2) {
      slice.var <- 3000
      overlap.var <- 50 #282
    } else if (x == 3) {
      slice.var <- 4000 #482
      overlap.var <- 100 
    } else if (x == 4) { #132
      slice.var <- 2500
      overlap.var <- 125 
    } else if (x == 5) {
      slice.var <- 5000
      overlap.var <- 150
    } else if (x == 6) {
      slice.var <- 1500
      overlap.var <- 25 #150 - 932 ; 100 - 1482 ; 50 - 682 ; 125 - 507 ; 75 - 1082 ; 25 - 282
    } else if (x == 7) {
      slice.var <- 2000 #182
      overlap.var <- 25 #25 - 
    } else if (x == 8) {
      slice.var <- 1000 #182
      overlap.var <- 10 #25 - 482 ; 10 - 122 ; 
    }
      #third, number of features
    for (i in 1:length(mffeatures)) {
      rolling.classify(plot.legend = TRUE, 
                       #SAMPLE VARS
                       slice.size = slice.var, slice.overlap = overlap.var, 
                       #METHOD
                       classification.method = "svm",
                       milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                            "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                       #MFF
                       mfw=mffeatures[i], 
                       training.set.sampling = "normal.sampling", 
                       #FEATURE
                       analyzed.features=char_word[feature_test], 
                       #NGRAM
                       ngram.size=tok_num[feature_test],
                       #write.svg.file = TRUE,
                       write.png.file = TRUE
      )
    }
    temp_dir <- paste0("images/", slice.var, "_", char_word[feature_test], tok_num[feature_test])
    dir.create(temp_dir)
    
    #svg_temp <- list.files(".", full.names = TRUE, include.dirs = FALSE, pattern = "*[.]svg")
    #use system
    system(paste0("cp *png ", temp_dir))
    system("rm *png")
    #png_temp <- list.files("/mnt/d/HDD Stuff/github/sister_peg/", full.names = TRUE, include.dirs = FALSE, pattern = "*[.]png")
    
    #file.rename(from = "/*.svm",
    #               to = "C:/Users/msc2/Desktop/Halwa/BADMASHI/SCOP/rabata.txt")
    #file.copy(svg_temp, paste0("images/", char_word[feature_test], tok_num[feature_test]))
    #file.copy(png_temp, paste0("images/", slice.var, "_", char_word[feature_test], tok_num[feature_test]))
    #file.remove(svg_temp)
    #file.remove(png_temp)
    #rm(png_temp)
  }
}


#TEMP
for (feature_test in 3:length(char_word)) {
  cat("\r", char_word[feature_test], tok_num[feature_test])
  #second, sample size
  for (x in 2:2) {
    if (x == 1) {
      slice.var <- 5000
      overlap.var <- 500
    } else {
      slice.var <- 2500
      overlap.var <- 200
    }
    #third, number of features
    for (i in 3:length(mffeatures)) {
      rolling.classify(plot.legend = TRUE, 
                       #SAMPLE VARS
                       slice.size = slice.var, slice.overlap = overlap.var, 
                       #METHOD
                       classification.method = "svm",
                       milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                            "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                       #MFF
                       mfw=mffeatures[i], 
                       training.set.sampling = "normal.sampling", 
                       #FEATURE
                       analyzed.features=char_word[feature_test], 
                       #NGRAM
                       ngram.size=tok_num[feature_test],
                       #write.svg.file = TRUE,
                       write.png.file = TRUE
      )
    }
  }
  dir.create(paste0("images/", char_word[feature_test], tok_num[feature_test]))
  #svg_temp <- list.files(".", full.names = TRUE, include.dirs = FALSE, pattern = "*[.]svg")
  png_temp <- list.files(".", full.names = TRUE, include.dirs = FALSE, pattern = "*[.]png")
  #file.rename(from = "/*.svm",
  #               to = "C:/Users/msc2/Desktop/Halwa/BADMASHI/SCOP/rabata.txt")
  #file.copy(svg_temp, paste0("images/", char_word[feature_test], tok_num[feature_test]))
  file.copy(png_temp, paste0("images/", char_word[feature_test], tok_num[feature_test]))
  #file.remove(svg_temp)
  file.remove(png_temp)
}




slice.var <- 5000
overlap.var <- 500

slice.var <- 2500
overlap.var <- 200

slice.var <- 4000
overlap.var <- 100

slice.var <- 2500
overlap.var <- 150 #382
overlap.var <- 125 #132

#1000,900, 800, 700
#3C, 2W, 1W, 4C

features <- c("W", "W", "C", "C")
grams <- c(1, 2, 3, 4)

#small samp, 1000 mfw

for (i in 1:length(features)) {
  rolling.classify(plot.legend = TRUE, slice.size = slice.var, slice.overlap = overlap.var, classification.method = "svm",
                   milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                        "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                   mfw=1000, training.set.sampling = "normal.sampling", analyzed.features=features[i], ngram.size=grams[i]
                   
                   )
}



#1 - mixed( 3, 6-10), 2 mixed (1-3, 4-5, 12), 3 early (1-4), 4 late, 11-13) 

for (i in 1:length(features)) {
  rolling.classify(plot.legend = TRUE, slice.size = slice.var, slice.overlap = overlap.var, classification.method = "svm",
                   milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                        "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                   mfw=900, training.set.sampling = "normal.sampling", analyzed.features=features[i], ngram.size=grams[i])
}

#1 mixed (intro, 3, 7-10), 2 miex (1-3, 12), 4 early (1-4), 4 leate - 11-12 

for (i in 1:length(features)) {
  rolling.classify(plot.legend = TRUE, slice.size = slice.var, slice.overlap = overlap.var, classification.method = "svm",
                   milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                        "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                   mfw=800, training.set.sampling = "normal.sampling", analyzed.features=features[i], ngram.size=grams[i])
}



for (i in 1:length(features)) {
  rolling.classify(plot.legend = TRUE, slice.size = slice.var, slice.overlap = overlap.var, classification.method = "svm",
                   milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                        "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                   mfw=700, training.set.sampling = "normal.sampling", analyzed.features=features[i], ngram.size=grams[i])
}


for (i in 3:length(features)) {
  rolling.classify(plot.legend = TRUE, slice.size = slice.var, slice.overlap = overlap.var, classification.method = "svm",
                   milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                        "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                   mfw=1000, training.set.sampling = "normal.sampling", analyzed.features=features[i], ngram.size=grams[i])
}



rolling.classify(plot.legend = TRUE, slice.size = slice.var, slice.overlap = overlap.var, classification.method = "svm",
                 milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                      "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="W", ngram.size=3)


rolling.classify(plot.legend = TRUE, slice.size = slice.var, slice.overlap = overlap.var, classification.method = "svm",
                 milestone.labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
                                      "X", "XI", "XII - BB", "XIII", "XIV - BE", "XV", "XVI", "XVII"),

                 mfw=1000, 
                 training.set.sampling = "normal.sampling", 
                 #FEATURE
                 analyzed.features="W", 
                 #NGRAM
                 ngram.size=3,
                 #write.svg.file = TRUE,
                 write.png.file = TRUE
)



rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=900, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)

rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=700, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=3)

rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=900, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=3)

rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=700, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=3)


rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=4)

rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=900, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=4)

rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=700, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=4)

#1w

#500 is pretty split
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=500, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#100 is mostly hume, but we know 100 is bad.
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=100, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#300 mostly ferg
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=300, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)

#1000 - mostly ferg
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)

#400 split, but slighly more ferg
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=400, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#600 split, but slighly more ferg
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=600, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#700 split, but slighly more ferg
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=700, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#800 - split
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=800, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#900 - split, mostly ferg
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=900, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#200 - split
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=200, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#7c

#100 - 60% hume?
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=100, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#200 - split
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=200, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#300 - split
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=300, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)



#400  - 50-50ish
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=400, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7,
                 use.existing.freq.tables = TRUE, use.existing.wordlist = TRUE)


#500 - mostly ferg, but strong hume
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=500, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#600 - mostly ferg, but strong hume
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=600, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#700 - slightly mostly hume
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=700, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#800 - split
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=800, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#1000 - mostly hume
rolling.classify(plot.legend = TRUE, slice.size = 1000, slice.overlap = 500, classification.method = "svm", 
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)



#1w

#500 100% ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=500, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#100 mostly ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=100, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#400 - mostly ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=400, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)

#200 - mostly ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=200, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#300 - 100% ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=300, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)

#1000 - 100% ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="w", ngram.size=1)


#7c

###########################################################


#100  - big hume chunk middle
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=100, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#200 - all ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=200, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

###########################################################
#300 - one hume chunk towards end
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=300, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

###########################################################
#400 - all ferg - but a bump at the interesting chunk
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=400, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

###########################################################
#500  - one hume chunk towards end
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=500, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#600 - all ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=600, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#700 - all ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=700, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#800 - all ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=800, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#900 - 100 ferg
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=900, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#1000 - most ferg, bit at start and tiny sliver near middle
rolling.classify(plot.legend = TRUE, slice.size = 5000, slice.overlap = 4500, classification.method = "svm", 
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)






#7c

###################################################
#500  - Hume, same section (and one other)
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=500, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#100  - mos t hume, including section
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=100, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#300 - 50-50, Hume for section, but also other.
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=300, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#400 - split, hume gets section
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=400, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#1000 - Hume, plus all the start.
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=1000, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#600 -split, hume gets section
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=600, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#700 - hume plus much more
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=700, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#800 - lots hume, include section
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=800, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)

#200 - split, hume gets it
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=200, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)


#900 - split two, hume gets bit
rolling.classify(plot.legend = TRUE, slice.size = 3000, slice.overlap = 2500, classification.method = "svm", 
                 mfw=900, training.set.sampling = "normal.sampling", analyzed.features="c", ngram.size=7)




7000 = 6865
2865/2500/2900 = 2365
2000-1865
1500 = 1365
1000 - 865