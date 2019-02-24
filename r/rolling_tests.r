#rolling tests

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