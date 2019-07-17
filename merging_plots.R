#setwd("~/Desktop/Postdoc/Acerbi_songLyrics/song_lyrics/Results")

#merging plots
bb_neg_full_original <- readRDS("bb_neg_full_original")
bb_pos_full_original <- readRDS("bb_pos_full_original")
mxm_neg_full_original <- readRDS("mxm_neg_full_original")
mxm_pos_full_original <- readRDS("mxm_pos_full_original")

# 4 figures arranged in 2 rows and 2 columns
attach(mtcars)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")