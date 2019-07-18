#setwd("~/Desktop/Postdoc/Acerbi_songLyrics/song_lyrics/Results")

#merging plots
bb_neg_full_original <- readRDS("bb_neg_full_original")
bb_pos_full_original <- readRDS("bb_pos_full_original")
mxm_neg_full_original <- readRDS("mxm_neg_full_original")
mxm_pos_full_original <- readRDS("mxm_pos_full_original")

#load figs
fig2a<- plot(precis(bb_pos_full_original), pars=c("bs","bp"), labels=c("Success bias","Prestige bias"), xlab="Estimate")
fig2b <- plot(precis(bb_pos_full), pars=c("bs","bp","ba"), labels=c("Success bias","Prestige bias","Unbiased transmission"), xlab="Estimate")
fig3a<- plot(precis(bb_neg_full_original), pars=c("bs","bp","bc"), labels=c("Success bias","Prestige bias","Chart Position"), xlab="Estimate")
fig3b <- plot(precis(bb_neg_full), pars=c("bs","bp","bc","bna"), labels=c("Success bias","Prestige bias","Content Bias","Unbiased transmission"), xlab="Estimate")

fig4a<- plot(precis(mxm_pos_full_original), pars=c("bs","bp"), labels=c("Success bias","Prestige bias"), xlab="Estimate")
fig4b <- plot(precis(mxm_pos_full), pars=c("bs","bp","ba"), labels=c("Success bias","Prestige bias","Unbiased transmission"), xlab="Estimate")
fig5a<- plot(precis(mxm_neg_full_original), pars=c("bs","bp"), labels=c("Success bias","Prestige bias"), xlab="Estimate")
fig5b <- plot(precis(mxm_neg_full), pars=c("bs","bp","bna"), labels=c("Success bias","Prestige bias","Unbiased transmission"), xlab="Estimate")


# 4 figures arranged in 2 rows and 2 columns
# CHECK: sometimes labels needs to be written top to bottom, sometimes bottom to top, depending on Rethinking version (have tried updating)
attach(mtcars)
par(mfrow=c(2,2))
fig2a<- plot(precis(bb_pos_full_original), pars=c("bs","bp"), labels=c("Prestige bias","Success bias"), xlab="Estimate")
title("a. Positive billboard model", adj = 0)
fig2b <- plot(precis(bb_pos_full), pars=c("bs","bp","ba"), labels=c("Unbiased transmission","Prestige bias","Success bias"), xlab="Estimate")
title("b. Positive billboard model\n(unbiased included)", adj = 0)
fig3a<- plot(precis(bb_neg_full_original), pars=c("bs","bp","bc"), labels=c("Content bias","Prestige bias","Success bias"), xlab="Estimate")
title("c. Negative billboard model", adj = 0)
fig3b <- plot(precis(bb_neg_full), pars=c("bs","bp","bc","bna"), labels=c("Unbiased transmission","Content bias","Prestige bias","Success bias"), xlab="Estimate")
title("d. Negative billboard model\n(unbiased included)", adj = 0)

par(mfrow=c(2,2))
fig4a<- plot(precis(mxm_pos_full_original), pars=c("bs","bp"), labels=c("Prestige bias","Success bias"), xlab="Estimate")
title("a. Positive mxm model", adj = 0)
fig4b <- plot(precis(mxm_pos_full), pars=c("bs","bp","ba"), labels=c("Unbiased transmission","Prestige bias","Success bias"), xlab="Estimate")
title("b. Positive mxm model\n(unbiased included)", adj = 0)
fig5a<- plot(precis(mxm_neg_full_original), pars=c("bs","bp"), labels=c("Prestige bias","Success bias"), xlab="Estimate")
title("c. Negative mxm model", adj = 0)
fig5b <- plot(precis(mxm_neg_full), pars=c("bs","bp","bna"), labels=c("Unbiased transmission","Prestige bias","Success bias"), xlab="Estimate")
title("d. Negative mxm model\n(unbiased included)", adj = 0)

