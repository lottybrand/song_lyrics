# R script for the pre-registered analysis described in 
# "The cultural evolution of emotions in 50 years of pop song lyrics"
# authors: Acerbi, Brand, Mesoudi


library(devtools)
devtools::install_github("rmcelreath/rethinking",ref="Experimental")
library(rethinking)

########################################################
bb <- read.csv("billboard_analysis.csv")

bb <- bb[,2:9]
colnames(bb)[1] <- "artist"
colnames(bb)[2] <- "chartPos"
colnames(bb)[8] <- "nLyrics"
bb <- na.omit(bb)

########################################################
# calculate "top ten" and "prestige" columns:

# top ten:
topTen <- bb[(bb$chartPos<11),]
aveTopTenLyrics <- aggregate(topTen$nLyrics, list(topTen$year), mean, na.rm=T)

# average negative score for top ten of each year:
aveTopTen <- aggregate(topTen$negative, list(topTen$year), mean, na.rm=T)
topTenFreq <- aveTopTen$x / aveTopTenLyrics$x
movingAverage = as.vector(filter(topTenFreq, rep(1/3, 3), sides=1))
success_merge_negative = data.frame(year=unique(bb$year), success_negative=c(NA, movingAverage[1:50]))
bb = merge(bb, success_merge_negative, by="year")

# average positive score for top ten of each year:
aveTopTen <- aggregate(topTen$positive, list(topTen$year), mean, na.rm=T)
topTenFreq <- aveTopTen$x / aveTopTenLyrics$x
movingAverage = as.vector(filter(topTenFreq, rep(1/3, 3), sides=1))
success_merge_positive = data.frame(year=unique(bb$year), success_positive=c(NA, movingAverage[1:50]))
bb = merge(bb, success_merge_positive, by="year")

# prestige: 
bb$artCount <- ave( as.numeric(bb[[1]]), bb[["artist"]], FUN=length)
bb$prestigious <- ifelse(bb$artCount>10,1,0)

# just prestigious artists:
prest_bb <- bb[(bb$prestigious==1),]
avePrestLyrics <- aggregate(prest_bb$nLyrics, list(prest_bb$year), mean, na.rm=T)

#average negative score for each year
avePrest <- aggregate(prest_bb$negative, list(prest_bb$year), mean, na.rm=T)
prestFreq <- avePrest$x / avePrestLyrics$x
movingAverage = as.vector(filter(prestFreq, rep(1/3, 3), sides=1))
prestige_merge_negative = data.frame(year=unique(bb$year), prestige_negative=c(NA, movingAverage[1:50]))
bb = merge(bb, prestige_merge_negative, by="year")

#average positive score for each year
avePrest <- aggregate(prest_bb$positive, list(prest_bb$year), mean, na.rm=T)
prestFreq <- avePrest$x / avePrestLyrics$x
movingAverage = as.vector(filter(prestFreq, rep(1/3, 3), sides=1))
prestige_merge_positive = data.frame(year=unique(bb$year), prestige_positive=c(NA, movingAverage[1:50]))
bb = merge(bb, prestige_merge_positive, by="year")

########################################################
# bb dataset models:

d_bb <- na.omit(bb)
d_bb$artist <- coerce_index(as.factor(d_bb$artist))
d_bb$year <- coerce_index(as.factor(d_bb$year))

d_bb$success_positive <- scale(d_bb$success_positive, center = TRUE, scale = TRUE)
d_bb$prestige_positive <- scale(d_bb$prestige_positive, center = TRUE, scale = TRUE)
d_bb$chart_pos <- scale(d_bb$chartPos, center = TRUE, scale = TRUE)
d_bb$success_negative <- scale(d_bb$success_negative, center = TRUE, scale = TRUE)
d_bb$prestige_negative <- scale(d_bb$prestige_negative, center = TRUE, scale = TRUE)

# POSITIVE EMOTIONS

# NULL MODEL FOR POSITIVE:
bb_pos_null <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- a + z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter =1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13))

print("finished Null")

# H1 MODEL (success bias) FOR POSITIVE:
bb_pos_h1 <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- a + success_positive*b +
      z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13))

print ("finished H1")

# H2 MODEL (prestige bias) FOR POSITIVE:
bb_pos_h2 <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- a + prestige_positive*b +
      z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13))

print("finished H2")

# FULL MODEL FOR POSITIVE:
bb_pos_full <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- a + success_positive*bs + prestige_positive*bp +  
      z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    bs ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13))

print("finished Full")
plot(precis(bb_pos_full), pars=c("bs","bp"), labels=c("Prestige","Success"))
############################################################
# NEGATIVE EMOTIONS

# NULL MODEL FOR NEGATIVE:
bb_neg_null <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- a + z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control = list( adapt_delta=0.99, max_treedepth=13))

# H1 MODEL (success bias) FOR NEGATIVE:
bb_neg_h1 <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- a + success_negative*b +
      z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control = list( adapt_delta=0.99, max_treedepth=13))

print("finished neg H1")

# H2 MODEL (prestige bias) FOR NEGATIVE:
bb_neg_h2 <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- a + prestige_negative*b +
      z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control = list(adapt_delta=0.99, max_treedepth=13))
print("finished neg H2")

# H3 MODEL (content bias) FOR NEGATIVE:
bb_neg_h3 <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- a + chart_pos*b +
      z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13))
print("finished neg H3")

# FULL MODEL FOR NEGATIVE:

bb_neg_full <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- a + success_negative*bs + prestige_negative*bp + chart_pos*bc + 
      z_a[artist]*sigma_a + z_y[year]*sigma_y,
    a ~ dnorm(0,1),
    bs ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bc ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_y[year] ~ dnorm(0,1),
    c(sigma_a, sigma_y) ~ normal(0,0.1)
  ),
  data=d_bb, chains=3, cores=3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_y="lower=0", sigma_a="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13))
print("finished neg Full")

precis(bb_neg_full)
plot(precis(bb_neg_full), pars=c("bs","bp","bc"), labels=c("Chart Position","Prestige","Success"))


########################################################
# mxm dataset models:

mxm <- read.csv("mxm_analysis.csv")
mxm <- mxm[,2:8]
colnames(mxm)[1] <- "artist"
colnames(mxm)[7] <- "nLyrics"
mxm <- na.omit(mxm)

mxm = merge(mxm, success_merge_negative, by="year")
mxm = merge(mxm, success_merge_positive, by="year")

mxm = merge(mxm, prestige_merge_negative, by="year")
mxm = merge(mxm, prestige_merge_positive, by="year")


d_mxm <- na.omit(mxm)

#force index ourselves:
NArtists = length(unique(d_mxm$artist))
OldID <- d_mxm$artist
artistID <- array(0,length(d_mxm$artist))
for (index in 1:NArtists){
  artistID[OldID == unique(OldID)[index]] = index
}
d_mxm$artist <- artistID

#d_mxm$artist <- coerce_index(as.factor(d_mxm$artist))
#d_mxm$artist <- coerce_index(d_mxm$artist)

NYears = length(unique(d_mxm$year))
OldID <- d_mxm$year
yearID <- array(0,length(d_mxm$year))
for (index in 1:NYears){
  yearID[OldID == unique(OldID)[index]] = index
}
d_mxm$year <- yearID

#d_mxm$year <- coerce_index(as.factor(d_mxm$year))

NGenres = length(unique(d_mxm$genre))
OldID <- d_mxm$genre
genreID <- array(0,length(d_mxm$genre))
for (index in 1:NGenres){
  genreID[OldID == unique(OldID)[index]] = index
}
d_mxm$genre <- genreID

#d_mxm$genre <- coerce_index(as.factor(d_mxm$genre))

d_mxm$success_positive <- scale(d_mxm$success_positive, center = TRUE, scale = TRUE)
d_mxm$prestige_positive <- scale(d_mxm$prestige_positive, center = TRUE, scale = TRUE)
d_mxm$success_negative <- scale(d_mxm$success_negative, center = TRUE, scale = TRUE)
d_mxm$prestige_negative <- scale(d_mxm$prestige_negative, center = TRUE, scale = TRUE)

############################################################

# POSITIVE EMOTIONS

# NULL MODEL FOR POSITIVE:
mxm_pos_null <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0, 1),
    z_g[genre] ~ dnorm(a, sigma_g),
    z_y[year] ~ dnorm(0, sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter = 1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))

# H1 MODEL (success bias) FOR POSITIVE:
mxm_pos_h1 <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- success_positive*b +
      z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0, 1),
    z_g[genre] ~ dnorm(a, sigma_g),
    z_y[year] ~ dnorm(0, sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter=1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))

# H2 MODEL (prestige bias) FOR POSITIVE

mxm_pos_h2 <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- prestige_positive*b +
      z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0, 1),
    z_g[genre] ~ dnorm(a, sigma_g),
    z_y[year] ~ dnorm(0, sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter = 1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))

# FULL MODEL FOR POSITIVE:
origStart <-Sys.time()
mxm_pos_full <- map2stan(
  alist(
    positive ~ dbinom(nLyrics, p),
    logit(p) <- success_positive*bs + prestige_positive*bp +
      z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    bs ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_g[genre] ~ dnorm(a,sigma_g),
    z_y[year] ~ dnorm(0,sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter = 1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))
origFin<- Sys.time()

plot(precis(mxm_pos_full), pars=c("bs","bp"), labels=c("Prestige","Success"))
#####################################################
# NEGATIVE EMOTIONS

# NULL MODEL FOR NEGATIVE:
origStart <- Sys.time()
mxm_neg_null <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_g[genre] ~ dnorm(a,sigma_g),
    z_y[year] ~ dnorm(0,sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter =1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))
origFin <- Sys.time()
precis(mxm_neg_null)

# H1 MODEL (success bias) FOR NEGATIVE:
origStart1 <-Sys.time()
mxm_neg_h1 <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- success_negative*b +
      z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_g[genre] ~ dnorm(a,sigma_g),
    z_y[year] ~ dnorm(0,sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter = 1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))
origFin1 <- Sys.time()

print("Finished mxm_neg_h1")

# H2 MODEL (prestige bias) FOR NEGATIVE
origStart2 <- Sys.time()
mxm_neg_h2 <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- prestige_negative*b +
      z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    b ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_g[genre] ~ dnorm(a,sigma_g),
    z_y[year] ~ dnorm(0,sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter = 1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))
precis(mxm_neg_h2)
origFin2 <- Sys.time()

print("finished mxm_neg_h2")

# FULL MODEL FOR NEGATIVE:
origStart3 <- Sys.time()
mxm_neg_full <- map2stan(
  alist(
    negative ~ dbinom(nLyrics, p),
    logit(p) <- success_negative*bs + prestige_negative*bp +
      z_a[artist]*sigma_a + z_g[genre]+ z_y[year],
    a ~ dnorm(0,1),
    bs ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    z_a[artist] ~ dnorm(0,1),
    z_g[genre] ~ dnorm(a,sigma_g),
    z_y[year] ~ dnorm(0,sigma_y),
    c(sigma_a, sigma_g, sigma_y) ~ normal(0,0.1)
  ),
  data=d_mxm, chains=3, cores = 3, iter = 1200, WAIC=TRUE, 
  constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_y="lower=0"), control=list( adapt_delta=0.99 , max_treedepth=13 ))
origFin3<-Sys.time()

print("finished mxm_neg_full")
plot(precis(mxm_neg_full), pars=c("bs","bp"), labels=c("Prestige","Success"))
#####

