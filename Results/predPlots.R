# full negative model: 
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

## aggregated binomial example on 306 uses postcheck:
##postcheck(bb_neg_full, n=1e4)


#mixture of 377 and 297??: 

#posterior predictions with prestige measure:
post<-extract.samples(bb_neg_full)

prestige.seq<- seq(-3,3,1)
#dummy data to predict over:
d.pred <- list(
  prestige_negative=prestige.seq,
  success_negative=rep(1,7),
  chart_pos=rep(1,7),
  artist=rep(1,7),
  year=rep(1,7)
)

link.bb_neg_full <- link(bb_neg_full, data=d.pred)
pred.p <- apply(link.bb_neg_full, 2, mean)
pred.p.PI <- apply(link.bb_neg_full, 2, PI)

#empty plot:
plot(0, 0, type='n', xlab="Prestige Measure", ylab="Proportion Negative Lyrics", ylim=c(0,0.1),
     xaxt="n", xlim=c(-3,3))
axis(1, at=-3:3, labels=c(-3,-2,-1,0,1,2,3) )

p <- by(d_bb$negative, d_bb$prestige_negative, mean)
lines(-3:3, pred.p)
shade(pred.p.PI, -3:3)

#trying for each year:
plot(0, 0, type='n', xlab="Prestige Measure", ylab="Proportion Negative Lyrics", ylim=c(0,0.1),
     xaxt="n", xlim=c(-3,3))
axis(1, at=-3:3, labels=c(-3,-2,-1,0,1,2,3) )

p <- by(d_bb$negative, d_bb$prestige_negative, mean)
lines(-3:3, artist)
shade(pred.p.PI, -3:3)

#### chart position:

chart.seq<- seq(-3,3,1)
#dummy data to predict over:
d.pred <- list(
  chart_pos=chart.seq,
  prestige_negative=rep(1,5),
  success_negative=rep(1,5),
  artist=rep(1,5),
  year=rep(1,5)
)

link.bb_neg_full <- link(bb_neg_full, data=d.pred)
pred.p <- apply(link.bb_neg_full, 2, mean)
pred.p.PI <- apply(link.bb_neg_full, 2, PI)


plot(0, 0, type='n', xlab="Chart Position", ylab="Proportion Negative Lyrics", ylim=c(0,0.1),
     xaxt="n", xlim=c(-3,3))
axis(1, at=-3:3, labels=c(-3,-2,-1,0,1,2,3) )

p <- by(d_bb$negative, d_bb$chart_pos, mean)
lines(-3:3, pred.p)
shade(pred.p.PI, -3:3)


############ absolute v relative
post<- extract.samples(bb_neg_full)
p.pres <- logistic(post$a + post$bp)
p.int <- logistic(post$a)
diff.prest <- p.pres-p.int
diff.prest
quantile(diff.prest, c(0.025,0.5,0.975))

logistic(-4.14 + 0.08)

precis(bb_neg_h3)
logistic(-4.2 + -0.02)
logistic(-4.2)
