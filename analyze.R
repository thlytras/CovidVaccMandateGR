library(Rivets)
library(lme4)

vacc60w <- read.csv2("vacc60w.csv")
vacc40w <- read.csv2("vacc40w.csv")

vacc60w$d <- c(NA, -diff(vacc60w$n))
vacc60w$l <- c(NA, log(1 - vacc60w$n[-1]/vacc60w$n[-nrow(vacc60w)]))
vacc60w$N <- c(NA, vacc60w$n[-nrow(vacc60w)])
vacc60w$age <- 1
vacc60w$t <- 1:nrow(vacc60w)
vacc60w$t1_obl <- pmax(0, vacc60w$t - which(vacc60w$wk==202148))
vacc60w$t2_obl <- pmax(0, vacc60w$t1_obl - 6)  # Corresponding to week 202102
vacc60w$t1_obl[vacc60w$t1_obl>6] <- 6

vacc40w$d <- c(NA, -diff(vacc40w$n))
vacc40w$l <- c(NA, log(1 - vacc40w$n[-1]/vacc40w$n[-nrow(vacc40w)]))
vacc40w$N <- c(NA, vacc40w$n[-nrow(vacc40w)])
vacc40w$age <- 0
vacc40w$t <- 1:nrow(vacc40w)
vacc40w$t1_obl <- 0
vacc40w$t2_obl <- 0


dat <- rbind(vacc60w[-1,], vacc40w[-1,])
dat$obl <- 0
dat$obl[dat$age==1 & dat$wk>=202148] <- 1
dat$olre <- 1:nrow(dat)

mm <- glmer(d ~ age + obl + t1_obl + t2_obl + (1|t) + (1|olre), offset=log(N), family="poisson", data=dat, control=glmerControl(optimizer="bobyqa"))


Fig2dat <- merge(vacc40w[,c("wk","l")], vacc60w[,c("wk","l")], by="wk")
names(Fig2dat) <- c("wk", "l40", "l60")
Fig2dat$col <- "blue"
Fig2dat$col[Fig2dat$wk>=202148] <- "red"
Fig2dat$col[Fig2dat$wk>202202] <- "magenta"


prdat <- subset(dat, wk>=202148 & age==1)[,c("wk","N","n","d","age","t","obl","t1_obl", "t2_obl", "olre")]
prdat$t1_obl <- 0
prdat$t2_obl <- 0
prdat$obl <- 0

bb <- bootMer(mm, function(m) {
  cat(".")
  predict(m,prdat)
}, nsim=50000, ncpus=4, parallel="multicore", re.form=NULL)

bb.cumprod <- apply(1-exp(bb$t), 1, function(x) cumprod(x))

pred60 <- subset(dat, wk>=202148 & age==1)[,c("wk","n")]
names(pred60)[2] <- "obs"
pred60$pred.noObl <- round(prdat$N[1]*cumprod(1-exp(predict(mm,prdat,re.form=NULL))))

benefit.obl <- round(dat$N[dat$age==1 & dat$wk==202148] * quantile(bb.cumprod[nrow(bb.cumprod),], c(0.5, 0.025, 0.975)) - rev(dat$n[dat$age==1])[1])

benefit.wk <- as.data.frame(t(round(apply(apply(bb.cumprod, 2, function(x) prdat$d + diff(dat$N[dat$age==1 & dat$wk==202148] * c(1, x))), 1, quantile, probs = c(0.5, 0.025, 0.975)))))
names(benefit.wk) <- c("est", "lo", "hi")
benefit.wk$wk <- prdat$wk
benefit.wk <- benefit.wk[,c(4,1:3)]


RRs <- (function(){
  sel <- c("obl","t1_obl","t2_obl")
  fd <- subset(dat, age==1 & wk>=202148)[,c("wk",sel)]
  betas <- as.matrix(fd[,-1]) %*% fixef(mm)[sel]
  vr <- diag(as.matrix(fd[,-1]) %*% tcrossprod(vcov(mm)[sel,sel], as.matrix(fd[,-1])))
  res <- as.data.frame.matrix(cbind(wk=fd$wk, exp(cbind(betas, sqrt(vr)) %*% rbind(1, c(0, -1.96, 1.96)))))
  colnames(res)[-1] <- c("RR", "lo", "hi")
  res
})()
