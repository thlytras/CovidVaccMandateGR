cols <- c("40" = "limegreen", "60" = "tomato")


plotsE <- list()


plotsE$axisWk <- function(x, side=1) {
  i <- which(x %% 10 %in% c(5,0))
  axis(side, at=i, labels=sprintf("%04d-%02d", x[i]%/%100, x[i]%%100), las=1)
  i <- which(x %% 100 == 1)
  axis(side, at=i, labels=sprintf("%04d-%02d", x[i]%/%100, x[i]%%100), las=1)
}

plotsE$markFig2point <- function(wk, pos=2, cex=0.7) {
  if (!wk %in% Fig2dat$wk) return()
  i <- match(wk, Fig2dat$wk)
  text(Fig2dat[i, "l40"], Fig2dat[i, "l60"], wk, pos=pos, cex=cex)
}


plotsE$Fig1 <- function() {
  par(mar=c(2,5,1,2), family="Fira Sans", mfrow=c(2,1))
  plot(vacc40w$n, type="n", ylim=c(0, 2500000), bty="l", xaxt="n", xlab="", yaxt="n",
    ylab="Population still unvaccinated (millions)", main="")
  axis(2, at=axTicks(2), labels=axTicks(2)/10^6)
  abline(v=1:nrow(vacc40w), col="lightgrey", lty="dotted")
  points(vacc40w$n, type="o", lwd=2, col=cols["40"], pch=19, cex=0.7)
  points(vacc60w$n, type="o", lwd=2, col=cols["60"], pch=19, cex=0.7)
  plotsE$axisWk(vacc60w$wk)
  mtext("Week number (ISO)", side=1, line=5)
  abline(v=which(vacc40w==202148), lty="dashed")
  abline(v=which(vacc40w==202202), lty="dashed")
  legend("topright", c("Intervention group (age ≥60)", "Control group (age 40-59)"), col=cols[2:1], pch=19, lwd=2, pt.cex=0.7)

  plot(vacc60w$l, type="n", ylim=c(-6.9,-1.5), bty="l", xaxt="n", xlab="", 
    ylab="Log-rate of weekly first-dose vaccinations", main="")
  abline(v=1:nrow(vacc40w), col="lightgrey", lty="dotted")
  points(vacc40w$l, type="o", lwd=2, col=cols["40"], pch=19, cex=0.7)
  points(vacc60w$l, type="o", lwd=2, col=cols["60"], pch=19, cex=0.7)
  plotsE$axisWk(vacc60w$wk)
  mtext("Week number (ISO)", side=1, line=3)
  abline(v=which(vacc40w==202148), lty="dashed")
  abline(v=which(vacc40w==202202), lty="dashed")
  legend("topright", c("Intervention group (age ≥60)", "Control group (age 40-59)"), col=cols[2:1], pch=19, lwd=2, pt.cex=0.7)
}


plotsE$Fig2 <- function() {
  plot(Fig2dat$l40, Fig2dat$l60, type="l", pch=19, bty="l",
    xlab="Log-rate of weekly first-dose vaccinations, age 40-59", ylab="Log-rate of weekly first-dose vaccinations, age ≥60")
  points(Fig2dat$l40, Fig2dat$l60, col=Fig2dat$col, pch=19)
  m0 <- lm(l60~l40, data=subset(Fig2dat, wk<202148))
  m1 <- lm(l60~l40, data=subset(Fig2dat, wk>=202148 & wk<=202202))
  m2 <- lm(l60~l40, data=subset(Fig2dat, wk>=202202))
  abline(m0, col="blue", lty="dashed")
  abline(m1, col="red", lty="dashed")
  abline(m2, col="magenta", lty="dashed")
  plotsE$markFig2point(202120, 2)
  plotsE$markFig2point(202148, 3)
  plotsE$markFig2point(202213, 4)
  plotsE$markFig2point(202202, 2)
  legend("bottomright", c("Before week 48/2021", "Week 48/2021 to 02/2022", "After week 02/2022"), text.col="white", lty="solid", pch=NA, inset=0.03, bty="n")
  legend("bottomright", c("Before week 48/2021", "Week 48/2021 to 02/2022", "After week 02/2022"), lty=0, lwd=1, pch=19, inset=0.03, bty="n", col=c("blue","red","magenta"))
}


plotsE$Fig3 <- function() {
  with(benefit.wk, {
    plot(est, type="h", lwd=20, lend=1, xlab=NA, ylab="Mandate-attributable vaccinations (age ≥60)", bty="l", ylim=c(0, max(hi)), xaxt="n", col="orange")
    axis(1, at=1:length(est), labels=sprintf("%04d-%02d", wk%/%100, wk%%100), las=2)
    mtext("Week number (ISO)", side=1, line=5)
  })
}


plotsE$Fig4 <- function() {
  with(pred60, {
    plot(0, type="n", xlab=NA, ylab="Population (age ≥60) still unvaccinated (thousands)", bty="l", ylim=c(0, prdat$N[1]), xlim=c(0, length(obs)), xaxt="n", yaxt="n")
    points(0:length(obs), c(prdat0$N[1], obs), type="o", lwd=2, pch=19, cex=0.7, col=cols["60"])
    points(pred.noObl, type="o", lwd=2, pch=19, cex=0.7, col="magenta")
    axis(1, at=0:length(obs), labels=c("2021-47", sprintf("%04d-%02d", wk%/%100, wk%%100)), las=2)
    legend("bottom", c("Observed", "Model-predicted, assuming no mandate"), col=c(cols["60"], "magenta"), lwd=2, pch=19, pt.cex=0.7, bty="n", inset=c(0,0.05))
    abline(v=1, lty="dashed")
    axis(2, at=axTicks(2), labels=axTicks(2)/1000)
    mtext("Week number (ISO)", side=1, line=5)
    arrows(length(obs), rev(obs)[1], length(obs), rev(pred.noObl)[1], code=3, angle=20, length=0.2)
    text(length(obs), mean(c(rev(obs)[1], rev(pred.noObl)[1])), "Mandate-attributable\nvaccinations", pos=2, adj=0)
  })
}




cairo_pdf("Fig1.pdf", width=10, height=7, pointsize=10)
par(family="Fira Sans", oma=c(4,0,1,0))
plotsE$Fig1()
dev.off()

cairo_pdf("Fig2.pdf", width=8, height=7, pointsize=12)
par(mar=c(5,5,2,2), family="Fira Sans")
plotsE$Fig2()
dev.off()

cairo_pdf("Fig3.pdf", width=9, height=7, pointsize=12)
par(mar=c(7,5,2,2), family="Fira Sans")
plotsE$Fig3()
dev.off()

cairo_pdf("Fig4.pdf", width=9, height=7, pointsize=12)
par(mar=c(7,5,2,2), family="Fira Sans")
plotsE$Fig4()
dev.off()

