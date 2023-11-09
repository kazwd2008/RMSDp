
# download wine dataset from UCI Machine Learning Repository 
# 174 recordes, 14 variables

tmp <- tempfile()
download.file("https://archive.ics.uci.edu/static/public/109/wine.zip", tmp)
wine <- read.csv(unz(tmp, "wine.data"), header=F)

o1 <- RMSDp(wine[,-1])
sum(o1$ot-1) # number of outliers

# plot1
MASS::parcoord(wine[,-1], col=ot1$ot, lty=c(3,1)[ot1$ot], lwd=c(1,2)[ot1$ot])

# plot2
dev.new(width=16, height=16)
pairs(wine[,-1], pch=20, col=o1$ot)
