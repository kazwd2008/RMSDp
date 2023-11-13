# download wine dataset from UCI Machine Learning Repository 
# 174 recordes, 14 variables

tmp <- tempfile()
download.file("http://archive.ics.uci.edu/static/public/109/wine.zip", tmp)
wine <- read.csv(unz(tmp, "wine.data"), header=F)

o1 <- RMSDp(wine[,-1], cores=2)
sum(o1$ot-1) # number of outliers

# plot
# dev.new(width=16, height=16)
pairs(wine[,-1], pch=20, col=o1$ot)
