library("lattice")
library("RMySQL")
db='processed'
query='SELECT depth, temp, UNIX_TIMESTAMP(date) FROM AMT WHERE  DATE(date) BETWEEN \'2014-8-11\' AND \'2014-9-12\'  and depth is not null ORDER BY date'
con <- dbConnect(MySQL(),user="webuser",password="webuserpass",dbname=db,host="doriiie02.ifca.es")
print(query)
result <- dbGetQuery(con, query)
dbDisconnect(con)

result.df = data.frame(x = result[,3], y = result[,1], z = result[,2])
result.loess = loess(z ~ x*y, data = result.df, degree = 2, span=0.75)

result.fit = expand.grid(list(x = seq(min(result.df$x), max(result.df$x), 1440), y = seq(min(result.df$y),max(result.df$y),0.1)))
z = predict(result.loess, newdata = result.fit)
result.fit$Height = as.numeric(z)
lowestValue <- min(result.fit$Height)
secondHighestValue <- unique(sort(result.fit$Height, decreasing=TRUE))[2]
numberOfColorBins <- 25
col.seq <- seq(lowestValue, secondHighestValue, length.out=numberOfColorBins)
brks <- c(0, col.seq, Inf)
cuts <- cut(result.fit$Height, breaks=brks)
colors <- colorRampPalette(c("green", "red"))(length(levels(cuts))-1)
colors <- c(colors, "black")
cls <- rep(colors, times=table(cuts))
colors <- colorRampPalette(c("#0040FF","#FFFFFF", "#FF0000"))(length(levels(cuts))-1)
param = 'Temperature'
levelplot(Height ~ x*y, data = result.fit, xlab = "Date", ylab = "Depth (m)", main = param, col.regions = colors, ylim = c(max(result.df$y),min(result.df$y)), xlim = c(as.POSIXct(min(result.df$x), origin="1970-01-01"), as.POSIXct(max(result.df$x), origin="1970-01-01")))
