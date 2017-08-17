# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
library("RMySQL")

# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
     all_cons <- dbListConnections(MySQL())

     print(all_cons)

  for(con in all_cons)
    +  dbDisconnect(con)

    db='processed'
    query=paste('SELECT startDate, endDate from profile WHERE startDate=','\'',input$date,'\'')
    print(query)
    con <- dbConnect(MySQL(),user="webuser",password="webuserpass",dbname=db,host="")
    profile <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    # last date year
    db='processed'
    query=paste('SELECT startDate, endDate from profile WHERE endDate IN (SELECT MAX(endDate) FROM profile WHERE YEAR(endDate)=YEAR(','\'',input$date,'\'))')
    print(query)
    con <- dbConnect(MySQL(),user="webuser",password="webuserpass",dbname=db,host="")
    profileFin <- dbGetQuery(con, query)
    dbDisconnect(con)

    query=paste('SELECT temp, depth from AMT where date BETWEEN \'',profile$startDate[1],'\' and \'',profileFin$endDate[1],'\'  and temp is not null ORDER BY depth ')
    con <- dbConnect(MySQL(),user="webuser",password="webuserpass",dbname=db,host="")
    profile <- dbGetQuery(con, query)
    dbDisconnect(con)
    dat=data.frame(x=profile$temp, y=(-1)*profile$depth)
    print(length(dat$x))
    source("functions.R")
    print(min(dat$x))
    print(getTeCpp(dat))
    if(length(dat$x) > 9)
    {
        #Hallo los distintos parametros
        Te <- getTeCpp(dat)
        Th <- min(dat$x)
        #Optim es el solver que ajusta la curva a los valores de temp
        result <- optim(par = c(0, 10), min.RSS, data = dat)
        alfa <- result$par[1]
        n <- result$par[2]
        m <- 1-1/n
        Zt <- (1/alfa)*((1-(1/n))^(1-(1-(1/n))))
        TZt <- Th+((Te-Th)/((1+((alfa*(Zt))^n))^(1-1/n)))
        TZtprima <- (-1)*(((Te-Th)*(n-1)*((alfa*Zt)^n)*((((alfa*Zt)^n)+1)^((1/n)-2)))/(Zt))
        Zu <- Zt-((TZt-Te)/(TZtprima))
        Zb <- Zt-((TZt-Th)/(TZtprima))
        S <- (Te-Th)/(Zb-Zu)
        mod <- data.frame(x=mod.RSS(dat, c(alfa,n)),y=dat$y)
        plot(y ~ x, data = dat,col=3:3,xlab="Temperature (ºC)",ylab="Depth (m)")
        title(paste("Profile: ",input$date))
        legend('topleft', c('Real','Adjustment') , lty=1, col=c('green', 'blue', 'green',' brown'), bty='n', cex=.95)
        lines(mod$x,mod$y,col=4:4) 
    }
  }, height = 400, width = 600)
}
