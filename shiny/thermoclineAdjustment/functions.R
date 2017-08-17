library("RMySQL")
library(Rcpp)

getProfile<- function()
{
    db='processed'
    query='SELECT startDate, endDate from profile WHERE startDate IN (SELECT MIN(startDate) FROM profile GROUP BY YEAR(startDate))'
    con <- dbConnect(MySQL(),user="webuser",password="webuserpass",dbname=db,host="")
    profileList <- dbGetQuery(con, query)
    dbDisconnect(con)
    return(profileList[,1])
}
getTe <- function(data){

    media = data$x[1];
    numElem = 1;

    for(i in 1:length(data$x))
    {
        if (data$y[i] > -1)
        {
            media = media + data$x[i]
            numElem = numElem + 1
        }
    }

    return (media/numElem)
}
# faster getTe
sourceCpp("functionsCppOpenMP.cpp")

#Funcion de ajusto, que es el minimo de la suma de la formula que crea la curva
min.RSS <- function(data, par) {
    Te <- getTeCpp(data)
    Th <- min(data$x)
    r <- with(data, sum((x-(Th+((Te-Th)/((1+((par[1]*(-y))^par[2]))^(1-1/par[2])))))^2))
    return (r)
}

#Funcion para hallar la temperatura en la curva ajustada, a partir de alfa y n
mod.RSS <- function(data, par) {
    Te <- getTeCpp(data)
    Th <- min(data$x)
    with(data, (Th+((Te-Th)/((1+((par[1]*(-y))^par[2]))^(1-1/par[2])))))
}


