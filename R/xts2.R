library(influxdbr2)
library(xts)
library(foreach)
library(caret)
library(XML)
library(pmml)
library(r2pmml)

con<-influxdbr2::influx_connection(host="129.13.17.46",port="8086",user="",pass="")


result1 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo' AND label='walking'")
result1[[1]]$tags <- {}
result1[[1]]$tags$subject <- "pablo"
result1[[1]]$tags$label <- "walking"


result2 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo' AND label='standing'")
result2[[1]]$tags <- {}
result2[[1]]$tags$subject <- "pablo"
result2[[1]]$tags$label <- "standing"

result3 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo' AND label='sitting'")
result3[[1]]$tags <- {}
result3[[1]]$tags$subject <- "pablo"
result3[[1]]$tags$label <- "sitting"

result4 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian' AND label='walking'")
result4[[1]]$tags <- {}
result4[[1]]$tags$subject <- "fabian"
result4[[1]]$tags$label <- "walking"

result5 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian' AND label='standing'")
result5[[1]]$tags <- {}
result5[[1]]$tags$subject <- "fabian"
result5[[1]]$tags$label <- "standing"

result6 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian' AND label='sitting'")
result6[[1]]$tags <- {}
result6[[1]]$tags$subject <- "fabian"
result6[[1]]$tags$label <- "sitting"

result7 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo2' AND label='walking'")
result7[[1]]$tags <- {}
result7[[1]]$tags$subject <- "pablo2"
result7[[1]]$tags$label <- "walking"


result8 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo2' AND label='standing'")
result8[[1]]$tags <- {}
result8[[1]]$tags$subject <- "pablo2"
result8[[1]]$tags$label <- "standing"

result9 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo2' AND label='sitting'")
result9[[1]]$tags <- {}
result9[[1]]$tags$subject <- "pablo2"
result9[[1]]$tags$label <- "sitting"

result10 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian2' AND label='walking'")
result10[[1]]$tags <- {}
result10[[1]]$tags$subject <- "fabian2"
result10[[1]]$tags$label <- "walking"

result11 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian2' AND label='standing'")
result11[[1]]$tags <- {}
result11[[1]]$tags$subject <- "fabian2"
result11[[1]]$tags$label <- "standing"

result12 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian2' AND label='sitting'")
result12[[1]]$tags <- {}
result12[[1]]$tags$subject <- "fabian2"
result12[[1]]$tags$label <- "sitting"

result13 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo3' AND label='walking'")
result13[[1]]$tags <- {}
result13[[1]]$tags$subject <- "pablo3"
result13[[1]]$tags$label <- "walking"


result14 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo3' AND label='standing'")
result14[[1]]$tags <- {}
result14[[1]]$tags$subject <- "pablo3"
result14[[1]]$tags$label <- "standing"

result15 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='pablo3' AND label='sitting'")
result15[[1]]$tags <- {}
result15[[1]]$tags$subject <- "pablo3"
result15[[1]]$tags$label <- "sitting"

result16 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian3' AND label='walking'")
result16[[1]]$tags <- {}
result16[[1]]$tags$subject <- "fabian3"
result16[[1]]$tags$label <- "walking"

result17 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian3' AND label='standing'")
result17[[1]]$tags <- {}
result17[[1]]$tags$subject <- "fabian3"
result17[[1]]$tags$label <- "standing"

result17 <- influxdbr2::influx_query_xts(con, db="training", query = "SELECT alpha,beta,gamma,x,y,z FROM orientation WHERE subject='fabian3' AND label='sitting'")
result17[[1]]$tags <- {}
result17[[1]]$tags$subject <- "fabian3"
result17[[1]]$tags$label <- "sitting"

results <-list(result1[[1]], result2[[1]], result3[[1]], result4[[1]], result5[[1]], result6[[1]], result7[[1]], result8[[1]], result9[[1]], result10[[1]], result11[[1]], result12[[1]], result13[[1]], result14[[1]], result15[[1]], result16[[1]], result17[[1]])

data=foreach(i=seq(1,length(results)),.combine=rbind) %do%
{
  ts=results[[i]]
  r={}
  r$label=ts$tags$label
  r$subject=ts$tags$subject
  foreach(w=split.xts(ts$values,f="millis",k=1000),.combine = rbind) %do%
  {
    
    r$alpha=mean(w$alpha)
    r$beta=mean(w$beta)
    #r$gamma=mean(w$gamma)
    r$x=mean(w$x)
    r$y=mean(w$y)
    r$z=mean(w$z)
    d3=sqrt((w$alpha-r$alpha)^2+(w$beta-r$beta)^2+(w$gamma-mean(w$gamma))^2)
    r$mean=mean(d3)
    r$sd=sd(d3)
    r$max=max(d3)
    
    r$sdZ = sd(w$z)
    
    as.data.frame(r)
  }
}
data <- na.omit(data)

featurePlot(x = data[, 6:10], y = data$label, plot = "pairs", auto.key = list(columns = 3))


subjects<-levels(factor(data$subject))
data_subject <- vector(mode = "list", length = nlevels(data$subject))

for (s in seq(1,nlevels(data$subject)))  data_subject[[s]]<- which(data$subject!=subjects[s])
trc <- trainControl(index=data_subject)
model <- train(data[,-c(1,2)], data$label, method = "rpart1SE", trControl=trc)


saveXML(pmml(model$finalModel),"/tmp/rpartclassifier.pmml")


