library(doParallel)
library(foreach)

system.time({file <- read.table(file.choose())})

names(file) = c("ipaddr"," ", " ", "timestamp", "gmt", "target", "status", "time", " ", "verbose")
data = file[,-c(2,3,9)]


data$time = as.numeric(data$time)
nof_ips = length(table(data$ipaddr))

ips_reqs = as.data.frame(table(d_frame$ipaddr))
ips_reqs2 = ips_reqs[order(-ips_reqs[,2]),]

iptime = aggregate(duration ~ ipaddr, data = d_frame,  FUN = mean)

targetime = aggregate(duration ~ target, data = d_frame,  FUN = median)

#call the library
cl <- makeCluster(2)
registerDoParallel(cl)
getDoParRegistered()

foreach(i=1:3) %dopar% sqrt(i)

d <- data.frame(x=1:10, y=rnorm(10))
s <- foreach(d=iter(d, by='row'), .combine=rbind) %dopar% d
identical(s, d)

spliter(i)



stime <- system.time({
   r <- foreach(d=iter(file[1:30], by='row'), .combine=rbind) %dopar% {
     ind <- sample(100, 100, replace=TRUE)
     result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
     coefficients(result1)
   }
 })[3]
 stime


dats = spliter(i)

stime <- system.time({


   r <- foreach(xx =iter(5:7), .combine=rbind) %dopar% {
     dats = spliter(xx)
	dats #d_frame2 = rbind(d_frame2,dats)
   }


 })[3]
 stime

foreach(i=1:3) %dopar% sqrt(i)

stime <- system.time({
 r <- foreach(i = 5:15, .combine=rbind) %dopar% spliter(i)
})


stime <- system.time({
for (i in 5:15){
dats = spliter(i)
d_frame = rbind(d_frame,dats)
}
})







{
       spliter(r)
	#d_frame2 = rbind(d_frame2,dats)
  }

