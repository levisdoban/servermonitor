
system.time({file <- read.table(file.choose())})

names(file) = c("ipaddr"," ", " ", "timestamp", "gmt", "target", "status", "time", " ", "verbose")
data = file[,-c(2,3,9)]


data$time = as.numeric(data$time)
nof_ips = length(table(data$ipaddr))

ips_reqs = as.data.frame(table(d_frame$ipaddr))
ips_reqs2 = ips_reqs[order(-ips_reqs[,2]),]

iptime = aggregate(duration ~ ipaddr, data = d_frame,  FUN = mean)

targetime = aggregate(duration ~ target, data = d_frame,  FUN = median)

