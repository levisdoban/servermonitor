#import the access.log file
system.time({file <- read.table(file.choose())})

#name the columns and remove some blank columns
names(file) = c("ipaddr"," ", " ", "timestamp", "gmt", "target", "status", "time", " ", "verbose")
data = file[,-c(2,3,9)]
#bind and clean the time stamp column
data$timestamp = gsub("\\[", "",data$timestamp)
data$timestamp = strptime(data$timestamp,format='%d/%b/%Y:%H:%M:%S')
recoded2 = within(data, times <- as.factor(ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('06','07','08', '09'),"Morning", ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('10','11','12'),"Mid-Morning",ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('14','15','16','13'),"Afternoon", ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('17','18','19','20'),"Evening", "Night"))  ) )))
recoded2$time = as.numeric(recoded2$time)
recoded2$byhr = strftime(recoded2$timestamp, "%H", tz="EAT")

#Number of 
nof_ips = length(table(recoded2$ipaddr))
#Number of different IPS accessing the server
nof_ips

#Number of requests per IP ordered from the most to least
ips_reqs = as.data.frame(table(recoded2$ipaddr))
ips_reqs2 = ips_reqs[order(-ips_reqs[,2]),]
head(ips_reqs2)

#Average request time for different times of the day
iptime = aggregate(time ~ times, data = recoded2,  FUN = mean)

#Average request time for each hour
hrtime = aggregate(time ~ byhr, data = recoded2,  FUN = mean)

#Percentage of server access status by time of day
with(recoded2, prop.table(table(times, status)))

