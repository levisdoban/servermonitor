#Ensure you have everything you need 
setwd("C:\\Users\\user\\servermonitor")
EnsurePackage<-function(x)
{x = as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}

PrepareRR=function()
{
  EnsurePackage("tools")
  EnsurePackage("brew")
  EnsurePackage("stringr")
  EnsurePackage("xtable")
  EnsurePackage("ggplot2")
  EnsurePackage("plotrix")
}
PrepareRR()
# A function to generate png from plots

#import the access.log file
system.time({file <- read.table(file.choose())})

#name the columns and remove some blank columns
names(file) = c("ipaddr"," ", " ", "timestamp", "gmt", "target", "status", "time", " ", "verbose")
data = file[,-c(2,3,9)]
#bind and clean the time stamp column
data$timestamp = gsub("\\[", "",data$timestamp)
data$timestamp = strptime(data$timestamp,format='%d/%b/%Y:%H:%M:%S')
data = within(data, times <- as.factor(ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('06','07','08', '09'),"Morning", ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('10','11','12'),"Mid-Morning",ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('14','15','16','13'),"Afternoon", ifelse(strftime(data$timestamp, "%H", tz="EAT") %in% c('17','18','19','20'),"Evening", "Night"))  ) )))
data$time = as.numeric(data$time)
data$byhr = strftime(data$timestamp, "%H", tz="EAT")

#REporting
startdate = data$timestamp[1]
stdate = format(startdate, format="%B %d, %Y")
enddate = data$timestamp[nrow(data)-1]
edate = format(enddate, format="%B %d, %Y")
daysa = as.numeric(round(difftime(enddate,startdate,  units='days'),1))

preamble = paste("This report covers server access time between ", stdate, " and ",  edate, " approximately ", daysa, " days period.", sep="")


#Number of 
nof_ips = length(table(data$ipaddr))
access = nrow(data)
avacc = round(access/daysa, 0)
#Number of different IPS accessing the server
acce = paste("There were a total of ", access, " access requests from ", nof_ips, " different IP addresses which comest to approximately ",  avacc, " access per day.", sep="")
preamble2 = paste(preamble, acce, sep=" ") 

#Number of requests per IP ordered from the most to least
ips_reqs = as.data.frame(table(data$ipaddr))
ips_reqs2 = ips_reqs[order(-ips_reqs[,2]),]
head(ips_reqs2)

#Average request time for different times of the day
iptime = aggregate(time ~ times, data = data,  FUN = mean)

mean_access = ggplot(data=iptime, aes(x=times, y=time, fill=time)) +
    geom_bar(stat="identity") + xlab("Time of day") + ylab("Mean access time in Milliseconds") 
    #ggtitle("Average access time by time of day")

d3 = paste(substitute(mean_access), "png", sep=".")
png(file = d3, bg="transparent")
mean_access
dev.off()
timax = max(iptime$time)
timin = min(iptime$time)
badti = as.character(iptime[iptime$time == timax,1])
goodti = as.character(iptime[iptime$time == timin,1])
story = paste("On average, your servers are easily reachable in the ", goodti, " when the average access time is about ",  round(timin/100,1), " Seconds. Your servers are much busier in the ", badti, " with an access time averageing ",  round(timax/100,1), " seconds.", sep="")

#Average request time for each hour
hrtime = aggregate(time ~ byhr, data = data,  FUN = mean)

#mean_access_hr = ggplot(data=hrtime, aes(x=byhr, y=time, fill=byhr)) +
 #   geom_bar(stat="identity") + xlab("Hour of Day") + ylab("Mean access time in Milliseconds") 
    #ggtitle("Average access time by time of day")

mean_access_hr = ggplot(data=hrtime, aes(x=as.numeric(byhr), y=time)) + geom_line()+ xlab("Hour of Day") + ylab("Mean access time in Milliseconds")

d4 = paste(substitute(mean_access_hr), "png", sep=".")
png(file = d4, bg="transparent")
mean_access_hr
dev.off()
timax2 = max(hrtime$time)
timin2 = min(hrtime$time)
badti2 = as.character(hrtime[hrtime$time == timax2,1])
goodti2 = as.character(hrtime[hrtime$time == timin2,1])
story2 = paste("On a hour-by-hour breakdown, your servers are easily reachable around ", goodti2, "00HR when the average access time is about ",  round(timin2/100,1), " Seconds. Your servers are much busier around ", badti2, "00HRS when access time averages ",  round(timax2/100,1), " seconds.", sep="")


#Percentage of server access status by time of day
times_tb = with(data, table(times, status))



#Brewery starts here
#dir.create("Reports")
rep = paste(Sys.Date())
 report_name =  paste("rep",rep, ".tex", sep="_" )
   brew("monitor_report.brew", report_name)
   texi2dvi(report_name, pdf = TRUE)


