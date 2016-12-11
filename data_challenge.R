###
### Data challenge from an unnamed startup. 
### They are a cery cool company and I really 
### enjoyed this challenge. 
### 
### Dataset had 4.5M rows in JSON-like format. 


library(RJSONIO)

###
### Load
###
g_json <- fromJSON('pings.txt')

### 
### Convert to dataframe
### 
gdf <- do.call("rbind", lapply(g_json, as.data.frame))

### This keeps crashing! 



library(jsonlite)
library(doBy)
library(plyr)
library(ggplot2)

###
### Load
###
g_json <- stream_in(file('pings.txt', "r"))

head(g_json) # Success!! 

gdf <- g_json # Copy so I don't have to load the original again 
str(gdf)

### 
### Clean up formatting, timestamps 
### 
#gdf$attributed_to <- abs(as.numeric(gsub("(u|f):", "", gdf$attributed_to)))
gdf$date_time <- strptime(gdf$date_time, format="%Y-%m-%d %H:%M")

###
### Find the cohorts of Feb 4 and 10
###
first_users_04 <- unique(gdf[gdf$used_first_time_today == TRUE &
                               as.Date(gdf$date_time) == '2016-02-04'
                             ,"attributed_to"])
first_users_10 <- unique(gdf[gdf$used_first_time_today == TRUE & 
                               as.Date(gdf$date_time) == '2016-02-10',
                             "attributed_to"])
activity_04 <- gdf[gdf$attributed_to %in% first_users_04,]
activity_10 <- gdf[gdf$attributed_to %in% first_users_10,]
activity_04$day <- as.Date(activity_04$date_time)
activity_10$day <- as.Date(activity_10$date_time)


# Check that we extracted properly
length(first_users_04) # 2795
length(unique(activity_04$attributed_to)) # 2795

# Find count of unique userids (attributed_to) by day
# for each starting cohort
cohort_04 <- summaryBy(attributed_to ~ day, 
                       data = activity_04, 
                       FUN = function(x) length(unique(x)))
cohort_10 <- summaryBy(attributed_to ~ day,
                       data = activity_10,
                       FUN = function(x) length(unique(x)))

# Clean up names of summary dataframes
names(cohort_04) <- c("day","count")
names(cohort_10) <- c("day","count")

# Normalize to initial count to get retention
cohort_04$retention <- cohort_04$count / cohort_04$count[1] * 100
cohort_10$retention <- cohort_10$count / cohort_10$count[1] * 100

# Count days since cohort start
cohort_04$num_days <- as.numeric(cohort_04$day - cohort_04[1,"day"])
cohort_10$num_days <- as.numeric(cohort_10$day - cohort_10[1,"day"])

# Plot as retention since start
# Not bad but a bit ugly
plot(cohort_04$num_days, cohort_04$retention, type='l', col='blue',
     xlab="Days since Start",
     ylab="Retention [%]",
     main="Retention Curves")
lines(cohort_10$num_days, cohort_10$retention, type='l', col='red')
legend("topright",c("Feb 04","Feb 10"),lty=c(1,1),col=c("blue","red"))
dev.copy(png,'Fig0_Retention_Curve_StdPlot.png')
dev.off()

# Quick reshape for ggplot2
cohort_04$cohort <- 'Feb_04'
cohort_10$cohort <- 'Feb_10'
cohorts <- rbind(cohort_04, cohort_10)

fig1 <- ggplot(data=cohorts,
               aes(x=num_days, y=retention, color=factor(cohort))) +
  #geom_line(colour="blue", size=1) +
  geom_point(size=3) +
  ylab("Retention [%]") +
  xlab("Days since Start") + 
  ggtitle("Retention Curves") +
  theme(plot.title = element_text(size=20, face="bold"))
fig1
dev.copy(png,'Fig1_Retention_Curve_ggplot_Num_Days.png')
dev.off()

fig2 <- ggplot(data=cohorts,
               aes(x=day, y=retention, color=factor(cohort))) +
  #geom_line(colour="blue", size=1) +
  geom_point(size=3) +
  ylab("Retention [%]") +
  xlab("Date") + 
  ggtitle("Retention Curves") +
  theme(plot.title = element_text(size=20, face="bold"))
fig2
dev.copy(png,'Fig2_Retention_Curve_ggplot_Abs_Date.png')
dev.off()



# Focus on 04 cohort and break out by source 
# Exclude source = NA
# Find count of unique userids (attributed_to) by day
cohort_04_channel <- summaryBy(attributed_to ~ day + first_utm_source, 
                               data = activity_04[!is.na(activity_04$first_utm_source),],
                               FUN = function(x) length(unique(x)))


# Clean up names
names(cohort_04_channel) <- c("day","source","count")

# Preview
head(cohort_04_channel,20)

# Remove source = NA series 
cohort_04_channel <- cohort_04_channel[!is.na(cohort_04_channel$source),]


# Normalize each source by initial count
# to calculate retention

i_count <- cohort_04_channel[cohort_04_channel$day == '2016-02-04', 
                             c("source","count")]

cohort_04_channel$retention <- cohort_04_channel$count
source_list <- i_count$source

for (i in 1:length(source_list)) {  
  source_initial <- i_count[i_count$source == source_list[i],"count"]
  
  print(paste("Processing ",source_list[i],": ",source_initial," initial users",sep=""))
  
  # Calculate retention by normalizing to first day
  cohort_04_channel[cohort_04_channel$source == source_list[i],"retention"] <- 
    cohort_04_channel[cohort_04_channel$source == source_list[i],"count"] /
    source_initial * 100
}

# Start incrementing from first day
cohort_04_channel$num_days <- as.numeric(cohort_04_channel$day - cohort_04_channel[1,"day"])

fig3 <- ggplot(data=cohort_04_channel,
               aes(x=day, y=retention, color=factor(source))) +
  geom_line() +
  #geom_point(size=3) +
  ylab("Retention [%]") +
  xlab("Date") + 
  ggtitle("Retention Curves") +
  theme(plot.title = element_text(size=20, face="bold"))
fig3
dev.copy(png,'Fig3_Retention_Curve_Channel_ggplot_Abs_Date.png')
dev.off()

# Why is Brand1 increasing in retention above starting point?? 

# Extract Brand1 users out of Feb 04 initial cohort
brand1_04 <- gdf[gdf$used_first_time_today == TRUE & 
                   as.Date(gdf$date_time) == '2016-02-04' & 
                   gdf$first_utm_source == 'brand1',
                 "attributed_to"]
brand1_04 <- unique(brand1_04[!is.na(brand1_04)])
length(brand1_04) # Finds 53 unique users throughout the month

cohort_04_channel[cohort_04_channel$source == 'brand1' 
                  & cohort_04_channel$num_days == 0,]$count # Finds 53 unique users 

# Explore Feb 16 users who originated on Feb 04 from 'brand1'
brand1_04_16 <- unique(gdf[as.Date(gdf$date_time) == '2016-02-16' & 
                             gdf$first_utm_source == 'brand1' &
                             gdf$attributed_to %in% brand1_04,
                           "attributed_to"])
length(brand1_04_16) # Finds 31 unique users 

raw_04_16 <- unique(activity_04[activity_04$day == '2016-02-16' & 
                                  activity_04$first_utm_source == 'brand1',]$attributed_to)

# These users are the discrepancy
raw_04_16[!(raw_04_16 %in% brand1_04_16)]

# Follow one specific user
debug_04 <- subset(gdf, attributed_to == raw_04_16[2] & used_first_time_today == TRUE)

# This person does not have a first_utm_source on their first 
# day of use! They are getting pulled into the cohort at a later
# date when the field does populate correctly. 

first_users_04_channel <- unique(gdf[gdf$used_first_time_today == TRUE &
                                       as.Date(gdf$date_time) == '2016-02-04' & 
                                       !is.na(gdf$first_utm_source), 
                                     "attributed_to"])
length(first_users_04_channel) # 1013

activity_04_ch <- gdf[gdf$attributed_to %in% first_users_04_channel,]
activity_04_ch$day <- as.Date(activity_04_ch$date_time)

# Find count of unique userids (attributed_to) by day
# for each starting cohort
cohort_04_ch <- summaryBy(attributed_to ~ day + first_utm_source, 
                          data = activity_04_ch, 
                          FUN = function(x) length(unique(x)))

names(cohort_04_ch) <- c("day","source","count")

# Preview
head(cohort_04_ch,20)

# Remove source = NA series 
cohort_04_ch <- cohort_04_ch[!is.na(cohort_04_ch$source),]


# Normalize each source by initial count
# to calculate retention

i_count <- cohort_04_ch[cohort_04_ch$day == '2016-02-04', 
                        c("source","count")]

cohort_04_ch$retention <- cohort_04_ch$count
source_list <- i_count$source

for (i in 1:length(source_list)) {  
  source_initial <- i_count[i_count$source == source_list[i],"count"]
  
  print(paste("Processing ",source_list[i],": ",source_initial," initial users",sep=""))
  
  # Calculate retention by normalizing to first day
  cohort_04_ch[cohort_04_ch$source == source_list[i],"retention"] <- 
    cohort_04_ch[cohort_04_ch$source == source_list[i],"count"] /
    source_initial * 100
}


# Start incrementing from first day
cohort_04_ch$num_days <- as.numeric(cohort_04_ch$day - cohort_04_ch[1,"day"])

fig4 <- ggplot(data=cohort_04_ch,
               aes(x=day, y=retention, color=factor(source))) +
  geom_line() +
  #geom_point(size=3) +
  ylab("Retention [%]") +
  xlab("Date") + 
  ggtitle("Retention Curves") +
  theme(plot.title = element_text(size=20, face="bold"))
fig4
dev.copy(png,'Fig4_Retention_Curve_Channel_ggplot_Abs_Date_Cleaned.png')
dev.off()

low_sources <- unique(cohort_04_ch[cohort_04_ch$num_days == 0 &
                                     cohort_04_ch$count == 1,]$source)
cohort_04_ch_cl <- cohort_04_ch[!(cohort_04_ch$source %in% low_sources),]

fig5 <- ggplot(data=cohort_04_ch_cl,
               aes(x=day, y=retention, color=factor(source))) +
  geom_line() +
  #geom_point(size=3) +
  ylab("Retention [%]") +
  xlab("Date") + 
  ggtitle("Retention Curves") +
  theme(plot.title = element_text(size=20, face="bold"))
fig5
dev.copy(png,'Fig5_Retention_Curve_Channel_ggplot_Abs_Date_Cleaned.png')
dev.off()
