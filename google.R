install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(dplyr)

read.csv("~/Downloads/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv") -> activity
read.csv("~/Downloads/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv") -> calories
read.csv("~/Downloads/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv") -> dailySteps
read.csv("~/Downloads/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv") -> hourlySteps
read.csv("~/Downloads/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv") -> sleep
read.csv("~/Downloads/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv") -> weight
read.csv("~/Downloads/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv") -> intensities

#look at summary of data
head(activity)
head(calories)
head(dailySteps)
head(hourlySteps)
head(sleep)
head(weight)
head(intensities)

#separating the hour and the date
calories$ActivityHour = as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$Day = format(calories$ActivityHour, format = "%m/%d/%Y")
calories$Time = format(as.POSIXct(calories$ActivityHour),format = "%H:%M:%S")

hourlySteps$ActivityHour = as.POSIXct(hourlySteps$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourlySteps$Day = format(hourlySteps$ActivityHour, format = "%m/%d/%Y")
hourlySteps$Time = format(as.POSIXct(hourlySteps$ActivityHour),format = "%H:%M:%S")

sleep$SleepDay = as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$Day = format(sleep$SleepDay, format = "%m/%d/%Y")
sleep$Time = format(as.POSIXct(sleep$SleepDay), format("%H:%M:%S"))

intensities$ActivityHour = as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$Day = format(intensities$ActivityHour, format="%m/%d/%Y")
intensities$Time = format(as.POSIXct(intensities$ActivityHour), format = "%H:%M:%S")

weight$Date = as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$Day = format(weight$Date, format = "%m/%d/%Y")
weight$Time = format(as.POSIXct(weight$Date), format("%H:%M:%S"))

activity$ActivityDate = as.POSIXct(activity$ActivityDate, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
activity$Day = format(activity$ActivityDate, format="%m/%d/%Y")
activity$Time = format(as.POSIXct(activity$ActivityDate), format("%H:%M:%S"))

#summarize data
n_distinct(activity$Id) # 33
n_distinct(calories$Id) # 33
n_distinct(dailySteps$Id) # 33
n_distinct(hourlySteps$Id) # 33
n_distinct(sleep$Id) # 24
n_distinct(weight$Id) # 8
n_distinct(intensities$Id) # 33

#Look at sleep data first
View(sleep)
sleep %>%
  select(TotalMinutesAsleep) %>%
  summary()

sleep_activities <- merge(sleep, activity, by=c("Id", "Day"))
View(sleep_activities)

head(activity)
activity$ActivityDate = as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())

install.packages("ggplot2")
library(ggplot2)

ggplot(data = sleep_activities) + 
  geom_point(mapping = aes(x = TotalMinutesAsleep, y = TotalDistance)) # no correlation

ggplot(data = sleep_activities) + 
  geom_point(mapping = aes(x = TotalMinutesAsleep, y = VeryActiveMinutes)) # no correlation 

ggplot(data = sleep_activities) + 
  geom_point(mapping = aes(x = TotalMinutesAsleep, y = FairlyActiveMinutes)) # no correlation

ggplot(data = sleep_activities) + 
  geom_point(mapping = aes(x = TotalMinutesAsleep, y = SedentaryMinutes, color=Id)) # negative correlation

ggplot(data = sleep) + 
  geom_point(mapping = aes(x = SleepDay, y = TotalMinutesAsleep)) + 
  facet_wrap(~Id)
# everyone seems to be getting almost cosistent sleep

sleep_activities$percentSlept = sleep$TotalMinutesAsleep / sleep$TotalTimeInBed
head(sleep)
ggplot(data = sleep) + 
  geom_point(mapping = aes(x = SleepDay, y = percentSlept)) + 
  facet_wrap(~Id)

ggplot(data = sleep_activities) + 
  geom_smooth(mapping = aes(x = percentSlept, y = VeryActiveMinutes)) +
  geom_point(mapping = aes(x = percentSlept, y = VeryActiveMinutes)) #correlation

ggplot(data = sleep) + 
  geom_bar(mapping=aes(x = percentSlept))


# weight
ggplot(data = weight) + 
  geom_point(mapping = aes(y = WeightPounds, x = Day)) + 
  facet_wrap(~Id)

weight_activities = merge(weight, activity, by=c("Id", "Day"))
head(weight_activities)
ggplot(data = weight_activities) + 
  geom_point(mapping = aes(x = BMI, y = TotalSteps, color = Id))

# activities
activity$percentLightDistance = activity$LightActiveDistance / activity$TotalDistance
ggplot(data = activity) +
  geom_point(mapping = aes(x = LightlyActiveMinutes, y = TotalSteps, color = percentLightDistance))

# calories
ggplot(data = calories) + 
  geom_point(mapping = aes(x = Time, y = Calories))


# step total
ggplot(data = hourlySteps) + 
  geom_point(mapping= aes(x = Time, y = StepTotal))

# intensity
intensities %>% group_by(Time) %>% drop_na %>% summarize(mean_intensity = mean(TotalIntensity))
max(intensities$TotalIntensity)
summary(intensities, TotalIntensity)
ggplot(data = newdf,  aes(x = Time, y = all_intensity)) + 
  geom_histogram(stat = "identity")

dailyCalories <- calories %>% group_by(Id, Day) %>% drop_na %>% summarize(sum_calories = sum(Calories))
n_distinct(dailyCalories$Id)                                                         

# histogram of # of people who burn x amount of calories 
averageCalories <- dailyCalories %>% group_by(Id) %>% drop_na %>% summarize(mean_calorie = mean(sum_calories))
View(averageCalories)
head(averageCalories)
ggplot(data = averageCalories) +
  geom_histogram(mapping = aes(x = mean_calorie, fill = Id), bins = 5)

sum_stats <- activity %>% group_by(Id) %>% drop_na %>% summarize(sum_steps = sum(TotalSteps), sum_distance = sum(TotalDistance), sum_veryActive = sum(VeryActiveMinutes), sum_fairlyActive = sum(FairlyActiveMinutes), sum_lightActive= sum(LightlyActiveMinutes), totalActiveMinutes = sum_veryActive + sum_fairlyActive + sum_lightActive, percentVeryActive = sum_veryActive / totalActiveMinutes, percentFairlyActive = sum_fairlyActive / totalActiveMinutes, percentLightActive = sum_lightActive / totalActiveMinutes, average_dailySteps = mean(TotalSteps))
sum_stats <- sum_stats %>% mutate(levels = case_when (average_dailySteps <= 5000 ~ "Light", average_dailySteps > 5000 & average_dailySteps <= 9000 ~ "Moderate", average_dailySteps > 9000 ~ "High"))

ggplot(data = sum_stats) +
  geom_bar(mapping = aes(x = sum_steps, fill = levels))




## REAL CODE

#checking intensity levels at each hour
intensities_mod <- intensities %>% mutate(IntenseLevels = case_when (TotalIntensity <= 20 ~ "Light", TotalIntensity > 20 & TotalIntensity <= 100 ~ "Medium", TotalIntensity > 100 ~ "High"))

ggplot(data = intensities_mod) +
  geom_bar(mapping = aes(x = Time, fill = IntenseLevels))


## checking step level by the hour
head(hourlySteps)
hourlySteps_mod <- hourlySteps %>% mutate (Levels = case_when (StepTotal <= 400 ~ "Light", StepTotal > 400 & StepTotal <= 800 ~ "Moderate", StepTotal > 800 ~ "High"))

ggplot(data = hourlySteps_mod) +
  geom_bar(mapping = aes(x = Time, fill = Levels, colors = "Blues"))


## Userbase by average daily steps and their levels.






