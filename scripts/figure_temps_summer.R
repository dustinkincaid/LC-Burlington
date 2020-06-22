
# Load packages
library("tidyverse")
library("lubridate")
library("dataRetrieval")

# Helpful example for downloading USGS data using dataRetrieval package: https://owi.usgs.gov/R/dataRetrieval.html#11 and 
#                                                                        https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html

# Provide information for data retrieval
siteNo <- "04294500"
pCodes <- c("00010", "62614", "00095")
start.date <- "2014-10-01"
end.date <- "2020-06-22"

# Retrieve data as specified above
lc <- readNWISuv(siteNumbers = siteNo,
                 parameterCd = pCodes,
                 startDate = start.date,
                 endDate = end.date)

# Tidy data
lc <- 
  # Function below is built in and should fix most/all of the column names
  renameNWISColumns(lc) %>% 
  # Above function didn't fix them all
  rename(Welev = "X_62614_Inst", Welev_cd = "X_62614_Inst_cd", timestamp = "dateTime") %>% 
  # Convert time from UTC to EST
  # The data are downloaded in UTC
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) %>% 
  # Convert the times to EST
  mutate(timestamp = force_tz(timestamp, tzone = "Etc/GMT+5")) %>% 
  # Drop tz column
  select(-tz_cd)

# Graph daily temp means for April-Aug for each year
test2 <- lc %>% 
  # Calculate daily temp means
  mutate(date = date(timestamp)) %>% 
  group_by(date) %>% 
  summarize(temp_dailyMean = mean(Wtemp_Inst, na.rm = T)) %>% 
  mutate(date = ymd(date))
  # Add a year column
  mutate(year = year(date)) %>% 
  # Add a day of the year column
  mutate(yday = yday(date)) %>% 
  # Select months to keep
  select(month(year) %in% c(4, 5, 7, 8)) %>% 
  # Plot
  ggplot(aes(x = yday, y = temp_mean, group = year)) +
  geom_line() +
  theme_classic()
  
  

