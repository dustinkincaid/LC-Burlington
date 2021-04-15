# Download USGS data for Lake Champlain in Burlington & plot annual temperature time series

# Load packages
library("tidyverse")
library("lubridate")
library("dataRetrieval")
library("RColorBrewer")

# Helpful example for downloading USGS data using dataRetrieval package: https://owi.usgs.gov/R/dataRetrieval.html#11 and 
#                                                                        https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html

# Provide information for data retrieval
siteNo <- "04294500"
pCodes <- c("00010", "62614", "00095")
start.date <- "2014-10-01"
end.date <- today()


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

# Convert all dates to same year to be able to plot them all together with nice date formats on x-axis
same_year <- function(x) {
  year(x) <- 2000
  x
}
  
# Plot - all years plotted
lc %>% 
  # Calculate daily temp means
  mutate(date = date(timestamp)) %>% 
  group_by(date) %>% 
  summarize(temp_dailyMean = mean(Wtemp_Inst, na.rm = T)) %>% 
  # Convert temp from deg. C to F
  mutate(temp_dailyMean_F = temp_dailyMean/5*9+32) %>% 
  # Add year and month columns
  mutate(year = year(date),
         month = month(date)) %>% 
  # Select months to keep
  filter(month %in% c(4, 5, 6, 7, 8, 9, 10)) %>%
  # Plot
  ggplot(aes(x = same_year(date), y = temp_dailyMean_F, color = factor(year))) +
  geom_line() +
  scale_color_brewer(name = "Year",
                     palette = "Accent") +
  # scale_color_discrete()
  ylab(expression(Water~temp.~at~88~ft.~(degree*F))) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.1, 0.75))

ggsave("plots/temps.png", height = 5, width = 5, units = "in", dpi = 150)


# Plot - min, max, median & last 2 years
# test <- lc %>% 
#   # Calculate daily temp means
#   mutate(date = date(timestamp)) %>% 
#   group_by(date) %>% 
#   summarize(temp_dailyMean = mean(Wtemp_Inst, na.rm = T)) %>% 
#   # Convert temp from deg. C to F
#   mutate(temp_dailyMean_F = temp_dailyMean/5*9+32) %>% 
#   # Add year and month columns
#   mutate(year = year(date),
#          month = month(date),
#          mday = mday(date),
#          doy = yday(date)) %>% 
#   select(-c(date, temp_dailyMean)) %>% 
#   pivot_wider(names_from = year, values_from = temp_dailyMean_F) %>% 
#   arrange(month, doy) %>% 
#   rowwise() %>% 
#   mutate(min = min(c_across(`2014`:ncol(.)), na.rm = T),
#          max = max(c_across(`2014`:ncol(.)), na.rm = T),
#          med = median(c_across(`2014`:ncol(.)), na.rm = T)) %>% 
#   mutate(year = 2000,
#          date = ymd(paste(year, month, mday))) %>%
#   # select(-year) %>% 
#   # select(date, everything()) %>% 
#   # pivot_longer(cols = `2014`:ncol(.), names_to = "year", values_to = "temp_dailyMean_F") %>% 
#   # # Select time series to plot
#   # filter(year %in% c(`2020`, `2021`, "min", "max", "med"))
#   # Select months to keep
#   filter(month %in% c(3, 4, 5, 6, 7, 8, 9, 10, 11)) %>%
#   # Plot
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = min), linetype = "dashed")
#   geom_line() +
#   scale_color_brewer(name = "Year",
#                      palette = "Accent") +
#   # scale_color_discrete()
#   ylab(expression(Water~temp.~at~88~ft.~(degree*F))) +
#   theme_classic() +
#   theme(axis.title.x = element_blank(),
#         legend.position = c(0.1, 0.75))
