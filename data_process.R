# Load packages
if(!require("pacman")) { require("pacman") }
pacman::p_load("magrittr", "dplyr", "readr", "lubridate", "ggplot2")


# Load US covid data
US_covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Filter out South Dakota
# Add variable for daily cases
# Add variable for daily growth rate: [ln(cumulative_cases) - ln(previous_day_cumulative_cases)] * 100
SD_covid_data <- US_covid_data %>%
    filter(state == "South Dakota") %>%
    select(date, cases) %>%
    rename(cumulative_cases = cases) %>%
    mutate(daily_cases = case_when(!is.na(lag(cumulative_cases)) ~ cumulative_cases - lag(cumulative_cases), TRUE ~ cumulative_cases),
           daily_growth = (log(cumulative_cases) - log(lag(cumulative_cases))) * 100)



# Period: 23 Mar 2020 - 17 May 2020 (8 weeks)
# Simulate counterfactual daily growth rates with masks mandated on 23rd Mar 2020
# As per Lyu & Wehby (https://www.healthaffairs.org/doi/10.1377/hlthaff.2020.00818)
# day 1-5: 0.9 pp
# day 6-10: 1.1 pp
# day 11-15: 1.4 pp
# day 16-20: 1.7 pp
# day 21+: 2 pp
SD_covid_data_period <- SD_covid_data %>%
    filter(date >= "2020-03-23", date <= "2020-05-17") %>%
    mutate(
      mask_daily_growth = case_when(
        date <= min(date) %m+% days(5) ~ daily_growth - 0.9,
        date <= min(date) %m+% days(10) ~ daily_growth - 1.1,
        date <= min(date) %m+% days(15) ~ daily_growth - 1.4,
        date <= min(date) %m+% days(20) ~ daily_growth - 1.7,
        TRUE ~ daily_growth - 2,
      ),
      
      # Change instances of negative growth to 0
      mask_daily_growth = case_when(
        mask_daily_growth < 0 ~ 0,
        TRUE ~ mask_daily_growth
      )
    )


# Calculate counterfactual cumulative cases with masks mandated on 23rd Mar 2020
mask_cumulative_cases <- SD_covid_data_period$cumulative_cases[1]

for (i in 2:nrow(SD_covid_data_period)) {
  mask_cumulative_cases <- c(mask_cumulative_cases, mask_cumulative_cases[i-1] * exp(SD_covid_data_period$mask_daily_growth[i] / 100))
}

SD_covid_data_period <- SD_covid_data_period %>%
    mutate(mask_cumulative_cases = mask_cumulative_cases %>% round(0))



# Plot comparison in cumulative cases between masks (counterfactual) and no masks (real) scenario
last_point <- SD_covid_data_period %>%
    filter(date == max(date)) %>%
    select(date, cumulative_cases, mask_cumulative_cases)

SD_covid_data_period %>% ggplot(aes(x = date, y = `cumulative cases`)) +
    geom_line(aes(y = cumulative_cases, color = "real data (no masks)")) +
    geom_line(aes(y = mask_cumulative_cases, color = "simulated data (masks)")) +
    geom_text(data = last_point, aes(y = cumulative_cases, label = cumulative_cases, hjust = -0.2)) +
    geom_text(data = last_point, aes(y = mask_cumulative_cases, label = mask_cumulative_cases, hjust = -0.2)) +
    scale_x_date(date_breaks = "1 week", date_labels = "%m/%d", limits = c(NA_Date_, max(SD_covid_data_period$date) + 5)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    labs(title = "South Dakota Covid data",
         subtitle = "Real vs counterfactual (no masks vs mask mandate)",
         color = NULL)
