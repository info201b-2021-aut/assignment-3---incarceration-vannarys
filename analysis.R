# load data
library(dplyr)
<<<<<<< HEAD
library(tidyverse)
library(ggplot2)
library(lintr)
library(maps)
df <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

# SUMMARY STATISTICS ---------------------------------------------------------------------------------------------------

# race with highest prison admission rate on average

prison_admission_rate_table <- df %>%
  select(year, state, total_prison_adm_rate, aapi_prison_adm_rate, 
        black_prison_adm_rate, latinx_prison_adm_rate, native_prison_adm_rate, 
        white_prison_adm_rate, female_prison_adm_rate, male_prison_adm_rate)
  
mean_aapi_prison_adm_rate <- df %>%
  summarise(aapi_prison_adm_rate_avg = mean(aapi_prison_adm_rate, na.rm=TRUE)) %>%
  pull(aapi_prison_adm_rate_avg)

mean_black_prison_adm_rate <- df %>%
  summarise(black_prison_adm_rate_avg = mean(black_prison_adm_rate, na.rm=TRUE)) %>%
  pull(black_prison_adm_rate_avg)

mean_latinx_prison_adm_rate <- df %>%
  summarise(latinx_prison_adm_rate_avg = mean(latinx_prison_adm_rate, na.rm=TRUE)) %>%
  pull(latinx_prison_adm_rate_avg)

mean_native_prison_adm_rate <- df %>%
  summarise(native_prison_adm_rate_avg = mean(native_prison_adm_rate, na.rm=TRUE)) %>%
  pull(native_prison_adm_rate_avg)

mean_white_prison_adm_rate <- df %>%
  summarise(white_prison_adm_rate_avg = mean(white_prison_adm_rate, na.rm=TRUE)) %>%
  pull(white_prison_adm_rate_avg)

prison_adm_rate_race_max <- df %>%
  summarise(aapi_prison_adm_rate_avg = mean(aapi_prison_adm_rate, na.rm=TRUE),
            black_prison_adm_rate_avg = mean(black_prison_adm_rate, na.rm=TRUE), 
            latinx_prison_adm_rate_avg = mean(latinx_prison_adm_rate, na.rm=TRUE),
            native_prison_adm_rate_avg = mean(native_prison_adm_rate, na.rm=TRUE), 
            white_prison_adm_rate_avg = mean(white_prison_adm_rate, na.rm=TRUE)
  ) %>%
  gather(key = "race", value = "prison_adm_rate_race") %>%
  filter(prison_adm_rate_race == max(prison_adm_rate_race)) %>%
  pull(prison_adm_rate_race)

highest_prison_adm_rate_race <- max(prison_adm_rate_race_max)
  
# race with lowest admission rate on average

prison_adm_rate_race_min <- df %>%
  summarise(aapi_prison_adm_rate_avg = mean(aapi_prison_adm_rate, na.rm=TRUE),
            black_prison_adm_rate_avg = mean(black_prison_adm_rate, na.rm=TRUE), 
            latinx_prison_adm_rate_avg = mean(latinx_prison_adm_rate, na.rm=TRUE),
            native_prison_adm_rate_avg = mean(native_prison_adm_rate, na.rm=TRUE), 
            white_prison_adm_rate_avg = mean(white_prison_adm_rate, na.rm=TRUE)
  ) %>%
  gather(key = "race", value = "prison_adm_rate_race") %>%
  filter(prison_adm_rate_race == min(prison_adm_rate_race)) %>%
  pull(prison_adm_rate_race)

lowest_prison_adm_rate_race <- min(prison_adm_rate_race_min)

# sex with highest admission rate on average

mean_female_prison_adm_rate <- df %>%
  summarise(female_prison_adm_rate_avg = mean(female_prison_adm_rate, na.rm=TRUE)) %>%
  pull(female_prison_adm_rate_avg)

mean_male_prison_adm_rate <- df %>%
  summarise(male_prison_adm_rate_avg = mean(male_prison_adm_rate, na.rm=TRUE)) %>%
  pull(male_prison_adm_rate_avg)

prison_adm_rate_sex_max<- df %>%
  summarise(female_prison_adm_rate_avg = mean(female_prison_adm_rate, na.rm=TRUE), 
            male_prison_adm_rate_avg = mean(male_prison_adm_rate, na.rm=TRUE)
  ) %>%
  gather(key = "sex", value = "prison_adm_rate_sex") %>%
 filter(prison_adm_rate_sex == max(prison_adm_rate_sex)) %>%
 pull(prison_adm_rate_sex)

highest_prison_adm_rate_sex <- max(prison_adm_rate_sex_max)

# sex with lowest admission rate on average

prison_adm_rate_sex_min <- df %>%
  summarise(female_prison_adm_rate_avg = mean(female_prison_adm_rate, na.rm=TRUE), 
            male_prison_adm_rate_avg = mean(male_prison_adm_rate, na.rm=TRUE)
  ) %>%
  gather(key = "sex", value = "prison_adm_rate_sex") %>%
filter(prison_adm_rate_sex == min(prison_adm_rate_sex)) %>%
  pull(prison_adm_rate_sex)

lowest_prison_adm_rate_sex <- min(prison_adm_rate_sex_min)

# difference between race with highest and lowest admission rate

adm_rate_race_difference <- highest_prison_adm_rate_race - lowest_prison_adm_rate_race

# difference between sex with highest and lowest admission rate

adm_rate_sex_difference <- highest_prison_adm_rate_sex - lowest_prison_adm_rate_sex

# TIME TREND CHART -----------------------------------------------------------------------------------------------------

# admission rate for each race over time

adm_rate_race_time_chart <- prison_admission_rate_table %>%
  group_by(year) %>%
  summarise(aapi_prison_adm_rate_avg = mean(aapi_prison_adm_rate, na.rm=TRUE),
            black_prison_adm_rate_avg = mean(black_prison_adm_rate, na.rm=TRUE), 
            latinx_prison_adm_rate_avg = mean(latinx_prison_adm_rate, na.rm=TRUE),
            native_prison_adm_rate_avg = mean(native_prison_adm_rate, na.rm=TRUE), 
            white_prison_adm_rate_avg = mean(white_prison_adm_rate, na.rm=TRUE)
  ) %>%
  drop_na(aapi_prison_adm_rate_avg, black_prison_adm_rate_avg, latinx_prison_adm_rate_avg,
          native_prison_adm_rate_avg, white_prison_adm_rate_avg) %>%
  gather(key = "race", value = "adm_rate_avg", -year)

time_chart <- adm_rate_race_time_chart %>%
  ggplot(aes(x = year, y = adm_rate_avg, color = race)) + 
  geom_line() +
  labs(title = "Average Prison Admission Rate by Race Over Time",
       x = "Year", 
       y = "Average Prison Admission Rate", 
       color = "Race") +
scale_color_manual(labels = c("AAPI", "Black", "Latinx", "Native", "White"),
                   values = c("red", "blue", "green", "orange", "yellow4"))
    
# VARIABLE COMPARISON CHART --------------------------------------------------------------------------------------------

# ratio of black vs white prison average admission rate in 2016

black_vs_white_adm_rate <- prison_admission_rate_table %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  summarise(black_prison_adm_rate_avg = mean(black_prison_adm_rate, na.rm=TRUE), 
            white_prison_adm_rate_avg = mean(white_prison_adm_rate, na.rm=TRUE)
            ) %>%
  drop_na(black_prison_adm_rate_avg, white_prison_adm_rate_avg) 

scatterplot_chart <- black_vs_white_adm_rate %>%
 ggplot(aes(x = black_prison_adm_rate_avg, y = white_prison_adm_rate_avg)) + 
 geom_point() +
 labs(title = "Ratio of Black vs White Average Prison Admission Rate in 2016", 
  x = "Black Average Prison Admission Rate",
  y = "White Average Prison Admission Rate")
 
# MAP ------------------------------------------------------------------------------------------------------------------

# average black prison admission rate per state in 2016 

black_adm_rate_state <- df %>%
  filter(year == 2016) %>% 
  group_by(state) %>%
  summarise(black_prison_adm_rate_avg = mean(black_prison_adm_rate, na.rm=TRUE)) %>%
  drop_na(black_prison_adm_rate_avg) 

black_adm_rate_state$state <- state.name[match(black_adm_rate_state$state, state.abb)] 

black_adm_rate_state <- black_adm_rate_state %>%
  mutate(state = tolower(state))
  
state_shape <- map_data("state") %>% 
  rename(state = region) %>%
  left_join(black_adm_rate_state, by="state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
       )

map_chart <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_prison_adm_rate_avg),
    color = "white",
    size = .1      
      ) +
  coord_map() + 
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs( title = "Average Black Prison Admission Rate Per State in 2016",
        fill = "Average Black Prison Admission Rate") +
  blank_theme 
=======
read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

# summary statistics

>>>>>>> 2d9c57e3d580729e025283fb55ae09f977698fca
