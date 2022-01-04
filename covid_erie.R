library("RSocrata")  # download data from the web
library("tidyverse") # manipulate data/create plots
library("tidyquant") # plotted moving averages
library("zoo")       # calculate moving averages
library("ggpubr")    # combining plots
library("scales")    # for percentages

setwd("C:/Users/Joanna/Dropbox/Repositories/COVID_Erie")

data <- read.socrata("https://health.data.ny.gov/Health/New-York-State-Statewide-COVID-19-Testing/xdss-u53e")

data <- data %>%
  filter(county == "Erie") 

# Create averages
data <- data %>%
  mutate(pct_pos      = round(new_positives/total_number_of_tests, 2),
         per100       = round((new_positives/918702)*100000)) # population of Erie county 918702

data$pct_pos[is.na(data$pct_pos)] <- 0

# Create moving averages
data <- data %>%
  mutate(pct_pos_07da = round(rollmean(pct_pos, k = 7, fill = NA, align = "right"), 2),
         per100_07da  = round(rollmean(per100, k = 7, fill = NA, align = "right"))) 


# Cases per 100k (7 day average)
p1 <- data %>%
  ggplot(aes(x = test_date, y = per100_07da, label=per100_07da)) +
  geom_line() + 
  geom_text(data = data %>% filter(test_date == max(test_date)), 
            aes(x = test_date, y = per100_07da, label=per100_07da), 
            vjust = -.8,
            fontface = "bold") +
  theme_minimal() +
  theme(legend.position      = "none",
        plot.title.position  = "plot",
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  coord_cartesian(ylim = c(0, 300)) +
  labs( x        = " ", 
        y        = "Cases per 100k \n(7 day average)", 
        fill     = " ",
        title    = "COVID-19 data for Erie County, NY",
        subtitle = paste("Data last updated:", as.character(max(data$test_date))))
p1

# Tests % Positive (7 day average)
p2 <- data %>%
  ggplot(aes(x = test_date, y = pct_pos, label=pct_pos_07da, color = "#E74C3C")) +
  geom_ma(ma_fun = SMA, n = 7, linetype="solid") + 
  geom_text(data = data %>% filter(test_date == max(test_date)), 
            aes(x = test_date, y = pct_pos_07da, label=percent(pct_pos_07da)), 
            vjust = -1,
            fontface = "bold") +
  theme_minimal() +
  theme(legend.position      = "none",
        plot.title.position  = "plot",
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  coord_cartesian(ylim = c(0, .3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs( x        = " ", 
        y        = "Tests % Positive \n(7 day average)", 
        fill     = " ",
        caption  = "Data: Provided by New York State Department of Health
        Chart design: Joanna R. Pepin")
p2

# Combine charts
fig <- ggarrange(p1, p2, nrow = 2)
fig

ggsave(filename = "erie.png", fig, width=6, height=6, units="in", dpi=300)

