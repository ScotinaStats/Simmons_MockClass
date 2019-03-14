library(ggplot2)
library(dplyr)
library(babynames)
data(babynames)

## Line plot for trends in Anthony name COUNT
babynames %>%
  filter(name =="Anthony" & sex == "M") %>%
  ggplot(aes(year, n)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Name Count by Year", title = "Trends for Babies Named Anthony") + 
  geom_vline(xintercept = 1989, col = "blue", linetype = "dashed", size = 1) + 
  geom_text(aes(x = 1992, label = "1989", y = 2500), colour="red")

## Create a new dataset that adds in RANK variable
## within sex and year
babynames.new =
  babynames %>%
  group_by(year, sex) %>%
  mutate(rank = row_number())


## Line plot for trends in Anthony name RANK
babynames.new %>%
  filter(name == "Anthony" & sex == "M") %>%
  ggplot(aes(x = year, y = rank, group = name)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Name Rank by Year", title = "Anthony Rank") + 
  scale_y_reverse() + 
  geom_vline(xintercept = 1989, col = "blue", linetype = "dashed", size = 1) + 
  geom_text(aes(x = 1992, label = "1989", y = 100), colour = "red")


## Line plot for Anthony vs Dylan name RANK
babynames.new %>%
  filter(name %in% c("Anthony", "Dylan") & sex == "M") %>%
  ggplot(aes(year, rank, group = name)) +
  geom_line(aes(color = name)) +
  geom_point(aes(color = name)) +
  xlab("Year") +
  ylab("Name Rank by Year") +
  scale_y_reverse() 


## Line plot for Anthony vs Dylan name RANK, years 1989-2017
babynames %>%
  filter(name %in% c("Anthony", "Dylan") & sex == "M" & year %in% 1989:2017) %>%
  ggplot(aes(year, rank, group = name)) +
  geom_line(aes(color = name)) +
  geom_point(aes(color = name)) +
  xlab("Year") +
  ylab("Name Rank by Year") +
  scale_y_reverse() 


## What does this code do?
## Filters the year 1989, alphabetizes name
babynames %>%
  filter(year == "1989") %>%
  arrange(name)

