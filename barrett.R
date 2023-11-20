library(tidyverse)
library(nbastatR)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
# How did he perform last year?
data <- nbastatR::game_logs(2022)
data <- data %>%
  filter(namePlayer == 'RJ Barrett')
data %>%
  ggplot(aes(x=numberGameTeamSeason, pctFG)) +
  geom_point(col = 'blue2') +
  geom_line(col = 'darkorange1') +
  labs(
    x="Games",
    y='Field Goal Percentage',
    title = 'RJ Barrett Game-by-Game Field Goal Percentage in 2021-2022',
    caption = 'Jarrett Markman | Data: nbastatR'
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
data <- data %>%
  mutate(
    fg_below_40 = case_when(
      pctFG < .4 ~ 1,
      pctFG > .4 ~ 0,
      pctFG == .4 ~ 0
      ),
    fg_above_40 = case_when(
      pctFG < .4 ~ 0,
      pctFG > .4 ~ 1,
      pctFG == .4 ~ 0
    ),
    fg_40 = case_when(
      pctFG < .4 ~ 0,
      pctFG > .4 ~ 0,
      pctFG == .4 ~ 1
    ),
    games_below_40 = sum(fg_below_40),
    games_above_40 = sum(fg_above_40),
    games_40 = sum(fg_40)
)
dataset <- c(34, 32, 4)
labels <- c("Below 40%", "Above 40%", "At 40%")
barplot(dataset, main = 'RJ Barrett Field Goal Percentage Distribution 2021-2022',
        xlab = 'Field Goal Percentage', ylab = 'Count', names.arg = labels,
        col = 'darkorange1')
data %>%
  ggplot(aes(pctFG)) +
  geom_histogram(fill = 'darkorange1', col = 'blue2', bins = 3, binwidth = .01) +
  labs(
    x = "Field Goal Percentage",
    y = 'Count',
    title = 'RJ Barrett Field Goal Percentage Distribution 2021-2022',
    caption = 'Jarrett Markman | Data: nbastatR'
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
# What about his other seasons?
data <- game_logs(2021) %>%
  filter(namePlayer == 'RJ Barrett')
data %>%
  ggplot(aes(x=numberGameTeamSeason, pctFG)) +
  geom_point(col = 'blue2') +
  geom_line(col = 'darkorange1') +
  labs(
    x="Games",
    y='Field Goal Percentage',
    title = 'RJ Barrett Game-by-Game Field Goal Percentage in 2020-2021',
    caption = 'Jarrett Markman | Data: nbastatR'
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
data <- data %>%
  mutate(
    fg_below_40 = case_when(
      pctFG < .4 ~ 1,
      pctFG > .4 ~ 0,
      pctFG == .4 ~ 0
      ),
    fg_above_40 = case_when(
      pctFG < .4 ~ 0,
      pctFG > .4 ~ 1,
      pctFG == .4 ~ 0
    ),
    fg_40 = case_when(
      pctFG < .4 ~ 0,
      pctFG > .4 ~ 0,
      pctFG == .4 ~ 1
    ),
    games_below_40 = sum(fg_below_40),
    games_above_40 = sum(fg_above_40),
    games_40 = sum(fg_40)
)
dataset <- c(27, 40, 1)
labels <- c("Below 40%", "Above 40%", "At 40%")
barplot(dataset, main = 'RJ Barrett Field Goal Percentage Distribution 2020-2021',
        xlab = 'Field Goal Percentage', ylab = 'Count', names.arg = labels,
        col = 'darkorange1')
data %>%
  ggplot(aes(pctFG)) +
  geom_histogram(fill = 'darkorange1', col = 'blue2', bins = 3, binwidth = .01) +
  labs(
    x = "Field Goal Percentage",
    y = 'Count',
    title = 'RJ Barrett Field Goal Percentage Distribution 2020-2021',
    caption = 'Jarrett Markman | Data: nbastatR'
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
# How consistent was he in his rookie year?
data <- game_logs(2020) %>%
   filter(namePlayer == 'RJ Barrett')
data %>%
  ggplot(aes(x=numberGameTeamSeason, pctFG)) +
  geom_point(col = 'blue2') +
  geom_line(col = 'darkorange1') +
  labs(
    x="Games",
    y='Field Goal Percentage',
    title = 'RJ Barrett Game-by-Game Field Goal Percentage in 2019-2020',
    caption = 'Jarrett Markman | Data: nbastatR'
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
```
data <- data %>%
  mutate(
    fg_below_40 = case_when(
      pctFG < .4 ~ 1,
      pctFG > .4 ~ 0,
      pctFG == .4 ~ 0
      ),
    fg_above_40 = case_when(
      pctFG < .4 ~ 0,
      pctFG > .4 ~ 1,
      pctFG == .4 ~ 0
    ),
    fg_40 = case_when(
      pctFG < .4 ~ 0,
      pctFG > .4 ~ 0,
      pctFG == .4 ~ 1
    ),
    games_below_40 = sum(fg_below_40),
    games_above_40 = sum(fg_above_40),
    games_40 = sum(fg_40)
)
dataset <- c(30, 23, 3)
labels <- c("Below 40%", "Above 40%", "At 40%")
barplot(dataset, main = 'RJ Barrett Field Goal Percentage Distribution 2019-2020',
        xlab = 'Field Goal Percentage', ylab = 'Count', names.arg = labels,
        col = 'darkorange1')
data %>%
  ggplot(aes(pctFG)) +
  geom_histogram(fill = 'darkorange1', col = 'blue2', bins = 3, binwidth = .01) +
  labs(
    x = "Field Goal Percentage",
    y = 'Count',
    title = 'RJ Barrett Field Goal Percentage Distribution 2019-2020',
    caption = 'Jarrett Markman | Data: nbastatR'
  ) +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
