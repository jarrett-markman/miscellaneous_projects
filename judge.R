library(tidyverse)
library(readxl)
judge_homers <- read_excel("~/Downloads/judge_homers.xls")
judge_homers <- judge_homers %>%
  separate(Inn, c('inning_half', 'inning'), ' ') %>%
  mutate(inning = as.numeric(inning))
judge_homers %>%
  ggplot(aes(x=RBI)) +
  geom_histogram(bins = 4, col = 'black', fill = 'blue3') +
  labs(
    x='Home Run Type',
    y='Home Runs',
    title = 'Aaron Judge Home Run Distribution',
    caption = 'Jarrett Markman | Data: Baseball Reference'
  ) +
  theme_bw() + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
judge_homers %>%
  filter(BOP == 1) %>%
  ggplot(aes(x=inning)) +
  geom_histogram(bins = 9, col = 'black', fill = 'blue3', binwidth = 1) +
  labs(
    x= "Inning",
    y='Home Runs',
    title = 'Aaron Judge Home Run Distribution Hitting First in the Lineup',
    caption = 'Jarrett Markman | Data: Baseball Reference'
  ) +
  theme_bw() + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
judge_homers %>%
  filter(BOP != 1 & RBI == 1) %>%
  ggplot(aes(x=inning)) +
  geom_histogram(bins = 9, col = 'black', fill = 'blue3', binwidth = 1) +
  labs(
    x= "Inning",
    y='Home Runs',
    title = 'Aaron Judge Solo Home Run Distribution when not Hitting First in the Lineup',
    caption = 'Jarrett Markman | Data: Baseball Reference'
  ) +
  theme_bw() + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
judge_homers %>%
  group_by(BOP) %>%
  count()
47/62
13/62
2/62
judge_homers %>%
  group_by(BOP) %>%
  ggplot(aes(x=BOP)) +
  geom_histogram(bins = 3, col = 'black', fill = 'blue3', binwidth = 1) +
  labs(
    x = "Spot in the Lineup",
    y = 'Count',
    title = "Aaron Judge Lineup Placement",
    caption = "Jarrett Markman | Data: Baseball Reference"
  ) +
  theme_bw() + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
judge_homers %>%
  filter(RBI == 1) %>%
  group_by(Out) %>%
  ggplot(aes(x=Out)) +
  geom_histogram(bins = 3, col = 'black', fill = 'blue3', binwidth = 1) +
  labs(
    x= "Outs in the Inning",
    y='Home Runs',
    title = "Aaron Judge Solo Home Runs by Outs in the Inning",
    caption = "Jarrett Markman | Data: Baseball Reference"
  ) +
  theme_bw() + 
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
solo <- judge_homers %>%
  filter(RBI == 1)
two <- judge_homers %>%
  filter(RBI == 2)
three <- judge_homers %>%
  filter(RBI == 3)
four <- judge_homers %>%
  filter(RBI == 4)
solo %>%
  filter(Notes == 'Lead-Off')
pcts <- c(13/156, 47/506, 2/30)
bops <- c(1, 2, 3)
data <- data.frame(pcts, bops)
data %>%
  ggplot(aes(x=pcts)) +
  geom_histogram(col = 'black', fill = 'blue3')
barplot(pcts, main = 'Aaron Judge Home Run Percentage Distribution by Spot in the Lineup',
        xlab = 'Spot in the Batting Order', ylab = 'Homer Run Percentage', names.arg = bops,
        col = 'blue3')