require(dplyr)
require(ggplot2)
require(tidyr)
require(extrafont)

dating_data <- read.csv('SpeedDatingData.csv')

miles_theme <- theme(
  panel.grid.major = element_line(color = '#CDCDCD'),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = '#F5F3EE'),
  axis.ticks = element_blank(),
  plot.margin = unit(c(25, 25, 25, 25), 'point'),
  plot.background = element_rect(fill = '#F5F3EE'),
  axis.text = element_text(color = '#333333', size = 10, family = 'Gill Sans MT'),
  axis.title = element_text(color = '#333333', size = 12, family = 'Gill Sans MT'),
  plot.title = element_text(hjust = 0, margin = margin(0,0,25,0), size = 16,
                            family = 'Gill Sans MT'),
  axis.title.x = element_text(margin = margin(10,0,0,0)),
  axis.title.y = element_text(margin = margin(0,10,0,0)),
  legend.background = element_rect(fill = NA, color = '#CDCDCD'),
  legend.key = element_rect(fill = NA ,color = NA),
  legend.spacing = unit(15, 'pt'),
  legend.title = element_blank(),
  legend.text = element_text(color = '#333333', size = 10, family = 'Calibri')
)

genders <- dating_data %>%
  select(iid, gender) %>%
  unique()
x1 <- c('Male', 'Female')


age_data <- dating_data %>%
  select(iid, gender, age) %>%
  unique() %>%
  filter(age < 40)
age_female <- filter(age_data, gender == 0)
age_male <- filter(age_data, gender == 1)

age_chart <- ggplot() +
  geom_histogram(data = age_female, aes(x = age_female$age, y = ..count..),
                 binwidth = 1, alpha = 0.5, color = 'black', fill = '#7D1935') +
  geom_histogram(data = age_male, aes(x = age_male$age, y = ..count..),
                 binwidth = 1, alpha = 0.5, color = 'black', fill = '#114B5F') +
  labs(title = 'Age of Participants', 
       x = 'Age', y = 'Participants') +
  miles_theme


race_data <- dating_data %>%
  select(iid, gender, race) %>%
  unique()

race_data$gender <- factor(race_data$gender)
race_data$gender <- x1[match(race_data$gender, c(1, 0))]

x3 <- c('Black', 'White', 'Latinx', 'Asian', 'Native American', 'Other')
race_data$race <- x3[match(race_data$race, c(1, 2, 3, 4, 5, 6))]

race_data$race <- factor(race_data$race, levels = rev(c('White', 'Asian', 'Latinx', 'Black',
                                                        'Other')))

race_chart <- ggplot(na.omit(race_data), aes(x = race, fill = gender)) +
  geom_bar(position = 'dodge', alpha = 0.8, color = '#333333') +
  labs(title = 'Race of Participants', x = 'Race', y = 'Participants') +
  scale_fill_manual(values = c('#7D1935', '#114B5F')) +
  coord_flip() +
  miles_theme


career_data <- dating_data %>%
  select(iid, gender, career_c) %>%
  unique()

career_data$gender <- factor(career_data$gender)
career_data$gender <- x1[match(career_data$gender, c(1, 0))]

x5 <- c('All Other', 'Research/Academic', 'All Other', 'All Other', 'All Other',
        'All Other', 'Finance/Business', 'All Other', 'All Other', 'All Other', 'All Other',
        'All Other', 'All Other', 'All Other', 'All Other', 'All Other', 'All Other')
career_data$career_c <- x5[match(career_data$career_c, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                                                         12, 13, 14, 15, 16, 17))]

career_chart <- ggplot(na.omit(career_data), aes(x = gender, fill = career_c)) +
  geom_bar(width = 0.5, alpha = 0.8, color = '#333333', size = 0.5) +
  labs(title = 'What is your intended career?', x = 'Gender', y = 'Participants') +
  scale_fill_manual(values = c('#77878B', '#7D1935', '#114B5F')) +
  coord_flip() +
  miles_theme


pickiness_data_1 <- dating_data %>%
  select(iid, gender, attr_o, dec)
pickiness_data_2 <- summarize(group_by(pickiness_data_1, iid),
                              attractiveness = mean(attr_o, na.rm = TRUE))
pickiness_data_3 <- summarize(group_by(pickiness_data_1, iid),
                              pickiness = mean(dec, na.rm = TRUE))
pickiness_data <- left_join(pickiness_data_2, pickiness_data_3, c('iid' = 'iid'))

pickiness_chart <- ggplot(pickiness_data, aes(x = attractiveness, y = (100*pickiness))) +
  geom_point(color = '#114B5F', size = 3, alpha = 0.3, stroke = 0) +
  geom_smooth(color = '#7D1935', size = 2, se = FALSE) +
  labs(title = 'Are Attractive People More Picky?', 
       x = 'Average Attractiveness Score', y = 'Percentage of Partners Liked') +
  miles_theme


effort_data_1 <- dating_data %>%
  select(iid, match, date)
effort_data_2 <- effort_data_1
effort_data_1 <- summarize(group_by(effort_data_1, iid),
                           success = mean(match, na.rm = TRUE))
effort_data_2 <- summarize(group_by(effort_data_2, iid),
                           effort = mean(date, na.rm = TRUE))
effort_data <- left_join(effort_data_1, effort_data_2, c('iid' = 'iid'))

effort_data$effort <- factor(effort_data$effort, levels = c(7, 6, 5, 4, 3, 2, 1))

effort_chart <- ggplot(na.omit(effort_data), aes(x = effort, y = (100*success))) +
  geom_jitter(size = 2, width = 0.2, color = '#114B5F', alpha = 0.5) +
  geom_boxplot(outlier.size = 0, outlier.stroke = 0, alpha = 0, color = '#7D1935',
               coef = 0, size = 1, width = 0.4) +
  labs(title = 'Does Effort Affect Success?', 
       x = 'Effort', y = 'Match Percentage') +
  miles_theme


mutual_data_1 <- dating_data %>%
  select(gender, dec_o, dec)
mutual_data_2 <- filter(mutual_data_1, dec_o == 0)
mutual_data_2 <- summarize(group_by(mutual_data_2, gender),
                           unrequitedpercentage = mean(dec))
mutual_data_3 <- filter(mutual_data_1, dec_o == 1)
mutual_data_3 <- summarize(group_by(mutual_data_3, gender),
                           mutualpercentage = mean(dec))
mutual_data <- left_join(mutual_data_2, mutual_data_3, c('gender' = 'gender')) %>%
  gather(reciprocity, percentage, -gender)
mutual_data$gender <- factor(mutual_data$gender)
mutual_data$gender <- x1[match(mutual_data$gender, c(1, 0))]

x4 <- c('Unrequited Likes', 'Reciprocated Likes')
mutual_data$reciprocity <- x4[match(mutual_data$reciprocity,
                                    c('unrequitedpercentage', 'mutualpercentage'))]

mutual_chart <- ggplot(mutual_data, aes(x = reciprocity, y = gender, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = round((100*percentage), 0))) +
  labs(title = 'Are we more attracted to people who are attracted to us?', 
       x = 'Reciprocity', y = 'Gender') +
  scale_fill_gradient(low = '#CFABB5', high = '#7D1935') +
  miles_theme


#ggsave('age_chart.pdf', age_chart)
#ggsave('career_chart.pdf', career_chart)
#ggsave('effort_chart.pdf', effort_chart)
#ggsave('mutual_chart.pdf', mutual_chart)
#ggsave('pickiness_chart.pdf', pickiness_chart)
#ggsave('race_chart.pdf', race_chart)