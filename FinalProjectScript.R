require(dplyr)
require(ggplot2)
require(tidyr)
require(extrafont)

dating_data <- read.csv('SpeedDatingData.csv')

miles_theme <- theme(
  panel.grid.major = element_line(color = '#CDCDCD'), ##
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
  unique()
age_female <- filter(age_data, gender == 0)
age_male <- filter(age_data, gender == 1)

age_chart <- ggplot() +
  geom_histogram(data = age_female, aes(x = age_female$age, y = ..count..),
                 binwidth = 1, alpha = 0.5, fill = '#7D1935') +
  geom_histogram(data = age_male, aes(x = age_male$age, y = ..count..),
                 binwidth = 1, alpha = 0.5, fill = '#114B5F') +
  labs(title = 'Age of Participants', 
       x = 'Age', y = 'Participants') +
  miles_theme


race_data <- dating_data %>%
  select(iid, gender, race) %>%
  unique()

race_data$gender <- factor(race_data$gender)
race_data$gender <- x1[match(race_data$gender, c(1, 0))]

x3 <- c('Black', 'White', 'Latin@', 'Asian', 'Native American', 'Other')
race_data$race <- x3[match(race_data$race, c(1, 2, 3, 4, 5, 6))]

race_data$race <- factor(race_data$race, levels = rev(c('White', 'Asian', 'Latin@', 'Black',
                                                        'Other')))

race_chart <- ggplot(na.omit(race_data), aes(x = race, fill = gender)) +
  geom_bar(position = 'dodge') +
  labs(title = 'Race of Participants', 
       x = 'Race', y = 'Participants') +
  scale_fill_manual(values = c('#7D1935', '#114B5F')) +
  coord_flip() +
  miles_theme


goal_data <- dating_data %>%
  select(iid, gender, goal) %>%
  unique()

goal_data$gender <- factor(goal_data$gender)
goal_data$gender <- x1[match(goal_data$gender, c(1, 0))]

x2 <- c('Seemed like a fun night out', 'To meet new people',
        'To get a date', 'Looking for a serious relationship',
        'To say I did it', 'Other')
goal_data$goal <- x2[match(goal_data$goal, c(1, 2, 3, 4, 5, 6))]

goal_data$goal <- factor(goal_data$goal, levels = rev(c('Seemed like a fun night out',
                                                  'To meet new people', 'To get a date',
                                                  'Looking for a serious relationship',
                                                  'To say I did it', 'Other')))

goal_chart <- ggplot(na.omit(goal_data), aes(x = goal, fill = gender)) +
  geom_bar(position = 'dodge') +
  labs(title = 'What is your primary goal in participating in this event?', 
       x = 'Goal', y = 'Participants') +
#  scale_x_reverse() +
  scale_fill_manual(values = c('#7D1935', '#114B5F')) +
  coord_flip() +
  miles_theme


career_data <- dating_data %>%
  select(iid, gender, career_c) %>%
  unique()

career_chart <- ggplot(career_data, aes(x = career_c)) +
  geom_bar(fill = '#7D1935') +
  labs(title = 'What is your intended career?', 
       x = 'Career Field', y = 'Participants') +
  coord_flip() +
  miles_theme


pickiness_data_1 <- dating_data %>%
  select(iid, gender, attr_o, dec)
pickiness_data_2 <- summarize(group_by(pickiness_data_1, iid),
                              attractiveness = mean(attr_o, na.rm = TRUE))
pickiness_data_3 <- summarize(group_by(pickiness_data_1, iid),
                              pickiness = mean(dec, na.rm = TRUE))
pickiness_data <- left_join(pickiness_data_2, pickiness_data_3, c('iid' = 'iid')) #%>%
#  left_join(genders, c('iid' = 'iid'))

pickiness_chart <- ggplot(pickiness_data, aes(x = attractiveness, y = (100*pickiness))) +
  geom_point(color = '#114B5F') +
  geom_smooth(color = '#7D1935', size = 2, se = FALSE) +
  labs(title = 'Are Attractive People More Picky?', 
       x = 'Average Attractiveness Score', y = 'Percentage of Partners Liked') +
  miles_theme#+
#  facet_wrap(~gender)

#print(pickiness_chart)


effort_data_1 <- dating_data %>%
  select(iid, match, date)
effort_data_2 <- effort_data_1
effort_data_1 <- summarize(group_by(effort_data_1, iid),
                           success = mean(match, na.rm = TRUE))
effort_data_2 <- summarize(group_by(effort_data_2, iid),
                           effort = mean(date, na.rm = TRUE))
effort_data <- left_join(effort_data_1, effort_data_2, c('iid' = 'iid'))

effort_chart <- ggplot(effort_data, aes(x = effort, y = (100*success))) +
  geom_jitter(width = 0.2, color = '#114B5F') +
  geom_smooth(method = 'lm', color = '#7D1935', size = 2, se = FALSE) +
  labs(title = 'Does Effort Affect Success?', 
       x = 'Effort', y = 'Match Percentage') +
  scale_x_reverse() +
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

mutual_chart <- ggplot(mutual_data, aes(x = gender, y = (100*percentage),
                                        fill = reciprocity)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'Are we more attracted to people who are attracted to us?', 
       x = 'Gender', y = 'Percentage') +
  scale_fill_manual(values = c('#7D1935', '#114B5F')) +
  coord_flip() +
  miles_theme


projection_data <- dating_data %>%
  select(iid, attr_o, sinc_o, intel_o, fun_o, amb_o, shar_o,
         attr4_1, sinc4_1, intel4_1, fun4_1, amb4_1, shar4_1,
         attr2_1, sinc2_1, intel2_1, fun2_1, amb2_1, shar2_1) %>%
  mutate(attr_ = ifelse(is.na(attr4_1), attr2_1, (attr4_1))) %>%
  mutate(sinc_ = ifelse(is.na(sinc4_1), sinc2_1, (sinc4_1))) %>%
  mutate(intel_ = ifelse(is.na(intel4_1), intel2_1, (intel4_1))) %>%
  mutate(fun_ = ifelse(is.na(fun4_1), fun2_1, (fun4_1))) %>%
  mutate(amb_ = ifelse(is.na(amb4_1), amb2_1, (amb4_1))) %>%
  mutate(shar_ = ifelse(is.na(shar4_1), shar2_1, (shar4_1))) %>%
  select(iid, attr_, sinc_, intel_, fun_, amb_, shar_) %>%
  unique() %>%
  mutate()
#Need to return the name of the column with the highest value.
#If tied... need to figure that out



#ggsave('age_chart.png', age_chart)
#ggsave('career_chart.png', career_chart)
#ggsave('effort_chart.png', effort_chart)
#ggsave('goal_chart.png', goal_chart)
#ggsave('mutual_chart.png', mutual_chart)
#ggsave('pickiness_chart.png', pickiness_chart)
#ggsave('race_chart.png', race_chart)