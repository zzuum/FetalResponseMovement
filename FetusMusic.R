# MAE 207 Project ----
# Fetal Movement Analysis
# Niklas Braun
####
# Libraries ----
library(ggplot2)
library(ggthemes)
library(tidyverse)

# Data import ----
classical.silence <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 1,
  skip = 2)[, 1:6] # Extra Unknown column
colnames(classical.silence) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Time'
)
classical.low <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 2,
  skip = 2)[, 1:6] # Extra Unknown column
colnames(classical.low) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Time'
)
classical.med <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 3,
  skip = 2)[, 1:6] # Extra Unknown column
colnames(classical.med) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Time'
)
classical.high <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 4,
  skip = 2)[, 1:6] # Extra Unknown column
colnames(classical.high) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Time'
)

# Data Cleaning ----
classical.silence$Movelog <- 0
for (time in na.omit(classical.silence$`Move Time`)){
  classical.silence$Movelog[classical.silence$Time == time] <- 1
}
classical.low$Movelog <- 0
for (time in na.omit(classical.low$`Move Time`)){
  classical.low$Movelog[classical.low$Time == time] <- 1
}
classical.med$Movelog <- 0
for (time in na.omit(classical.med$`Move Time`)){
  classical.med$Movelog[classical.med$Time == time] <- 1
}
classical.high$Movelog <- 0
for (time in na.omit(classical.high$`Move Time`)){
  classical.high$Movelog[classical.high$Time == time] <- 1
}

# Initial Exploration ----
# Exploration by music volume

silence <- ggplot(data = classical.silence) + 
  geom_jitter(aes(Time, `Ch. A`, color = "A")) + 
  geom_jitter(aes(Time, `Ch. B`, color = "B")) + 
  geom_jitter(aes(Time, `Ch. C`, color = "C")) + 
  geom_vline(
    xintercept = classical.silence$Time[classical.silence$Movelog == 1],
    color = '#FF3300') +
  scale_colour_manual(
    name = "Channel", 
    values = c("A" = '#99CCFF', "B" = '#CA7AD2', "C" = '#FF6666')) +
  theme_gray(base_size = 18) + 
  theme(axis.text = element_text(color = "black"),
        legend.key.height  = grid::unit(0.1, "npc")) +
  labs(x = "Time (s)", fontface = "bold") + 
  labs(y = "Acceleration (V)") +
  labs(title = "Acceleration, No Music",
       subtitle = 'Vertical lines signify distinct fetal movement')
print(silence)

low <- ggplot(data = classical.low) + 
  geom_jitter(aes(Time, `Ch. A`, color = "A")) + 
  geom_jitter(aes(Time, `Ch. B`, color = "B")) + 
  geom_jitter(aes(Time, `Ch. C`, color = "C")) + 
  geom_vline(
    xintercept = classical.low$Time[classical.low$Movelog == 1],
    color = '#FF3300') +
  scale_colour_manual(
    name = "Channel", 
    values = c("A" = '#99CCFF', "B" = '#CA7AD2', "C" = '#FF6666')) +
  theme_gray(base_size = 18) + 
  theme(axis.text = element_text(color = "black"),
        legend.key.height  = grid::unit(0.1, "npc")) +
  labs(x = "Time (s)", fontface = "bold") + 
  labs(y = "Acceleration (V)") +
  labs(title = "Acceleration, 65 db Music",
       subtitle = 'Vertical lines signify distinct fetal movement')
print(low)

med <- ggplot(data = classical.med) + 
  geom_jitter(aes(Time, `Ch. A`, color = "A")) + 
  geom_jitter(aes(Time, `Ch. B`, color = "B")) + 
  geom_jitter(aes(Time, `Ch. C`, color = "C")) + 
  geom_vline(
    xintercept = classical.med$Time[classical.med$Movelog == 1],
    color = '#FF3300')+
  scale_colour_manual(
    name = "Channel", 
    values = c("A" = '#99CCFF', "B" = '#CA7AD2', "C" = '#FF6666')) +
  theme_gray(base_size = 18) + 
  theme(axis.text = element_text(color = "black"),
        legend.key.height  = grid::unit(0.1, "npc")) +
  labs(x = "Time (s)", fontface = "bold") + 
  labs(y = "Acceleration (V)") +
  labs(title = "Acceleration, 75 db Music",
       subtitle = 'Vertical lines signify distinct fetal movement')
print(med)

high <- ggplot(data = classical.high) + 
  geom_jitter(aes(Time, `Ch. A`, color = "A")) + 
  geom_jitter(aes(Time, `Ch. B`, color = "B")) + 
  geom_jitter(aes(Time, `Ch. C`, color = "C")) + 
  geom_vline(
    xintercept = classical.high$Time[classical.high$Movelog == 1],
    color = '#FF3300')+
  scale_colour_manual(
    name = "Channel", 
    values = c("A" = '#99CCFF', "B" = '#CA7AD2', "C" = '#FF6666')) +
  theme_gray(base_size = 18) + 
  theme(axis.text = element_text(color = "black"),
        legend.key.height  = grid::unit(0.1, "npc")) +
  labs(x = "Time (s)", fontface = "bold") + 
  labs(y = "Acceleration (V)") +
  labs(title = "Acceleration, 85 db Music",
       subtitle = 'Vertical lines signify distinct fetal movement')
print(high)

# Combining Data ----

classical.silence$Level <- 0
classical.low$Level <- 1
classical.med$Level <- 2
classical.high$Level <- 3

classical <- rbind(
  classical.silence[, -5:-7],
  classical.low[, -5:-7],
  classical.med[, -5:-7],
  classical.high[, -5:-7]
)
classical$Level <- factor(classical$Level)
# Export to csv
write.csv(classical, file = 'ClassicalMusicVoltages.csv')

# New Exploration ----
# Exploration by Channel

ch.A <- ggplot(data = classical) + 
  geom_jitter(aes(Time, `Ch. A`, color = Level), alpha = 0.5) + 
  theme_gray(base_size = 18) + 
  theme(axis.text = element_text(color = "black"),
        legend.key.height  = grid::unit(0.1, "npc")) +
  scale_colour_manual(
    name = "Sound Level", 
    values = c('black', 'brown', 'orange', 'yellow'),
    labels = c('0 db', '65 db', '75 db', '85 db')) +
  ylim(c(1.9, 2.1)) +
  labs(x = "Time (s)", fontface = "bold") + 
  labs(y = "Acceleration (V)") +
  labs(title = "Acceleration, Channel A")
print(ch.A)

ch.B <- ggplot(data = classical) + 
  geom_jitter(aes(Time, `Ch. B`, color = Level), alpha = 0.5) + 
  theme_gray(base_size = 18) + 
  theme(axis.text = element_text(color = "black"),
        legend.key.height  = grid::unit(0.1, "npc")) +
  scale_colour_manual(
    name = "Sound Level", 
    values = c('black', 'brown', 'orange', 'yellow'),
    labels = c('0 db', '65 db', '75 db', '85 db')) +
  ylim(c(1.75, 1.85)) +
  labs(x = "Time (s)", fontface = "bold") + 
  labs(y = "Acceleration (V)") +
  labs(title = "Acceleration, Channel B")
print(ch.B)

ch.C <- ggplot(data = classical) + 
  geom_jitter(aes(Time, `Ch. C`, color = Level), alpha = 0.5) + 
  theme_gray(base_size = 18) + 
  theme(axis.text = element_text(color = "black"),
        legend.key.height  = grid::unit(0.1, "npc")) +
  scale_colour_manual(
    name = "Sound Level", 
    values = c('black', 'brown', 'orange', 'yellow'),
    labels = c('0 db', '65 db', '75 db', '85 db')) +
  ylim(c(1.75, 1.85)) + 
  labs(x = "Time (s)", fontface = "bold") + 
  labs(y = "Acceleration (V)") +
  labs(title = "Acceleration, Channel C")
print(ch.C)

# Basic Regression ----
## This is not going to be correct. The data has not been fixed to reflect 
# noise correction or acceleration.
ch.a.model <- lm(`Ch. A` ~ Level, classical)
summary(ch.a.model)
# Correct Data Import ----
sil <- c(11.713,7.575,17.096,17.585,14.899,11.969,7.318,6.598,13.190,13.190)
db65 <- c(8.202,8.446,12.108,9.667,8.202,5.470,8.156,11.620,6.372,10.109,
          10.643,8.888,12.596,8.400,7.713,9.497,9.911,6.935,6.935)
db75 <- c(8.522,6.895,5.837,8.278,9.255,9.987,6.163,8.116,8.323,8.079,10.276,
          6.777,8.604,8.604)

total <- c()
leve <- c()
for (i in sil){
  total <- append(total, i)
  leve <- append(leve, 0)
}
for (i in db65){
  total <- append(total, i)
  leve <- append(leve, 65)
}
for (i in db75){
  total <- append(total, i)
  leve <- append(leve, 75)
}
data <- data.frame(total, leve)
data$leve <- factor(data$leve)

real.model <- lm(total ~ leve, data = data)
anova(real.model)
real.model
