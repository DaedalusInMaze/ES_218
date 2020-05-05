library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tmap) 
library(spData)
library(spDataLarge)
library(sf)

install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')


# Want to test if sex, car type, 

dat <- readRDS('Data/farsp.RDS')

# group the states into four main regions in U.S

# Regroup and create region variable
west <- c(2,4,6,8,15,16,32,35,41,49,53,56 )
midwe <- c(17,18,10,20,26,27,30,31,38,39,46)
south <- c(1,5,11,12,13,21,22,24,28,29,37,40,45,47,48,51,54)
northea <- c(9,10,23,25,33,34,36,42,44,50,55 )
oth <- c(43,52)

# Regroup car type

dat <- dat %>% 
  mutate('Region' = case_when(state  %in% west ~ 'West',
                              state %in% midwe ~ 'MidWest',
                              state  %in% south ~ 'South',
                              state  %in% northea ~ 'NorthEast',
                              state  %in% oth ~ 'Other'))




### visualize how the factors differ from region to region

# summarize by area first
data("World")

tm_shape(World) +
  tm_polygons("HPI")




### time trend of the accidents
dat_full <- dat %>% 
  select(-c(day, hour, minute, county)) %>% 
  # filter out severity
  filter(inj_sev %in% c(1,2,3,4,5,6))%>% 
  drop_na(Region)


# Make a graph of how accidents happen related to sex
# male vs female by year
dat_sex <- dat_full %>%
  group_by(year, sex) %>% 
  summarise(n = n()) %>% 
  filter(sex %in% c(1,2)) %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(percent = n/sum(n))

ggplot(dat_sex, aes(year, percent, col = sex)) + geom_point() +
  xlab('Year') + ylab('Percentage of Accidents') + 
  ggtitle('Accidents breakdown by sex') +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

# male vs female by region
dat_sex_loc <- dat_full %>% 
  group_by(Region, sex) %>% 
  summarise(n = n()) %>% 
  filter(sex %in% c(1,2)) %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(percent = n/sum(n))

# univariate
ggplot(dat_sex_loc, aes(Region, percent, fill = sex)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  xlab('Year') + ylab('Percentage of Accidents') + 
  ggtitle('Accidents breakdown by state and sex') +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))
  


# collision by year

dat_col <- dat_full %>%
  group_by(year, man_coll) %>% 
  summarise(n = n()) %>% 
  mutate(Collision = case_when(man_coll == "0" ~ 'Not with motor',
                               man_coll == "1" ~ 'Front-Rear',
                               man_coll == "2" ~ 'Front-Front',
                               man_coll == "3" ~ 'Not shown on Table (Known type 1)',
                               man_coll == "4" ~ 'Not shown on Table (Known type 2)',
                               man_coll == "6" ~ 'Angle',
                               man_coll == "7" ~ 'Sideswipe (same direc)',
                               man_coll == "8" ~ 'Sideswipe (oppo)',
                               man_coll == "9" ~ 'Rear-Side',
                               man_coll == "10" ~ 'Rear-Rear',
                               TRUE ~ 'Other')) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(Collision != 'Other') %>% 
  mutate(percent = n/sum(n))

ggplot(dat_col, aes(year, percent, col = Collision)) + geom_point() +
  geom_line() + xlab('Year') + ylab('Percentage of Accidents') + 
  ggtitle('Accidents breakdown by Collision type') +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))


# keep the Region variable
dat_col_reg <- dat_full %>%
  group_by(year, man_coll, Region) %>% 
  summarise(n = n()) %>% 
  mutate(Collision = case_when(man_coll == "0" ~ 'Not with motor',
                               man_coll == "1" ~ 'Front-Rear',
                               man_coll == "2" ~ 'Front-Front',
                               man_coll == "3" ~ 'Not shown on Table (Known type 1)',
                               man_coll == "4" ~ 'Not shown on Table (Known type 2)',
                               man_coll == "6" ~ 'Angle',
                               man_coll == "7" ~ 'Sideswipe (same direc)',
                               man_coll == "8" ~ 'Sideswipe (oppo)',
                               man_coll == "9" ~ 'Rear-Side',
                               man_coll == "10" ~ 'Rear-Rear',
                               TRUE ~ 'Other')) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  filter(Collision != 'Other') %>% 
  mutate(percent = n/sum(n))

ggplot(dat_col_reg, aes(year, percent, col = Collision)) + 
  geom_point() + geom_line() + facet_wrap( ~ Region) +
  xlab('Year') + ylab('Percentage of Accidents') + 
  ggtitle('Accidents breakdown by Collision type') +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

# Region by year
dat_reg <- dat_full %>%
  group_by(year, Region) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(percent = n/sum(n))%>% 
  drop_na(Region)

ggplot(dat_reg, aes(year, percent, fill = Region)) + 
  geom_bar(position = 'stack', stat = 'identity') +
  xlab('Year') + ylab('Percentage of Accidents') + 
  ggtitle('Accidents breakdown by State') +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

### Age vs accidents by region
dat_age <- dat_full %>%
  group_by(year, Region) %>% 
  summarise(avg = mean(age))%>% 
  drop_na(Region)

ggplot(dat_age, aes(year, avg, col = Region)) + geom_point() +
  geom_smooth(se = FALSE, method = 'loess') +
  xlab('Year') + ylab('Average age of accident drivers') + 
  ggtitle('Average age of accident drivers') +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

# Number of person vs year
dat_per <- dat_full %>%
  group_by(year, Region) %>% 
  summarise(avg = mean(per_no))

ggplot(dat_per, aes(year, avg, col = Region)) + geom_point() +
  geom_smooth(se = FALSE, method = 'loess') +
  xlab('Year') + ylab('Average number of people involved in accident') + 
  ggtitle('Average number of people invovled') +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))


# severity
dat_sev <- dat_full %>% 
  group_by(year, Region) %>% 
  summarise(avg = mean(inj_sev)) %>% 
  drop_na(Region)

ggplot(dat_sev, aes(year, avg, col = Region)) + geom_point() +
  geom_smooth(se = FALSE, method = 'loess')

# State
state <- read.csv("Data/state_code.csv") %>% 
  rename(state = state_code)

# Create the names for states
dat_state <- left_join(dat, state, by = 'state')

dat_states <- dat_state %>% 
  group_by(state_name) %>% 
  summarise(avrg_sev = mean(inj_sev))

# create plot
ggplot(dat_states, aes(forinfreq(state_name, max(avrg_sev)), avrg_sev)) + geom_bar(stat = 'identity') +
  xlab("State") + ylab("Average severity for each state") +
  coord_flip() + 
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")) 



# spread location to test severity across loaction
sl1 <- dat_full %>% 
  filter(sex %in% c(1,2)) %>% 
  group_by(Region) %>% 
  mutate(med = median(inj_sev),
         res = sqrt(abs(inj_sev - med))) %>% 
  summarise(med = median(inj_sev), med_res = median(res))

ggplot(sl1, aes(med, med_res)) +
  geom_point() +
  ylab(expression(sqrt(abs('Residuals')))) +
  geom_text(aes(x = med, y = 200, label = Region))



### model relationship between severity and other variables
  
# Bivariate

# fit loess for severity and other variables

lm_sev_age <- lm(inj_sev ~ age * Region, dat = dat_full)
summary(lm_sev_age)


# fit with more variables
lm_all <- lm(inj_sev ~ age * Region + sex + man_coll + per_no, dat = dat_full)
summary(lm_all)

## fits and residuals



