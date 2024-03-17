library(readxl)
library(tidyverse)
library(stringr)
library(magrittr)


# 1280 rows
# ADB - production of industries
# ADE - Men employed in selected industries

production <- read_xlsx("USCensus_1840_county.xlsx")

# 1281 rows
# ACX - County Borders Navigable waterway, 1 if yes
navig <- read_xlsx("USCensusData1840.xlsx")

# 1280 rows
# ADC - Capital Invested
# ADD - Persons Employed
capital <- read_xlsx("USCensusData1840_2.xlsx")

# bigboy is all the data sets combined
bigboy <- production %>% inner_join(navig) %>% inner_join(capital) 

# I separate the header descriptions into a separate variable, then clean bigboy up a bit
bigboy_head <- bigboy %>% slice_head()
bigboy %<>% slice(-1) %>% select(-YEAR) %>% 
  mutate(across(starts_with(c("ad", "ac")), as.numeric))

bigboy_head %>% View()
# lookup function for columns so we can check what each column represents quickly
colLook <- function(columnNames){
  codeDict <- c("ADE"="Men Employed In", "ADB"="Production Of", "ACX"="Navigable Waterway",
                "ADC"="Capital Invested", "ADD" = "")
  for (i in columnNames){
    print(paste(i,':', codeDict[substr(i, 1, 3)],bigboy_head[i][[1]]))
  }
}

colLook(c("ADC010", "ADC011"))



bigboy %<>% mutate(menSum = rowSums(across(starts_with("ADE")))) %>%
  mutate(peopleSum = rowSums(across(starts_with("ADD"))))  %>% 
  mutate(capitalSum = rowSums(across(starts_with("ADC"))))

#remove all rows with NA values
bigboy <- bigboy[!rowSums(is.na(bigboy)),]



# ggplot(data = bigboy, aes(x = peopleSum, y = capitalSum)) +
#   geom_point() +
#   theme_bw()
# 
# ggplot(data = bigboy, aes(x = menSum, y = capitalSum)) +
#   geom_point() +
#   theme_bw()

# bigboy %>% filter(ACX001 == 1) %>% arrange(desc(capitalSum)) %>% View()
# 
# ggplot(data=bigboy,aes(x=peopleSum,y=reorder(ACX001,-peopleSum))) +
#   geom_boxplot(fill="Tan") +
#   labs(title="Car Horsepower Boxplot",
#        x="Amount of People Working",
#        y="On Navigale Waterway") +
#   theme_bw()
# 
ggplot(data=bigboy,aes(x=capitalSum,y=reorder(ACX001,-capitalSum))) +
  geom_boxplot(fill="Tan") +
  labs(title="Car Horsepower Boxplot",
       x="Amount of Capital Invested",
       y="On Navigale Waterway") +
  theme_bw()

# adb012 - sperm and wax candles produced
# adb014 /15 brick houses vs wooden houses
# adb021 sperm oil gallons, could be useful for y/n
# adb019 quintailes of dried fish
# adc031 sugar refinieries?
# add005 persons in ocean
# add006 persons in inland navigiation, actually kinda good

bigboy %>% arrange(desc(ADD005)) %>% filter(ACX001 == 0) %>% View()

ggplot(data=bigboy,aes(x=ADD001,y=reorder(ACX001,-ADD001))) +
  geom_boxplot(fill="Tan") +
  labs(title="Brich Houses Employed",
       x="Amount of Capital Invested",
       y="On Navigale Waterway") +
  theme_bw()

modelCap <- glm(data = bigboy, formula = "ACX001 ~ capitalSum", family="binomial")
summary(modelFirst)


modelSpaghetti <- glm(data = bigboy, 
      formula = "ACX001 ~  capitalSum + ADD005 +ADD006", family="binomial")
summary(modelSpaghetti)
