#happiness analysis hello
happy= read.csv("happy.csv")
View(happy)
library("tidyverse")
#to be sure that there are not missing values in my data 
happy= na.omit(happy)
View(filtered_dataset)

str(happy)
glimpse(happy)

#analysis done by the other man
#he wants to focus on the score on 2018 only
filtered_dataset <- happy %>% arrange(desc(Score)) %>% filter(Year == "2018")

#shows which country has the highest score 
ggplot(data = filtered_dataset, mapping = aes(x = reorder(Country_or_region, Score), y = Score)) +
  geom_bar(stat = "identity") +
  labs(title = "Scores by Country", x = "Country", y = "Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Now, for defining which of the six components of the research is better correlated with the score,
#we will plot 6 graphs, faceting by year, and adding a linear regression line. 

#here score depends on GDP per capita
ggplot(data = happy, mapping = aes(x = GDP_per_capita, y = Score))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Score by GDP Per Capita", caption = "X values are in a range of 0 - 2", x = "GDP Per Capita") +
  facet_wrap(~Year)

#now score depends on social support
ggplot(data = happy, mapping = aes(x = Social_support, y = Score))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Score by Social Support", caption = "X values are in a range of 0 - 2", x = "Social Support") +
  facet_wrap(~Year)

#now score depends on healthy life expectancy
ggplot(data = happy, mapping = aes(x = Healthy_life_expectancy, y = Score))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Score by Life Expectancy", caption = "X values are in a range of 0 - 1", x = "Life Expectancy") +
  facet_wrap(~Year)

#now score depends on freedom to make life choices
ggplot(data = happy, mapping = aes(x = Freedom_to_make_life_choices, y = Score))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Score by Freedom to Make Life Choices", caption = "X values are in a range of 0 - 1", x = "Freedom") +
  facet_wrap(~Year)

#now score depends on generositiy
ggplot(data = happy, mapping = aes(x = Generosity, y = Score))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Score by Generosity", caption = "X values are in a range of 0 - 1", x = "Generosity") +
  facet_wrap(~Year)

#now score depends on perception of corruption
ggplot(data = happy, mapping = aes(x = Perceptions_of_corruption, y = Score))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Score by Perception of Corruption", caption = "X values are in a range of 0 - 1", x = "Perception of Corruption") +
  facet_wrap(~Year)

#########
#now we start the regression model
linear_model <- lm(Score ~ GDP_per_capita + Social_support + Healthy_life_expectancy + Freedom_to_make_life_choices + Generosity + Perceptions_of_corruption, data = happy)
summary(linear_model)
