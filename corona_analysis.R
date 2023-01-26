library(tidyverse)

summary(corona)

corona %>% 
  select(Country, TotalCases) %>% 
  head(20) %>% 
  ggplot(aes(x = "", y = TotalCases, fill = Country)) +
  geom_bar(width = 1, stat = 'identity', color = "white") +
  geom_label(aes(label = TotalCases),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)+
  coord_polar('y', start = 0)

corona %>% 
  select(TotalCases, TotalTests, Population) -> sample

sample$TotalCases <- scale(sample$TotalCases)
sample$TotalTests <- scale(sample$TotalTests)
sample$Population <- scale(sample$Population)

cor(sample$TotalCases, sample$Population)

reg = lm(TotalCases~Population, sample)
summary(reg)

plot(sample$TotalCases, sample$Population,
     xlab = "확진자 수", 
     ylab = "인구",
     main = "인구 대비 확진자")
abline(reg$coefficients, col = "red")


cor(sample$TotalCases, sample$TotalTests)

test = lm(TotalCases~TotalTests, sample)
summary(test)

plot(sample$TotalCases, sample$TotalTests,
     xlab = "확진자 수", 
     ylab = "검사자 수",
     main = "검사자 대비 확진자")
abline(test$coefficients, col = "red")
