library(rio)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(e1071)

df <- import("startup_data.csv")
view(df)


sum(is.na(df))

df <- na.omit(df)



head(df)
tail(df)

numeric_cols <- df %>%
  select(where(is.numeric)) %>%
  colnames()


summary_df <- data.frame(
  Variable = character(),
  Min = numeric(),
  Max = numeric(),
  Mean = numeric(),
  Median = numeric(),
  SD = numeric(),
  Skewness = numeric(),
  Kurtosis = numeric(),
  stringsAsFactors = FALSE
)



for (var in numeric_cols) {
  x <- df[[var]]
  summary_df <- rbind(summary_df, data.frame(
    Variable = var,
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE)
  ))
}

view(summary_df)

region_counts <- df %>%
  count(Region) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0( round(percentage, 1), "%"))


ggplot(region_counts, aes(x = "", y = n, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  labs(title = "Udział regionów w liczbie założonych startupów") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 3.5)



ggplot(df, aes(x = Industry, fill=Industry))+
  geom_bar()+
  coord_flip()+
  theme_minimal()




numeric_data <- df[sapply(df, is.numeric)]

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix, 
         method="color", 
         type="upper",
         addCoef.col = "black", 
         number.cex = 0.8, 
         tl.cex = 0.8, 
         tl.col = "black")


ggplot(df, aes(x = `Revenue (M USD)`, y = `Valuation (M USD)`))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Valuation vs Revenue",
       x = "Revenue (M USD)", y = "Valuation (M USD)") +
  theme_minimal()



ggplot(df, aes(x = `Funding Amount (M USD)`, y =`Market Share (%)`))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Funding Amount vs Market share",
       x = "Funding Amount (M USD)", y = "Market Share (%)") +
  theme_minimal()

ggplot(df, aes(x = df$`Revenue (M USD)`, y =df$Employees))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Employees vs Revenue",
       x = "Revenue (M USD)", y = "Employees") +
  theme_minimal()


ggplot(df, aes(x = `Funding Amount (M USD)`, y = `Valuation (M USD)`))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Valuation vs Revenue",
       x = "Revenue (M USD)", y = "Valuation (M USD)") +
  theme_minimal()


model1 <- lm(`Valuation (M USD)` ~ `Revenue (M USD)`, data = df)
summary(model1)

par(mfrow = c(2, 2))
plot(model1)

par(mfrow = c(1,1))

df$log_revenue <- log(df$`Revenue (M USD)` + 1)
df$log_valuation <- log(df$`Valuation (M USD)` + 1)

model_log <- lm(log_valuation ~ log_revenue, data = df)
summary(model_log)


model1 <- lm(`Valuation (M USD)` ~ `Revenue (M USD)` + 
               `Funding Amount (M USD)`, data = df)
summary(model1)

model2 <- lm(`Valuation (M USD)` ~ `Revenue (M USD)` + 
               `Funding Amount (M USD)` + 
               Employees + `Market Share (%)`, data = df)
summary(model2)


plot(model1)
plot(model2)



founded_per_year <- df %>%
  count(`Year Founded`)

ggplot(founded_per_year, aes(x= `Year Founded`, y = n))+
  geom_point(color="dark red", size=1)+
  geom_line(color="dark red", size=1)+
  theme_minimal()

ts_startups = ts(founded_per_year$n, 
                      start = min(founded_per_year$`Year Founded`), 
                      frequency = 2)



decomp_add <- decompose(ts_startups, type = "additive")
plot(decomp_add)


decomp_mult <- decompose(ts_startups, type = "multiplicative")
plot(decomp_mult)

df$Profitable <- factor(df$Profitable, labels = c("No", "Yes"))


ggplot(df, aes(x = Profitable, y = `Valuation (M USD)`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Valuation by Profitability",
       x = "Profitable?", y = "Valuation (M USD)") +
  theme_minimal()

t.test(`Valuation (M USD)` ~ Profitable, data = df)

ggplot(df, aes(x = `Exit Status`, y = `Valuation (M USD)`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Valuation by Exit Status",
       x = "Exit Status", y = "Valuation (M USD)") +
  theme_minimal()


anova_result <- aov(`Valuation (M USD)` ~ `Exit Status`, data = df)
summary(anova_result)


