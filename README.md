Startup analysis
================
Nikodem Kijas
2025-06-13

## **Data import and cleaning.**

### We start the anylysis from importing data set using function import() from “rio” package.

``` r
df <- import("startup_data.csv")
```

### We check is there any missing (NA) value in our dataset.

``` r
sum(is.na(df))
```

    ## [1] 0

### We display the first and last rows of dataset.

``` r
knitr::kable(head(df))
```

| Startup Name | Industry | Funding Rounds | Funding Amount (M USD) | Valuation (M USD) | Revenue (M USD) | Employees | Market Share (%) | Profitable | Year Founded | Region | Exit Status |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|
| Startup_1 | IoT | 1 | 101.09 | 844.75 | 67.87 | 1468 | 5.20 | 0 | 2006 | Europe | Private |
| Startup_2 | EdTech | 1 | 247.62 | 3310.83 | 75.65 | 3280 | 8.10 | 1 | 2003 | South America | Private |
| Startup_3 | EdTech | 1 | 109.24 | 1059.37 | 84.21 | 4933 | 2.61 | 1 | 1995 | South America | Private |
| Startup_4 | Gaming | 5 | 10.75 | 101.90 | 47.08 | 1059 | 2.53 | 0 | 2003 | South America | Private |
| Startup_5 | IoT | 4 | 249.28 | 850.11 | 50.25 | 1905 | 4.09 | 0 | 1997 | Europe | Acquired |
| Startup_6 | AI | 5 | 103.89 | 1541.76 | 12.56 | 1462 | 8.96 | 1 | 2004 | Europe | IPO |

``` r
knitr::kable(tail(df))
```

|  | Startup Name | Industry | Funding Rounds | Funding Amount (M USD) | Valuation (M USD) | Revenue (M USD) | Employees | Market Share (%) | Profitable | Year Founded | Region | Exit Status |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|
| 495 | Startup_495 | E-Commerce | 4 | 114.12 | 1503.71 | 79.19 | 4014 | 1.13 | 1 | 2015 | Europe | IPO |
| 496 | Startup_496 | EdTech | 2 | 181.86 | 2378.65 | 59.64 | 3331 | 0.58 | 1 | 1993 | Europe | Private |
| 497 | Startup_497 | AI | 2 | 107.34 | 1394.58 | 10.22 | 2223 | 5.85 | 0 | 2019 | South America | Private |
| 498 | Startup_498 | E-Commerce | 1 | 160.29 | 502.09 | 84.73 | 2222 | 4.32 | 0 | 2019 | Australia | Private |
| 499 | Startup_499 | Gaming | 5 | 234.65 | 2814.52 | 53.16 | 4972 | 5.53 | 0 | 2011 | Europe | Private |
| 500 | Startup_500 | HealthTech | 4 | 211.76 | 2563.17 | 84.19 | 2374 | 5.26 | 0 | 2000 | North America | Private |

## **Descriptive statistics.**

``` r
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
```

``` r
knitr::kable(summary_df)
```

| Variable | Min | Max | Mean | Median | SD | Skewness | Kurtosis |
|:---|---:|---:|---:|---:|---:|---:|---:|
| Funding Rounds | 1.00 | 5.00 | 2.95800 | 3.000 | 1.4409680 | 0.0572110 | -1.3261033 |
| Funding Amount (M USD) | 0.57 | 299.81 | 152.65676 | 156.005 | 86.6837110 | -0.0892263 | -1.1357880 |
| Valuation (M USD) | 2.43 | 4357.49 | 1371.80918 | 1222.580 | 978.2265787 | 0.6885319 | -0.1164603 |
| Revenue (M USD) | 0.12 | 99.71 | 49.32174 | 48.800 | 29.2676050 | -0.0106769 | -1.2428757 |
| Employees | 12.00 | 4984.00 | 2532.09200 | 2496.500 | 1385.4349207 | -0.0488833 | -1.1097968 |
| Market Share (%) | 0.10 | 10.00 | 5.09294 | 5.135 | 2.8076457 | -0.0402071 | -1.1305477 |
| Profitable | 0.00 | 1.00 | 0.43200 | 0.000 | 0.4958505 | 0.2737277 | -1.9289190 |
| Year Founded | 1990.00 | 2022.00 | 2006.04400 | 2006.000 | 9.3471280 | -0.0007858 | -1.1760053 |

### We calculated basic descriptive statistics such as min, max, mean, median, standard deviation, skewness and curtiosis for all numeric variables. The result show us structure and distribution of dataset.

``` r
t.test(`Valuation (M USD)` ~ Profitable, data = df)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  Valuation (M USD) by Profitable
    ## t = -2.0385, df = 430.64, p-value = 0.04211
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -358.435433   -6.534362
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        1292.976        1475.461

### The t-test of Valuation and Profiitable showed that the difference in avereage valuation between porfitable and non profitable is statistically valuable, because p-value is smaller than 0,05. So generally we can say that profitable startups have higher average valuation.

``` r
ggplot(df, aes(x = `Exit Status`, y = `Valuation (M USD)`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Valuation by Exit Status",
       x = "Exit Status", y = "Valuation (M USD)") +
  theme_minimal()
```

![](markdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### This boxplot show us distribution of valuation in different exit status. On the first look we can’t see any significant impact on valuation.

### So we will make more formal statistical test which is Anova test.

``` r
anova_result <- aov(`Valuation (M USD)` ~ `Exit Status`, data = df)
summary(anova_result)
```

    ##                Df    Sum Sq Mean Sq F value Pr(>F)
    ## `Exit Status`   2    779097  389549   0.406  0.666
    ## Residuals     497 476727595  959210

### The Anova test shows that difference of average startup valuation is not statistically significant, because p-value is much greater than 0.05. So we cannot say that exit status has an impact on startup valuation .

## **Visualisation of Variable Distributions.**

``` r
region_counts <- df %>%
  count(Region) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0( round(percentage, 1), "%"))


ggplot(region_counts, aes(x = "", y = n, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  labs(title = "Startup Distribution by Region") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 3.5)
```

![](markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### The pie chart shows the distributions of startups across different regions. Segments shows the percentage of Startups founded in every region.

``` r
ggplot(df, aes(x = Industry, fill=Industry))+
  geom_bar()+
  coord_flip()+
  theme_minimal()
```

![](markdown_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### The bar chart displays the number of startups in each industry.

#### The visualisations from both charts indicate that data set is well prepared and has a variety data .

## **Visualisation of Relatshionships Between Variables.**

``` r
ggplot(df, aes(x = `Revenue (M USD)`, y = `Valuation (M USD)`))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Valuation vs Revenue",
       x = "Revenue (M USD)", y = "Valuation (M USD)") +
  theme_minimal()
```

![](markdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot(df, aes(x = `Funding Amount (M USD)`, y =`Market Share (%)`))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Funding Amount vs Market share",
       x = "Funding Amount (M USD)", y = "Market Share (%)") +
  theme_minimal()
```

![](markdown_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
ggplot(df, aes(x = `Revenue (M USD)`, y = Employees))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Employees vs Revenue",
       x = "Revenue (M USD)", y = "Employees") +
  theme_minimal()
```

![](markdown_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

### The above scatterplots explore a linear relationship between the pairs of variables.

- Revenue vs Valuation
- Funding Amount vs Market share
- Revenue vs Number of Employees

### In each case, the linear regression line added by geom_smooth(method = “lm”) is nearly flat, which suggest that there is no stronger linear relationship between variables.

## **Simple Linear Regression Model.**

``` r
model1 <- lm(`Valuation (M USD)` ~ `Revenue (M USD)`, data = df)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = `Valuation (M USD)` ~ `Revenue (M USD)`, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1451.0  -849.9  -123.9   646.8  2989.3 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       1275.835     85.730  14.882   <2e-16 ***
    ## `Revenue (M USD)`    1.946      1.495   1.301    0.194    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 977.5 on 498 degrees of freedom
    ## Multiple R-squared:  0.003389,   Adjusted R-squared:  0.001388 
    ## F-statistic: 1.694 on 1 and 498 DF,  p-value: 0.1937

``` r
par(mfrow = c(2, 2))
plot(model1)
```

![](markdown_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
par(mfrow = c(1,1))
```

### The above is a simple model of linear regression to predict valuation based on revenue. However an R-squared value is low, so linear relationship is weak. Based on observation, logarithmic transformation was applied.

## **Log Transformed Linear Model**

``` r
df$log_revenue <- log(df$`Revenue (M USD)` + 1)
df$log_valuation <- log(df$`Valuation (M USD)` + 1)

model_log <- lm(log_valuation ~ log_revenue, data = df)
summary(model_log)
```

    ## 
    ## Call:
    ## lm(formula = log_valuation ~ log_revenue, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5116 -0.5172  0.3274  0.7815  1.6875 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.42184    0.21015  30.558   <2e-16 ***
    ## log_revenue  0.10641    0.05616   1.895   0.0587 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.165 on 498 degrees of freedom
    ## Multiple R-squared:  0.007158,   Adjusted R-squared:  0.005164 
    ## F-statistic:  3.59 on 1 and 498 DF,  p-value: 0.0587

## After transformation, a new model was built. The result is better fitted than previous one, what can we see in higher R-squared value and more linear relationship.

## **Multiple Linear Regression Model.**

``` r
model1 <- lm(`Valuation (M USD)` ~ `Revenue (M USD)` + 
               `Funding Amount (M USD)`, data = df)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = `Valuation (M USD)` ~ `Revenue (M USD)` + `Funding Amount (M USD)`, 
    ##     data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1570.62  -336.99    14.86   334.96  1727.88 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              -48.6923    69.0453  -0.705    0.481    
    ## `Revenue (M USD)`          1.0674     0.9086   1.175    0.241    
    ## `Funding Amount (M USD)`   8.9603     0.3068  29.209   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 593.7 on 497 degrees of freedom
    ## Multiple R-squared:  0.6331, Adjusted R-squared:  0.6317 
    ## F-statistic: 428.9 on 2 and 497 DF,  p-value: < 2.2e-16

``` r
model2 <- lm(`Valuation (M USD)` ~ `Revenue (M USD)` + 
               `Funding Amount (M USD)` + 
               Employees + `Market Share (%)`, data = df)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = `Valuation (M USD)` ~ `Revenue (M USD)` + `Funding Amount (M USD)` + 
    ##     Employees + `Market Share (%)`, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1585.18  -344.04     9.01   343.62  1793.50 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              -106.94993   94.06932  -1.137    0.256    
    ## `Revenue (M USD)`           1.10251    0.90632   1.216    0.224    
    ## `Funding Amount (M USD)`    8.93099    0.30680  29.110   <2e-16 ***
    ## Employees                  -0.01472    0.01916  -0.768    0.443    
    ## `Market Share (%)`         19.29498    9.46400   2.039    0.042 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 592.1 on 495 degrees of freedom
    ## Multiple R-squared:  0.6366, Adjusted R-squared:  0.6336 
    ## F-statistic: 216.7 on 4 and 495 DF,  p-value: < 2.2e-16

``` r
plot(model1)
```

![](markdown_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](markdown_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](markdown_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](markdown_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

``` r
plot(model2)
```

![](markdown_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->![](markdown_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->![](markdown_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->![](markdown_files/figure-gfm/unnamed-chunk-14-8.png)<!-- -->

### Compared two linear regression models:

- model1 which includes Revenue and Funding amount
- model2 adds Employees and Market share to model1 \### Both models are
  statistically significant (p-value \< 0,001). The difference between
  R-squared or Residual Standard Error is marginally. Even if the
  improvment is modest, model2 better capture the variation in valuation
  by including more variables.

## **Correlation Analysis**

``` r
df <- df %>% select(-log_revenue, -log_valuation)
numeric_data <- df[sapply(df, is.numeric)]

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix, 
         method="color", 
         type="upper",
         addCoef.col = "black", 
         number.cex = 0.8, 
         tl.cex = 0.8, 
         tl.col = "black")
```

![](markdown_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### A correlation matrix was created for all numeric variables to find potential linear relationship. Almost for each pair of variable the correlation is close to 0, the only strong linear relationship was observed between funding amount and revenue.

``` r
ggplot(df, aes(x = `Funding Amount (M USD)`, y = `Valuation (M USD)`))+
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Funding Amount vs Valuation",
       x = "Funding Amount (M USD)", y = "Valuation (M USD)") +
  theme_minimal()
```

![](markdown_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### The scatter plot above represents relationship between Funding amount and Valuation. Red regression line confirms pattern that valuation increase when funding amount rise.

## **Time Series and Seasonality Analysis.**

``` r
founded_per_year <- df %>%
  count(`Year Founded`)

ggplot(founded_per_year, aes(x= `Year Founded`, y = n))+
  geom_point(color="dark red", size=1)+
  geom_line(color="dark red", size=1)+
  theme_minimal()
```

![](markdown_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### The line plot represent amount of startups founded in each year. The trend appears irregular, without clear pattern or annual cycles.

``` r
ts_startups = ts(founded_per_year$n, 
                      start = min(founded_per_year$`Year Founded`), 
                      frequency = 2)



decomp_add <- decompose(ts_startups, type = "additive")
plot(decomp_add)
```

![](markdown_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
decomp_mult <- decompose(ts_startups, type = "multiplicative")
plot(decomp_mult)
```

![](markdown_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

<span style="color:red">Unfortunately our dataset includes only year of
each startup funding, so we can’t perform a proper seasonal analysis. To
allow decompose to run we were forced to set fake frequency = 2 only to
demonstrate seasonability analysis. We can’t get any valid information
from this chunk.<span/>
