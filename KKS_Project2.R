# Khe Kit Shum
# Project 2
# Drake STAT/CS 190
rm(list =ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)
library(randomForest)
library(boot)

diamonds <- read.csv("D:/Drake Spring 2021/Drake CS190/Project2/diamonds.csv", 
                     header = TRUE, stringsAsFactors = TRUE)

diamonds$X <- NULL 

dim(diamonds)

head(diamonds)

summary(diamonds)

# Histogram of Price
ggplot(data = diamonds,aes(x = price)) +
  geom_histogram(stat="bin", binwidth=500, fill="steelblue") +
  ggtitle("Price of Diamonds") +
  labs(x = "Price $ USD", y = "Frequency") +
  scale_colour_grey("Result") +
  theme_minimal()

diamonds$cut <- factor(diamonds$cut,levels = c("Fair", "Good", "Very Good", "Premium", 
                                                       "Ideal"))

ggplot(data = diamonds,aes(x = price, fill=cut)) +
  geom_histogram(stat="bin", binwidth=500) +
  ggtitle("Price of Diamonds") +
  labs(x = "Price $ USD", y = "Frequency") +
  scale_colour_grey("Result") +
  theme_minimal()
  
# Histogram of weight of Diamond (carat)
ggplot(data = diamonds, aes(x = carat)) +
  geom_histogram(stat="bin", binwidth=0.18, fill="steelblue") +
  ggtitle("Weight of Diamonds") +
  labs(x = "Carat", y = "Frequency") +
  scale_colour_grey("Result") +
  theme_minimal()

# Histogram of Price by Cut
ggplot(data = diamonds, aes(x = price)) +
  geom_histogram(stat="bin", binwidth=500, fill="steelblue") +
  facet_wrap(~cut) +
  ggtitle("Price of Diamonds") +
  labs(x = "Price", y = "Frequency") +
  scale_colour_grey("Result") +
  theme_minimal()

# count for each category for cut
diamonds %>% 
  group_by(cut) %>%
  summarise(counts = n())

# cut quality of the cut (Fair, Good, Very Good, Premium, Ideal)
ggplot(data = diamonds, aes(x = cut)) +
  geom_bar(fill="steelblue") +
  geom_text(stat="count", aes(label=..count..), vjust=1.6, color="white", size=3.5) +
  ggtitle("Quality of Diamonds") +
  labs(x = "Cut", y = "Frequency") +
  scale_colour_grey("Result") +
  theme_minimal()

# color diamond color, from D (best) to J (worst)
ggplot(data = diamonds, aes(x = color)) +
  geom_bar(fill="steelblue") +
  geom_text(stat="count", aes(label=..count..), vjust=1.6, color="white", size=3.5) +
  ggtitle("Colors of Diamonds") +
  labs(x = "Color", y = "Frequency") +
  scale_colour_grey("Result") +
  theme_minimal()

# clarity a measurement of how clear the diamond
# I1 the worst to IF the best
diamonds$clarity <- factor(diamonds$clarity,levels = c("I1", "SI2", "SI1", "VS2", 
                                                       "VS1", "VVS2", "VVS1", "IF"))
ggplot(data = diamonds, aes(x = clarity)) +
  geom_bar(fill="steelblue") +
  geom_text(stat="count", aes(label=..count..), vjust=1.6, color="white", size=3.5) +
  ggtitle("Clarity of Diamonds") +
  labs(x = "Clarity", y = "Frequency") +
  scale_colour_grey("Result") +
  theme_minimal()

# Scatter plot for Carat vs Price and Clarity
ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price, col = clarity)) +
  ggtitle("Carat Vs Price") +
  labs(x = "Carat", y = "Price") +
  scale_color_brewer("Clarity", palette = "Set2") +
  theme_minimal()

# Scatter plot for Carat vs Price and Color
ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price, col = color)) +
  ggtitle("Carat Vs Price") +
  labs(x = "Carat", y = "Price") +
  scale_color_brewer("Color", palette = "Dark2") +
  theme_minimal()

# Scatter plot for Carat vs Price and Cut
ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price, col = cut)) +
  ggtitle("Carat Vs Price") +
  labs(x = "Carat", y = "Price") +
  scale_color_brewer("Cut", palette = "Dark2") +
  theme_minimal()

# Bar graph of quality of Diamonds with proportion of its color
ggplot(data = diamonds) +
  geom_bar(aes(x = cut, fill = color), position = "fill") +
  ggtitle("Quality of Diamonds with Colors") +
  labs(x = "Cut", y = "Proportion") +
  scale_fill_brewer("Colors of Diamond", palette = "Dark2") +
  theme_minimal()

# Violin plot of Price against Color with category Clarity
# the bumping part means more common price
# had to use log scale for price
ggplot(data = diamonds, aes(x=color, y=price)) + 
  geom_violin() + 
  scale_y_log10() + 
  labs(x = "Color", y = "Price in (log)") +
  facet_wrap(~ clarity) +
  theme_minimal()

####################################################################
# Random Forest ########################################
###########################################
diamonds$price_bin <- ifelse(diamonds$price>=3000, "High", "Low")
diamonds$price_bin <- as.factor(diamonds$price_bin)

RNGkind(sample.kind="default")
set.seed(741852)

# Initializes training and testing set for modeling
train.idx <- sample(x=1:nrow(diamonds),size=floor(.8*nrow(diamonds)))
train.df <- diamonds[train.idx,]
test.df <- diamonds[-train.idx,]

# temporary forest
tempforest <- randomForest(price_bin ~ clarity+color+carat+cut+depth+
                             x+y+z+table,
                           data = train.df,
                           ntree = 1000,
                           mtry = 4)

# there are 9 x's variables
mtry <- seq(1, 9, by = 2)

# make from for OOB error, m value
keeps <- data.frame(m = rep(NA, length(mtry)),
                    OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  
  tempforest <- randomForest(price_bin ~ clarity+color+carat+cut+depth+
                               x+y+z+table,
                             data = train.df, 
                             ntree = 1000, 
                             mtry = mtry[idx])
  # record this iteration's m value in idx'th row
  keeps[idx, "m"] <- mtry[idx]
  # record this iteration's oob error in idx'th row
  keeps[idx,"OOB_err_rate"] <- mean(predict(tempforest)!= train.df$price_bin)
}

# Plot:
qplot(m, OOB_err_rate, geom = c("line", "point"), data = keeps) +
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate") +
  scale_x_continuous(breaks = c(1:10))

# final forest
dforest_final <- randomForest(price_bin ~ clarity+color+carat+cut+depth+
                                x+y+z+table,
                            data = train.df,
                            ntree = 1000,
                            mtry = 3,
                            importance = TRUE)

dforest_final

pi_hat <- predict(dforest_final, test.df, type = "prob")[,"High"]
rocCurve <- roc(response = test.df$price_bin,
                predictor = pi_hat,
                levels = c("Low", "High"))

plot(rocCurve,print.thres = TRUE, print.auc=TRUE)

# Adds prediction values from the final forest into the table "diamonds"
diamonds$forest_pred <- predict(dforest_final, diamonds, type="class")

# Plot showing which variables will be most important to include in the final model
varImpPlot(dforest_final, type = 1)

###########################################################
# Modeling #####################################
#######################################

# Based on histogram of price and scatter plot
# will try Normal, Gamma and Inverse Gaussian GLM and compare AIC
# using log link as price is always positive

# Scatter plot for Carat vs Price in Clarity
ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price, col = clarity)) +
  geom_smooth(aes(x = carat, y = price)) +
  facet_wrap(~clarity) +
  ggtitle("Increasing Price with Carat") +
  labs(x = "Carat", y = "Price (US $)") +
  scale_color_brewer("Clarity", palette = "Dark2") +
  theme_minimal()

m1 <- lm(price ~ clarity+color+cut+depth, data = diamonds)
summary(m1)

m2 <- glm(price ~ clarity+color+cut+depth,
           data = diamonds, family = Gamma(link = "log"))
summary(m2)

m3 <- glm(price ~ clarity+color+cut+depth,
           data = diamonds, family = inverse.gaussian(link = "log"))
summary(m3)

m4 <- glm(price ~ clarity+color+cut+depth,
          data = diamonds, family = gaussian(link = "log"))
summary(m4)

AIC(m1);AIC(m2);AIC(m3);AIC(m4)

glm.diag.plots(m3, glm.diag(m3))

summary(m3)
anova(m3)

lr <- -2*(m3$deviance- m3$null.deviance )
df <- m3$df.null - m3$df.residual
1-pchisq(lr,df)

exp(coef(m3))

1-pchisq(m3$deviance, df=m3$df.residual)
