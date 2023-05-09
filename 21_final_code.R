#below we are calling the libraries needed 
library(dplyr)
library(ggplot2)   # for plotting
library(plotly)
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("sjlabelled")
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyr)
#Q1 

#data frame 1 reads the NDAP report number 7063
#Groundwater Level
df_1 <- read.csv("/Users/medhahira/Desktop/econ_project/21_report.csv")
# selecting the desired columns
df_2 <- select(df_1,State,District,Yearcode,Ground.water.level)
#omitting all NAs
df_2 <- na.omit(df_2)

df_3 <- df_2 %>%
  group_by(State, District, Yearcode) %>%
  #taking the mean/average
  mutate(Ground.water.level = mean(Ground.water.level, na.rm = TRUE)) %>%
  ungroup()

# remove duplicates
df_3 <- distinct(df_3)
#Q2

colnames(df_3) <- c("STATE", "districtName", "YEAR", "Ground.water.level")

# create new dataframe with distinct srcDistrictName
df_4 <- distinct(df_3, districtName)

# add new column with unique IDs for each srcDistrictName
df_4 <- df_4 %>%
  mutate(ID = row_number())%>%
  rename(districtID = ID)

# show final dataframe
df_5 <- merge(df_3, df_4, by = "districtName")

df_5 = df_5 %>% mutate(ID = (10000*districtID)+YEAR)

df_5$STATE <- tolower(df_5$STATE)

sdp = read.csv("/Users/medhahira/Desktop/econ_project/21_sdp.csv")
sdp = na.omit(sdp)

sdp$STATE <- tolower(sdp$STATE)

#now reading the district level Gini index
df_7g = read.csv("/Users/medhahira/Desktop/econ_project/21_gini.csv")
df_7g = na.omit(df_7g)

colnames(df_7g) <- c("districtName","ginivalue")

#Q3
#merging 
df_6_sdp = merge(df_5,sdp,by = c("YEAR","STATE"))

#run model over here - regression
df_6_sdp$districtName <- tolower(df_6_sdp$districtName)
df_7g$districtName <- tolower(df_7g$districtName)
#Q4
#merging the dataset with the district level Gini Index, that was stored in df_7g
df_8f = merge(df_6_sdp,df_7g,by = "districtName")
print(colnames(df_8f))
#manipulating the VALUE column to become numeric
df_8f$VALUE = as.numeric(df_8f$VALUE)

#Q5
summary(df_8f)
sd(df_8f$Ground.water.level)
sd(df_8f$ginivalue)
sd(df_8f$VALUE)

#Q6
#running the regressing model
model <- lm(formula = Ground.water.level ~ as.numeric(VALUE), data = df_8f) #Model for Question - 1
summary(model)
#showing results in a tabular manner
tab_model(model)


#Q7
residuals <- resid(model)
#This plots the histogram of residuals
plot_ly(x = residuals, type = "histogram") %>% 
  layout(title = "Histogram of Residuals", 
         xaxis = list(title = "Residuals"), 
         yaxis = list(title = "Frequency"))

#to verify that the sum of residuals tends to zero
sum(residuals)

# check if the summation is equal to zero
sum(residuals) == 0
#summation not strictly equal to zero, but around 10^-12, which is a very small number and it tends to zero

str(df_8f)

#For Finding the Summary in Question - 5
summary(df_8f)

#Q-5 Plotting the Histogram and Box Plot and Distribution Shape Plot
plot_ly(data = df_8f, x = ~Ground.water.level, type = "histogram")
plot_ly(data = df_8f, x = ~VALUE, type = "histogram")
plot_ly(data = df_8f, x = ~ginivalue, type = "histogram")

plot_ly(data = df_8f, y = ~Ground.water.level, type = "box")
plot_ly(data = df_8f, y = ~VALUE, type = "box")
plot_ly(data = df_8f, y = ~ginivalue, type = "box")

plot_ly(data = df_8f, y = ~Ground.water.level, type = "violin")
plot_ly(data = df_8f, y = ~VALUE, type = "violin")
plot_ly(data = df_8f, y = ~ginivalue, type = "violin")


#Finding the Quartile Range, IQR for finding out the Outliers

#For Ground Water Level
q1 <- quantile(df_8f$Ground.water.level, 0.25)
print(q1)
q3 <- quantile(df_8f$Ground.water.level, 0.75)
print(q3)
iqr <- q3 - q1
print(iqr)
print(q3+1.5*iqr)
print(q1 - 1.5*iqr)
outliers1 <- df_8f$Ground.water.level < (q1 - 1.5*iqr) | df_8f$Ground.water.level > (q3 + 1.5*iqr)
sum(outliers1)

#For SDP Value
q11 <- quantile(df_8f$VALUE, 0.25)
print(q11)
q31 <- quantile(df_8f$VALUE, 0.75)
print(q31)
iqr2 <- q31 - q11
print(iqr2)
print(q31+1.5*iqr2)
print(q11 - 1.5*iqr2)
outliers2 <- df_8f$VALUE < (q11 - 1.5*iqr2) | df_8f$VALUE > (q31 + 1.5*iqr2)
sum(outliers2)

#For gini value
q12 <- quantile(df_8f$ginivalue, 0.25)
print(q12)
q32 <- quantile(df_8f$ginivalue, 0.75)
print(q32)
iqr3 <- q32 - q12
print(iqr3)
print(q32+1.5*iqr3)
print(q12 - 1.5*iqr3)
outliers3 <- df_8f$ginivalue < (q12 - 1.5*iqr3) | df_8f$ginivalue > (q32 + 1.5*iqr3)
sum(outliers3)

#finding skewness for Question - 5 Summary
install.packages("moments")
library(moments)

skewness(df_8f$Ground.water.level)
skewness(as.numeric(df_8f$VALUE))
skewness(df_8f$ginivalue)


#Q9 
# fit the regression model
model2 <- lm(Ground.water.level ~ as.numeric(VALUE) + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3) + ginivalue, data = df_8f)

# summarize the results
summary(model2)
tab_model(model2)

#Q7
#Part - A : Trace 1 is Residuals vs Ground.water.level and Trace 2 is SDP Value vs Ground.water.level
scatter_plot <- plot_ly(data = df_8f, x = ~VALUE, y = ~Ground.water.level, type = "scatter",
                        mode = "markers", name="Ground Water Level") %>%
  add_trace(y = residuals, type = "scatter", mode = "markers", marker = list(color = "red"), 
            name="Model Residuals")
scatter_plot <- scatter_plot %>% layout(title = 'SDP VS GWL', xaxis = list(title = 'SDP'), yaxis = list(title = 'GWL'), margin = list(l = 50, r = 50, t = 50, b = 50))
scatter_plot %>% show()

#Part - B : Scatter plot of residuals vs SDP values
scatter_plot1 <- plot_ly(data = df_8f, x = ~VALUE, y = ~residuals, type = 'scatter', mode = 'markers')
scatter_plot1 <- scatter_plot1 %>% layout(title = 'Residuals vs SDP Value', xaxis = list(title = 'SDP Value'), yaxis = list(title = 'Residuals'), margin = list(l = 50, r = 50, t = 50, b = 50))
scatter_plot1 <- scatter_plot1 %>% layout(title = 'Residuals vs SDP Value', xaxis = list(title = 'SDP Value'), yaxis = list(title = 'Residuals'), margin = list(l = 50, r = 50, t = 50, b = 50))
scatter_plot1 %>% show()



# generate predictions on the test data
pred <- predict(model, newdata = df_8f)

# create a dataframe with the predicted and true values
plot_data <- data.frame(Predicted = pred, True = df_8f$Ground.water.level)

# create a vector of colors
# create a factor variable for the "True" and "Predicted" groups
plot_data$Group <- factor(c(rep("True", nrow(plot_data)/2), rep("Predicted", nrow(plot_data)/2)))


#Part C : Scatter plot of predicted vs True Ground.water.level
# create the scatter plot with color argument set to Group factor variable
scatter_plot <- plot_ly(data = plot_data, x = ~True, y = ~Predicted, type = 'scatter', mode = 'markers', color = ~Group) %>%
  layout(title = 'Predicted vs True Ground.water.level', xaxis = list(title = 'Ground.water.level'), yaxis = list(title = 'Predicted'), margin = list(l = 50, r = 50, t = 50, b = 50), legend = list(title = 'Data', traceorder = 'normal'))

# show the plot
scatter_plot %>% show()
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Part - 1 # --------------------------------------------------
# Extra Variables added - Effective Number of Political Parties, VoteShare
df_f1 = read.csv("/Users/medhahira/Desktop/econ_project/vote_share.csv")
df_f1 = na.omit(df_f1)
df_f1 <- df_f1 %>% rename(STATE = state)
df_f1 <- df_f1 %>% rename(districtName = district)

df_m1 <- merge(df_f1, df_8f, by = c("STATE", "districtName"))
df_m1 <- na.omit(df_m1)

df_f2 = read.csv("/Users/medhahira/Desktop/econ_project/enop.csv")
df_f2 <- df_f2 %>% rename(enop = effective_number_of_political_parties)
df_f2 = na.omit(df_f2)
df_f2 <- df_f2 %>% rename(STATE = state)
df_f2 <- df_f2 %>% rename(districtName = district)
df_m2 <- merge(df_f2,df_m1, by = c("STATE","districtName"))

df_f3 = read.csv("/Users/medhahira/Desktop/econ_project/avgmonthlysalary.csv")
df_f3 = na.omit(df_f3)
df_f3 <- df_f3 %>% rename(STATE = statename)
df_f3$STATE <- tolower(df_f3$STATE)
df_m3 <- merge(df_f3,df_m2, by = c("STATE"))
df_m3 = na.omit(df_m3)
df_m4 <- select(df_m3,STATE,districtName,YEAR,Ground.water.level,ID,VALUE,ginivalue,vote_share,enop,avgmonthlysalary)

df_f4 = read.csv("/Users/medhahira/Desktop/econ_project/women_rep.csv")
df_f4 = na.omit(df_f4)
df_f4 <- df_f4 %>% rename(STATE = statename)
df_f4$STATE <- tolower(df_f4$STATE)
df_m5 <- merge(df_m4,df_f4, by = c("STATE"))
df_m5 = na.omit(df_m5)

df_f5 = read.csv("/Users/medhahira/Desktop/econ_project/literacy_rate.csv")
df_f5 = na.omit(df_f5)
df_f5$STATE <- tolower(df_f5$STATE)
df_m6 <- merge(df_m5,df_f5, by = c("STATE"))

model3 <- lm(Ground.water.level ~ as.numeric(VALUE) + ginivalue + vote_share + enop + avgmonthlysalary + women_rep + litrate + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3), data = df_m6)
summary(model3)

df_f6 = read.csv("/Users/medhahira/Desktop/econ_project/StateWise-Gini.csv")
#Part - 2 # ------------------------------------3
#Now the Next Target is to
df_f6 <- df_f6 %>% rename(stateginivalue = Gini.Coff.2015.16.)
df_m6_temp <- select(df_m6,STATE,districtName,YEAR,Ground.water.level,ID,VALUE,vote_share,enop,avgmonthlysalary,women_rep,litrate)
df_m6_state = merge(df_m6_temp,df_f6,by = c("STATE"))
df_m6_state <- select(df_m6_state,STATE,districtName,YEAR,Ground.water.level,ID,VALUE,stateginivalue,vote_share,enop,avgmonthlysalary,women_rep,litrate)
model4 <- lm(Ground.water.level ~ as.numeric(VALUE) + stateginivalue + vote_share + enop + avgmonthlysalary + women_rep + litrate + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3), data = df_m6_state)
summary(model4)

separate.data <- function(df, type) {
  states <- list(
    north = c("arunachal pradesh", "haryana", "himachal pradesh", "punjab", "uttar pradesh", "uttarakhand", "bihar", "delhi"),
    south = c("andhra pradesh", "karnataka", "kerala", "telangana", "tamil nadu"),
    east = c("meghalaya", "nagaland", "tripura", "west bengal", "assam"),
    west = c("gujarat", "maharashtra", "rajasthan", "goa"),
    central = c("madhya pradesh", "chhattisgarh", "jharkhand")
  )
  
  df_state <- df[df$STATE %in% states[[type]], ]
  df_rest <- df[!(df$STATE %in% states[[type]]), ]
  
  return(list(state = df_state, rest = df_rest))
}

chow.test <- function(df.state, df.rest, ST, alpha=0.05) {
  formula <- formula(Ground.water.level ~ VALUE + ginivalue + vote_share + enop + avgmonthlysalary + women_rep + litrate + VALUE^2 + VALUE^3)
  models <- lapply(list(df.state, df.rest), function(df) lm(formula, data = df))
  deviances <- sapply(models, deviance)
  
  k <- 10
  df2 <- nrow(df.state) + nrow(df.rest) - (2 * k)
  
  chow <- ((ST - sum(deviances)) / k) / (sum(deviances) / df2)
  crit.val <- qf(c(alpha/2, 1 - alpha/2), k, df2, lower.tail = FALSE)
  
  if (chow > crit.val[1] & chow < crit.val[2]) {
    cat("Fail to reject the null hypothesis at alpha = ", alpha, ", no structural break present.\n")
  } else {
    cat("Reject the null hypothesis at alpha = ", alpha, ", structural break present.\n")
  }
}


# Separate data into different regions
print(nrow(df_m6))

temp=separate.data(df_m6, "north")
df.north=temp$state
df.north.rest=temp$rest
print(nrow(df.north))

print(nrow(df_m6))

temp=separate.data(df_m6, "south")
df.south=temp$state
df.south.rest=temp$rest
print(nrow(df.south))

temp=separate.data(df_m6, "east")
df.east=temp$state
df.east.rest=temp$rest
print(nrow(df.east))

temp=separate.data(df_m6, "west")
df.west=temp$state
df.west.rest=temp$rest
print(nrow(df.west))

temp=separate.data(df_m6, "central")
df.central=temp$state
df.central.rest=temp$rest
print(nrow(df.central))

ST.q1=deviance(model3)
print(ST.q1)

print("CHOW TEST : NORTHERN STATES")
chow.test(df.north, df.north.rest, ST.q1)
print("CHOW TEST : SOUTHERN STATES")
chow.test(df.south, df.south.rest, ST.q1)
print("CHOW TEST : EASTERN STATES")
chow.test(df.east, df.east.rest, ST.q1)
print("CHOW TEST : WESTERN STATES")
chow.test(df.west, df.west.rest, ST.q1)
print("CHOW TEST : CENTRAL STATES")
chow.test(df.central, df.central.rest, ST.q1)

#Part - 5 ------

install.packages("pacman")
library(pacman) # wraps library and package related functions to improve workflow 
pacman::p_load(data.table, fixest, stargazer, dplyr) 

#running the SLRM to obtain true population parameter
model <- lm(formula = Ground.water.level ~ as.numeric(VALUE), data = df_8f) #Model for Question - 1
summary(model)
#showing results in a tabular manner
tab_model(model)

#MONTE CARLO
set.seed(1)
alpha_0 = 7.740e+00
alpha_1 = 1.976e-06
X = df_8f$VALUE
n <- length(X)
alpha0_hat <- numeric(n)
alpha1_hat <- numeric(n)

# Loop over the number of simulations
for (i in 1:n) {
  # Generate the error term
  U <- rnorm(n, mean = 0, sd = 2)
  
  # Generate the dependent variable
  Y <- alpha_0 + alpha_1 * X + U
  
  # Estimate the parameters using OLS
  fit <- lm(Y ~ X)
  alpha0_hat[i] <- coef(fit)[1]
  alpha1_hat[i] <- coef(fit)[2]
}

# Calculate the mean and standard deviation of the parameter estimates
mean_alpha0 <- mean(alpha0_hat)
mean_alpha1 <- mean(alpha1_hat)
sd_alpha0 <- sd(alpha0_hat)
sd_alpha1 <- sd(alpha1_hat)

# Print the results
cat("True values:\n")
cat("alpha0 =", alpha_0, "\n")
cat("alpha1 =", alpha_1, "\n")
cat("\n")
cat("Estimated values:\n")
cat("alpha0_hat =", mean_alpha0, "(", sd_alpha0, ")\n")
cat("alpha1_hat =", mean_alpha1, "(", sd_alpha1, ")\n")
hist(alpha0_hat, main = "Distribution of intercept estimates", xlab = "Intercept")
abline(v = alpha_0, col = "red")
hist(alpha1_hat, main = "Distribution of slope coefficient estimates", xlab = "Slope coefficient")
abline(v = alpha_1, col = "red")

#ANOVA TABLE
library(broom)
library(knitr)
summary(model)
model5 <- lm(Ground.water.level ~ as.numeric(VALUE) + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3) + ginivalue + women_rep + enop + vote_share + avgmonthlysalary + litrate + north + south + west + center, data = df_m6)
tab_model(model5)
my_anova <- aov(model5)
anova_tidy <- tidy(my_anova)
knitr::kable(anova_tidy,format = "latex")

library(ggplot2)

# Create a data frame with the observed values of the independent variable, VALUE
value_df <- data.frame(VALUE = seq(min(df_m6$VALUE), max(df_m6$VALUE), length.out = 2610))
View(df_m6)
# Create a new data frame with all of the variables used in the model, including ginivalue
newdata <- data.frame(VALUE = value_df$VALUE, 
                      ginivalue = df_m6$ginivalue, 
                      women_rep = df_m6$women_rep, 
                      enop = df_m6$enop, 
                      vote_share = df_m6$vote_share, 
                      avgmonthlysalary = df_m6$avgmonthlysalary, 
                      litrate = df_m6$litrate, 
                      north = df_m6$north, 
                      south = df_m6$south, 
                      west = df_m6$west, 
                      center = df_m6$center)




##part 6 
#MLE
df_m6$south <- ifelse(df_m6$STATE %in% c("andhra pradesh", "karnataka", "kerala", "tamil nadu", "telangana"), 1, 0)
df_m6$north <- ifelse(df_m6$STATE %in% c("delhi", "haryana", "himachal pradesh", "jammu and kashmir", "punjab", "rajasthan", "uttar pradesh", "uttarakhand"),1,0)
df_m6$west <- ifelse(df_m6$STATE %in% c('goa', 'gujarat', 'maharashtra', 'rajasthan'),1,0)
df_m6$center <- ifelse(df_m6$STATE %in% c('madhya pradesh', 'uttar pradesh', 'chhattisgarh', 'jharkhand'),1,0)
model5 <- lm(Ground.water.level ~ as.numeric(VALUE) + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3) + ginivalue + women_rep + enop + vote_share + avgmonthlysalary + litrate + north + south + west + center, data = df_m6)
model_mle_poisson <- glm(Ground.water.level ~ as.numeric(VALUE) + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3) + ginivalue + women_rep + enop + vote_share + avgmonthlysalary + litrate + north + south + west + center, data = df_m6, family = poisson)
model_mle_gauss <- glm(Ground.water.level ~ as.numeric(VALUE) + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3) + ginivalue + women_rep + enop + vote_share + avgmonthlysalary + litrate + north + south + west + center, data = df_m6, family = gaussian)
# Print the summary of the model to view the estimated coefficients and goodness of fit measures
summary(model_mle_poisson)
summary(model_mle_gauss)
tab_model(model_mle_gauss)
tab_model(model_mle_poisson)

model_mle_gauss <- glm(Ground.water.level ~ as.numeric(VALUE) + I(as.numeric(VALUE)^2) + I(as.numeric(VALUE)^3) + ginivalue + women_rep + enop + vote_share + avgmonthlysalary + litrate + north + south + west + center, data = df_m6, family = chisq.test())

# Use the predict function to calculate the predicted values of Ground.water.level for the observed values of VALUE
pred_df <- data.frame(VALUE = value_df$VALUE, 
                      Ground.water.level = predict(model3, newdata = newdata))
# Create the scatterplot with a regression line
ggplot(data = df_m6, aes(x = VALUE, y = Ground.water.level)) +
  geom_point() +
  geom_line(data = pred_df, aes(x = VALUE, y = Ground.water.level), color = "blue") +
  labs(x = "VALUE", y = "Ground.water.level") +
  ggtitle("Scatterplot with regression line")

library(lmtest)

# Perform the Breusch-Pagan test
bptest(model5)
library(sandwich)
# Calculate coefficients with HCSE
coeftest(model5, vcov = vcovHC(model5))
tidy(model5, vcov = vcovHC(model5))
coeff_table <- tidy(model5, vcov = vcovHC(model5))
knitr::kable(coeff_table, format = "latex")

# Calculate the Pearson correlation coefficient
correlation <- cor(df_m6$women_rep, df_m6$Ground.water.level, method = "pearson")

# Calculate the degrees of freedom
df <- length(df_m6$women_rep) - 2
# Perform the hypothesis test
summary(model5)

# Extract the p-value for women_rep variable
p_value <- summary(model5)$coefficients["women_rep", "Pr(>|t|)"]
p_value
# Set the level of significance
alpha <- 0.05

# Print the results
if(p_value < alpha) {
  cat("Reject null hypothesis. There is a significant positive relationship between women's representation in politics and groundwater level, after controlling for other variables.")
} else {
  cat("Fail to reject null hypothesis. There is no significant relationship between women's representation in politics and groundwater level, after controlling for other variables.")
}


####part 7a#####
variances <- sapply(list(df.north, df.south, df.east, df.west, df.central), function(df) sd(df$Ground.water.level)^2)
names(variances) <- c("North States", "South States", "East States", "West States", "Central States")
print(variances)

##T test
coeff <- coef(summary(model5))["south", "Estimate"]
std_error <- coef(summary(model5))["south", "Std. Error"]
t_value <- coeff/std_error

cat("T Value: ", t_value)
# Extract the values of the "south" variable from your dataset
south_values <- model5$model$south
# Perform a t-test on the "south" variable
t_test <- t.test(south_values, alternative = "two.sided", mu = 0, conf.level = 0.95)
# Print the results of the t-test
print(t_test)