library(readxl)
library(WeightIt)
library(marginaleffects)
library(cobalt)
library(bnlearn)
library(stringr)
library(dplyr)
library(boot)
library(forestmodel)
library(sjPlot)
supply_chain <- read_excel("Desktop/Y4S2/FYP/Possible datasets/archive/SCM_Dataset_Updated_with_Green_Logistics.xlsx")

####Cleaning dataset#####

#Checking class of each variable
sapply(supply_chain, class)

colSums(is.na(supply_chain)) #Checking which columns have NA values
#Environmental Impact score has maximum empty values -> not possible to use in the analysis

index <- match("Environmental Impact Score", names(supply_chain))
supply_chain <- supply_chain[,-9]
supply_chain <- supply_chain[-1000,]
cols_to_factor <- c('Company Name','SCM Practices', 'Technology Utilized', 'Supply Chain Agility', 'Supply Chain Integration Level','Sustainability Practices','Supplier Collaboration Level', 'Supply Chain Complexity Index')
supply_chain[cols_to_factor]<- lapply(supply_chain[cols_to_factor], factor)
supply_chain$`Cost of Goods Sold (COGS)` <- as.numeric(str_extract(supply_chain$`Cost of Goods Sold (COGS)`, "[0-9.]+"))

#IPW VARIABLE ASSIGNMENT - 1
treatment <- ifelse(supply_chain$`Supply Chain Agility`== "High", 1,0)
supply_chain$treatment <- treatment
outcome_1 <- supply_chain$`Revenue Growth Rate out of (15)`
outcome_2 <- supply_chain$`Operational Efficiency Score`
confounder_1 <- supply_chain$`SCM Practices`
confounder_2<- supply_chain$`Supply Chain Integration Level`
confounder_3<- supply_chain$`Supplier Count`
confounder_4<- supply_chain$`Customer Satisfaction (%)`
confounder_5<- supply_chain$`Order Fulfillment Rate (%)`

# Fitting the model using weights from IPW WeightIt - Outcome 1
test <- weightit(treatment~confounder_1+confounder_2+confounder_3+confounder_4+confounder_5, data=supply_chain,estimand="ATE")
fit <- lm(outcome_1~treatment, data = supply_chain, weights=test$weights)
summary(fit)
a1<- avg_comparisons(fit, variables = 'treatment', vcov='HC3', wts = test$weights)
summary(test) 

#Love plot for test : Weightit object that calculates propensity scores
love.plot(test, 
          thresholds = c(m = .1), # Adds a dashed line at 0.1 (the gold standard)
          binary = "std",         # Standardize binary variables
          stas="std",
          abs = TRUE,             # Show absolute distances from zero
          colors = c("red", "blue"), 
          shapes = c("circle", "triangle"),
          sample.names = c("Unweighted", "Weighted"))


#Fitting the model using weights from IPW weightit - Outcome 2
fit_2 <- lm(outcome_2~treatment, data = supply_chain, weights=test$weights)
summary(fit_2)
avg_comparisons(fit_2, variables = 'treatment', vcov='HC3')


#Bootstrap to estimate Standard Error in ATE
set.seed(38)

#ATE function for outcome 1
ate_fun<- function(data,i){
  df<- data[i,]
  test <- weightit(`treatment`~+`SCM Practices`+`Supply Chain Integration Level` +`Supplier Count`+`Customer Satisfaction (%)`+`Order Fulfillment Rate (%)`, data=df,estimand="ATE")
  fit <- lm(`Revenue Growth Rate out of (15)`~treatment, data = df, weights=test$weights)
  return(avg_comparisons(fit, variables='treatment', vcov='HC3')$estimate)
  }
boot(data = supply_chain, statistic = ate_fun, R = 1000)


#ATE function for outcome 2
ate_fun<- function(data,i){
  df<- data[i,]
  test <- weightit(`treatment`~+`SCM Practices`+`Supply Chain Integration Level` +`Supplier Count`+`Customer Satisfaction (%)`+`Order Fulfillment Rate (%)`, data=df,estimand="ATE")
  fit <- lm(`Operational Efficiency Score`~treatment, data = df, weights=test$weights)
  return(avg_comparisons(fit, variables='treatment', vcov='HC3')$estimate)
}
boot(data = supply_chain, statistic = ate_fun, R = 1000)


#Propensity Score- Outcome 1 & 2
avg_propensity_score <- mean(test$ps)
summary(test$ps)
hist(test$ps)

#Outcome Regression
# Fit a single model with no interaction term
#Model 1
or1 <- lm(`Revenue Growth Rate out of (15)` ~ 
           treatment + 
           `SCM Practices` + 
           `Supply Chain Integration Level` + 
           `Supplier Count` + 
           `Customer Satisfaction (%)`+
           `Order Fulfillment Rate (%)`, 
         data = supply_chain)
summary(or1)

#ATE Calculation for Model 1
avg_predictions(or1,variables="treatment")
avg_comparisons(or1, variables="treatment", newdata=supply_chain)

#Fit a model with interactions
#Model 2
or2 <- lm(`Revenue Growth Rate out of (15)` ~ 
           treatment *(`Order Fulfillment Rate (%)`+ 
           `SCM Practices` + 
           `Supply Chain Integration Level` + 
           `Supplier Count` + 
            `Customer Satisfaction (%)`),
          data = supply_chain)

          


#Alternate approach to fit the predictions
'd0<- transform(supply_chain, treatment=0)
d1<- transform(supply_chain, treatment=1)

p0 <- predictions(or, newdata = d0)
p1 <- predictions(or, newdata=d1)'

avg_predictions(or2,variables="treatment")
avg_comparisons(or2, variables="treatment", newdata=supply_chain)

# Fit a single model with no interaction term
#Model 3
or3 <- lm(`Operational Efficiency Score`~ 
           `treatment` + 
           `SCM Practices` + 
           `Supply Chain Integration Level` + 
           `Supplier Count` + 
            `Customer Satisfaction (%)`+
           `Order Fulfillment Rate (%)`, 
         data = supply_chain)

avg_predictions(or3, variables="treatment")
avg_comparisons(or3, variables="treatment", newdata=supply_chain)

#Fit a model with interaction terms
or4 <- lm(`Operational Efficiency Score` ~ 
            treatment*`Supplier Count` + 
            `SCM Practices` + 
            `Supplier Count` +
            `Supply Chain Integration Level` + 
            `Order Fulfillment Rate (%)`, 
          data = supply_chain)

avg_comparisons(or4, variables="treatment", newdata=supply_chain)

#Bootstrap SE for Outcome Regression

#Model 1 Revenue Growth Rate without interaction
ate_fun_or<- function(data,i){
  df<- data[i,]
  or <- lm(`Revenue Growth Rate out of (15)` ~ 
             treatment + 
             `SCM Practices` + 
             `Supply Chain Integration Level` + 
             `Supplier Count` + 
             `Customer Satisfaction (%)`+
             `Order Fulfillment Rate (%)`, 
           data = df)
  ate<- avg_comparisons(or, variables="treatment", newdata=df)
  return (ate$estimate)
}
boot(data=supply_chain, statistic=ate_fun_or,R=1000)

#Model 2 Revenue Growth Rate with interaction
ate_fun_int<- function(data,i){
  df<- data[i,]
  or1 <- lm(`Revenue Growth Rate out of (15)` ~ 
              treatment *`Order Fulfillment Rate (%)`+ 
              `SCM Practices` + 
              `Supply Chain Integration Level` + 
              `Supplier Count` + 
              `Customer Satisfaction (%)`+
              `Order Fulfillment Rate (%)`, 
            data = df)
  ate<- avg_comparisons(or1, variables="treatment", newdata=df)
  return (ate$estimate)
}

boot(data=supply_chain, statistic=ate_fun_int,R=1000)

#Model 3 Operational Efficiency Score without interaction
ate_fun_or_2<- function(data,i){
  df<- data[i,]
  or2 <- lm(`Operational Efficiency Score` ~ 
              `treatment` + 
              `SCM Practices` + 
              `Supply Chain Integration Level` + 
              `Supplier Count` + 
              `Customer Satisfaction (%)`+
              `Order Fulfillment Rate (%)`, 
            data = df)
  ate<- avg_comparisons(or2, variables="treatment", newdata=df)
  return (ate$estimate)
}
boot(data=supply_chain, statistic=ate_fun_or_2,R=1000)


#Model 4 Operational Efficiency Score with interaction
ate_fun_int_2<- function(data,i){
  df<- data[i,]
  or3 <- lm(`Operational Efficiency Score` ~ 
              treatment*`Supplier Count` + 
              `SCM Practices` + 
              `Supplier Count` +
              `Supply Chain Integration Level` + 
              `Order Fulfillment Rate (%)`, 
            data = df)
  ate<- avg_comparisons(or3, variables="treatment", newdata=df)
  return (ate$estimate)
}
boot(data=supply_chain, statistic=ate_fun_int_2,R=1000)

#Plotting the 4 models
plot_model(or1, show.values = TRUE, value.offset = .3) +
  theme_minimal() +
  labs(title = "Determinants of Revenue Growth  Rate- Model 1")

plot_model(or2, show.values = TRUE, value.offset = .3) +
  theme_minimal() +
  labs(title = "Determinants of Revenue Growth  Rate- Model 2")

plot_model(or3, show.values = TRUE, value.offset = .3) +
  theme_minimal() +
  labs(title = "Operational Efficiency Score- Model 3")

plot_model(or4, show.values = TRUE, value.offset = .3) +
  theme_minimal() +
  labs(title = "Operational Efficiency Score- Model 4")


#Bayesian belief network
#Subset from the dataframe
sub_sc <- supply_chain[, (names(supply_chain)) %in% c("Supply Chain Agility", "Supply Chain Integration Level", "Revenue Growth Rate out of (15)", "Operational Efficiency Score", "Supplier Count", "SCM Practice", "Customer Satisfaction (%)", "Order Fulfillment Rate (%)")]
sub_sc<- as.data.frame(sub_sc)
dag_full <- hc(supply_chain)

# Learn the structure automatically
dag <- hc(sub_sc)
adj<- amat(dag)
dag

# Visualize the structure
plot(dag)
# Fit the model 
fitted_bn <- bn.fit(dag, data = sub_sc)
bn.net(fitted_bn)

# Strength using bootstrapping
boot = boot.strength(sub_sc, R = 500, algorithm = "hc")
avg.net = averaged.network(boot, threshold = 0.5)
strength.plot(avg.net, boot)


strength<- arc.strength(dag, data = sub_sc)
strength.plot(dag, strength)

#Verify the relationship infered from the netwrok using bootstrapped average network
boot[boot$from=='Supply Chain Agility',]

#Effect on outcome 1
boot[boot$to=='Revenue Growth Rate out of (15)',]

#Effect on outcome 2
boot[boot$to=='Operational Efficiency Score',]

#ATE Calculation:
parents_A <- bnlearn::parents(dag, "Supply Chain Agility")

bn_do <- dag
for (p in parents_A) {
  bn_do <- drop.arc(bn_do, from = p, to = "Supply Chain Agility")
}

bn_fit_do <- bn.fit(bn_do, sub_sc)

set.seed(123)
samp1 <- cpdist(bn_fit_do,
                nodes = "Revenue Growth Rate out of (15)",
                evidence = (`Supply Chain Agility` == "High"), n=5000)

samp0 <- cpdist(bn_fit_do,
                nodes = "Revenue Growth Rate out of (15)",
                evidence = (`Supply Chain Agility` == "Medium"), n=5000)

# compute expectations
E1 <- mean(samp1$`Revenue Growth Rate out of (15)`)
E0 <- mean(samp0$`Revenue Growth Rate out of (15)`)

ATE <- E1 - E0

#Compute Standard Deviation of ATE
Y1 <- samp1$`Revenue Growth Rate out of (15)`
Y0 <- samp0$`Revenue Growth Rate out of (15)`
n1 <- length(Y1)
n0 <- length(Y0)
SE <- sqrt(var(Y1)/n1 + var(Y0)/n0)

#Compute Standard Deviation of ATE for Operational Efficiency Score
Y2 <- samp_1_1$`Operational Efficiency Score`
Y3 <- samp_0_1$`Operational Efficiency Score`
n2 <- length(Y2)
n3 <- length(Y3)
SE_2 <- sqrt(var(Y2)/n2 + var(Y3)/n3)


#For outcome 2 Operational Efficiency Score
samp_1_1 <- cpdist(bn_fit_do,
                nodes = "Operational Efficiency Score",
                evidence = (`Supply Chain Agility` == "High"), n=5000)

samp_0_1 <- cpdist(bn_fit_do,
                nodes = "Operational Efficiency Score",
                evidence = (`Supply Chain Agility` == "Medium"), n=5000)

# compute expectations
E1_1 <- mean(samp_1_1$`Operational Efficiency Score`)
E0_1 <- mean(samp_0_1$`Operational Efficiency Score`)

ATE_1 <- E1_1 - E0_1


#Bootstrap Standard Error for ATE using bayesian approach
bn_se <- function(data,i){
  set.seed(123)
  df <- data[i,]
  dag_sub<- dag
  parents_A_sub <- bnlearn::parents(dag_sub, "Supply Chain Agility")
  
  bn_do_sub <- dag_sub
  for (p in parents_A_sub) {
    bn_do_sub <- drop.arc(bn_do_sub, from = p, to = "Supply Chain Agility")
  }
  
  bn_fit_do_sub <- bn.fit(bn_do_sub, df)
  
  samp1_sub <- cpdist(bn_fit_do_sub,
                  nodes = "Revenue Growth Rate out of (15)",
                  evidence = (`Supply Chain Agility` == "High"), n=5000)
  
  samp0_sub <- cpdist(bn_fit_do_sub,
                  nodes = "Revenue Growth Rate out of (15)",
                  evidence = (`Supply Chain Agility` == "Medium"), n=5000)
  E1_sub <- mean(samp1_sub$`Revenue Growth Rate out of (15)`)
  E0_sub <- mean(samp0_sub$`Revenue Growth Rate out of (15)`)
  return(E1_sub-E0_sub)
}

boot(data=sub_sc, statistic=bn_se, R=500)


#Bootstrap Standard Error for ATE using bayesian approach
bn_se_2 <- function(data,i){
  set.seed(123)
  df <- data[i,]
  dag_sub<- hc(df)
  parents_A_sub <- bnlearn::parents(dag_sub, "Supply Chain Agility")
  
  bn_do_sub <- dag_sub
  for (p in parents_A_sub) {
    bn_do_sub <- drop.arc(bn_do_sub, from = p, to = "Supply Chain Agility")
  }
  
  bn_fit_do_sub <- bn.fit(bn_do_sub, df)
  
  samp1_sub <- cpdist(bn_fit_do_sub,
                      nodes = "Operational Efficiency Score",
                      evidence = (`Supply Chain Agility` == "High"), n=5000)
  
  samp0_sub <- cpdist(bn_fit_do_sub,
                      nodes = "Operational Efficiency Score",
                      evidence = (`Supply Chain Agility` == "Medium"), n=5000)
  E1_sub <- mean(samp1_sub$`Operational Efficiency Score`)
  E0_sub <- mean(samp0_sub$`Operational Efficiency Score`)
  return(E1_sub-E0_sub)
}

boot(data=sub_sc, statistic=bn_se_2, R=500)


#Forest plot
library(ggplot2)

# Create the dataset
df <- data.frame(
  Outcome = c(rep("Revenue Growth Rate", 4), rep("Operational Efficiency Score", 4)),
  Method = c("IPW", "Regression (No Int)", "Regression (Int)", "BBN",
             "IPW", "Regression (No Int)", "Regression (Int)", "BBN"),
  ATE = c(1.64, 0.24, 1.05, 1.70, 0.59, -0.33, -0.29, 0.28),
  SE = c(0.09, 0.12, 0.14, 0.08, 0.25, 0.44, 0.44, 0.34) # Using the most complete SE column
)

# Calculate 95% Confidence Intervals
df$lower <- df$ATE - (1.96 * df$SE)
df$upper <- df$ATE + (1.96 * df$SE)
ggplot(df, aes(x = Method, y = ATE, color = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Null effect line
  coord_flip() + # Flip to make it look like a standard forest plot
  facet_wrap(~Outcome, scales = "free_x") + # Separate plots for each outcome
  theme_minimal() +
  labs(
    title = "Forest Plot of ATE Estimates",
    subtitle = "Comparing different causal inference methods",
    x = "Estimation Method",
    y = "Average Treatment Effect (ATE)"
  ) +
  theme(legend.position = "none")
