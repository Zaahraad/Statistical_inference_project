############Project Phase2#############
library(ggplot2)

data <- read.csv("HealthCare.csv")
View(data)

############Question 1################
levels(factor(data$work_type))
levels(factor(data$Residence_type))
two_catg_table <- table(data$Residence_type, data$work_type)



#A

df_a <- data[, c(7,8)]

compute_CI <- function(P_hat1, P_hat2){
  
  n1 <- 2514
  n2<- 2596
  
  SE <- sqrt(((P_hat1 * (1 - P_hat1)) / n9) + ((P_hat2 * (1 - P_hat2)) / n10))
  left <- (P_hat1 - P_hat2) - (-qnorm(0.025) * SE)
  right <- (P_hat1 - P_hat2) + (-qnorm(0.025) * SE)
  
  return(c(left, right))
  
}


#Self_employed
df_a$Self_employed <- ifelse(df_a$work_type == "Self-employed", 'y','n')

View(df_a)

tableSelfEmployed <- addmargins(table(df_a$Self_employed, df_a$Residence_type))

P_hat_1 <- tableSelfEmployed[2, "Rural"]/tableSelfEmployed[3, "Rural"]
P_hat_2 <- tableSelfEmployed[2, "Urban"]/tableSelfEmployed[3, "Urban"]

compute_CI(P_hat_2, P_hat_1)


#Private
df_a$Private <- ifelse(df_a$work_type == "Private", 'y','n')
View(df_a)

tablePrivate <- addmargins(table(df_a$Private, df_a$Residence_type))

P_hat_3 <- tablePrivate[2, "Rural"]/tablePrivate[3, "Rural"]
P_hat_4 <- tablePrivate[2, "Urban"]/tablePrivate[3, "Urban"]

compute_CI(P_hat_3, P_hat_4)

#Never_worked
df_a$Never_worked <- ifelse(df_a$work_type == "Never_worked", 'y','n')
View(df_a)

tableNever_worked <- addmargins(table(df_a$Never_worked, df_a$Residence_type))

P_hat_5 <- tableNever_worked[2, "Rural"]/tableNever_worked[3, "Rural"]
P_hat_6 <- tableNever_worked[2, "Urban"]/tableNever_worked[3, "Urban"]

compute_CI(P_hat_6, P_hat_5)


#Children
df_a$Children <- ifelse(df_a$work_type == "children", 'y','n')
View(df_a)

tableChildren <- addmargins(table(df_a$Children, df_a$Residence_type))

P_hat_7 <- tableChildren[2, "Rural"]/tableChildren[3, "Rural"]
P_hat_8 <- tableChildren[2, "Urban"]/tableChildren[3, "Urban"]

compute_CI(P_hat_7, P_hat_8)


#Govt_job
df_a$Govt_job <- ifelse(df_a$work_type == "Govt_job", 'y','n')
View(df_a)

tableGovt_job <- addmargins(table(df_a$Govt_job, df_a$Residence_type))

P_hat_9 <- tableGovt_job[2, "Rural"]/tableGovt_job[3, "Rural"]
P_hat_10 <- tableGovt_job[2, "Urban"]/tableGovt_job[3, "Urban"]

compute_CI(P_hat_10, P_hat_9)


#B

addmargins(two_catg_table)
chisq_indp <- chisq.test(two_catg_table)

chisq_indp$expected









############Question 2################
set.seed(123)

small_sample <- data.frame(data[sample(nrow(data), 15),c(4)])
colnames(small_sample) <- c("hypertension")
prop_hat<- mean(small_sample == 1)
p_value <- choose(nrow(small_sample), 2) * ((0.5)^14) * ((0.5) ^ 1)

ggplot(as.data.frame(table(small_sample)), aes(x = small_sample, y = Freq, fill =  small_sample)) +
  geom_bar(stat = "identity") + 
  labs(x = "hypertension", y = "count", fill = "hypertension")+
  scale_y_continuous(expand = c(0, 0))+
  coord_cartesian(ylim = c(0, 15)) +
  ggtitle("barplot for hypertension")+
  theme(
    axis.line.x = element_line(size = 1.5, linetype = "solid", colour = "gray"),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 15, hjust = 0.5)
  )



#simulation
prop_list <- list()

for (i in 1:10000){
  my.sample <- sample( x=c("1", "0"), size=15, replace=TRUE)
  prop_list[[i]] <- mean(my.sample == "1")
}

prop_df <- data.frame(matrix(unlist(prop_list), nrow=10000, byrow=TRUE),stringsAsFactors=FALSE)
colnames(prop_df) <- c('proportion')

ggplot(prop_df,aes(x = proportion)) +
  geom_histogram(bins = 30, fill = "palevioletred3", color = "palevioletred4")+
  ggtitle("Randomization distribution")+
  scale_y_continuous(expand = c(0, 0))+
  coord_cartesian(ylim = c(0, 2200)) +
  theme(
    panel.grid = element_line(color = "gray91"),
    axis.line.x = element_line(size = 1.5, linetype = "solid", colour = "gray"),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 15, hjust = 0.5)
  )

p_val_sim <- mean(prop_df$proportion <= prop_hat)
cat("p-value is: ", p_val_sim)










############Question 3################

#A
work_table <- round(prop.table(table(data$work_type))*100,2)

#set.seed(1234)
sample_1 <-data.frame(data[sample(nrow(data), 100),c(7)])
colnames(sample_1) <- c("work_type")
table(sample_1)

children <- data[data$work_type == "children", c(1,7)]
Private <- data[data$work_type == "Private", c(1,7)]
Govt_job <- data[data$work_type == "Govt_job", c(1,7)]
Self_employed <- data[data$work_type == "Self-employed", c(1,7)]

Never_worked <- data.frame(data[data$work_type == "Never_worked", c(7)])
colnames(Never_worked) <- c("work_type")

b <- data.frame(children[sample(nrow(children), 35),c(2)])
colnames(b) <- c("work_type")

c <- data.frame(Private[sample(nrow(Private), 30),c(2)])
colnames(c) <- c("work_type")

d <- data.frame(Govt_job[sample(nrow(Govt_job), 5),c(2)])
colnames(d) <- c("work_type")

e <- data.frame(Self_employed[sample(nrow(Self_employed), 8),c(2)])
colnames(e) <- c("work_type")

total_sample <- rbind(Never_worked,b,c,d,e)
table(total_sample)

#chi_square test with considering sample size condition for unbiased sample
count_1 <- c(11, 13, 62, 14)
chi_test_sample_1 <- chisq.test(count_1 , p = c(0.1387, 0.1286, 0.5724, 0.1603))
chi_test_sample_1$expected

#chi_square test without considering sample size condition for unbiased sample
count_2 <- c(10, 13, 1, 62, 14)
chi_test_sample_2 <- chisq.test(count_2 , p = c(0.1344, 0.1286,0.0043, 0.5724, 0.1603))
chi_test_sample_2$expected

#chi_square test without considering sample size condition for biased sample
count_3 <- c(35, 5, 22, 30, 8)
chi_test_sample_3  <- chisq.test(count_3 , p = c(0.1344, 0.1286,0.0043, 0.5724, 0.1603))
chi_test_sample_3$expected

#chi_square test with considering sample size condition for biased sample
count_4 <- c(40, 22, 30, 8)
chi_test_sample_4  <- chisq.test(count_4 , p = c(0.1387, 0.1286, 0.5724, 0.1603))




#B
#sol1
df <- data
df$gender[df$gender == "Other"]  <- "Female"

gender_work_table <- table(df$gender, df$work_type)

indep_test <- chisq.test(df$gender, df$work_type)

#sol2
df2 <- data
df2$gender[df2$gender == "Other"]  <- "Male"

gender_work_table_2 <- table(df2$gender, df2$work_type)

indep_test2 <- chisq.test(df2$gender, df2$work_type)











############Question 4################
num_no_miss_df <- data[rowSums(is.na(data)) == 0, c(13, 3, 10)]

# A)
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


upper.panel<-function(x, y){
  
  points(x,y, pch = 19, col = "#C71585")
}


pairs(num_no_miss_df,
      lower.panel = panel.cor,
      upper.panel = upper.panel
      )


# B)

#least squares regression for bmi
linReg_model_bmi <- lm(health_bills ~ bmi, data = num_no_miss_df)
bmi_reg <- summary(linReg_model_bmi)

#nearly normal residuals for bmi
hist(linReg_model_bmi$residuals, col="lightseagreen")

qqnorm(linReg_model_bmi$residuals, col="lightseagreen")
qqline(linReg_model_bmi$residuals)


#constant variability of residuals for bmi
plot(linReg_model_bmi$residuals~ linReg_model_bmi$fitted.values, col = "lightseagreen")
abline(h = 0, lty = 4, col="mediumvioletred")



#least squares regression for age
linReg_model_age <- lm(health_bills ~ age, data = num_no_miss_df)
age_reg <-summary(linReg_model_age)

#nearly normal residuals for age
hist(linReg_model_age$residuals, col="lightseagreen")

qqnorm(linReg_model_age$residuals, col="lightseagreen")
qqline(linReg_model_age$residuals)


#constant variability of residuals for age
plot(linReg_model_age$residuals~ linReg_model_age$fitted.values, col = "lightseagreen")
abline(h = 0, lty = 4, col="mediumvioletred")



#B
library(ggplot2)

#Scatter plot for bmi
ggplot(num_no_miss_df , aes(x = bmi, y = health_bills) ) +
  geom_point(color = 'aquamarine3') +
  geom_abline(slope = bmi_reg$coefficients[[2]], 
              intercept = bmi_reg$coefficients[[1]],
              color = 'deeppink3', lwd = 1.3)+
  ggtitle("scatter plot and least-squares line for 'bmi' explanatory variabale")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
  )


#Scatter plot for age
ggplot(num_no_miss_df , aes(x = age, y = health_bills) ) +
  geom_point(color = 'aquamarine3') +
  geom_abline(slope = age_reg$coefficients[[2]], 
              intercept = age_reg$coefficients[[1]],
              color = 'deeppink3', lwd = 1.3)+
  ggtitle("scatter plot and least-squares line for 'age' explanatory variabale")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
  )



#C
age_reg$coefficients[8]
bmi_reg$coefficients[8]

#D
#adjusted R squared
bmi_reg$adj.r.squared
age_reg$adj.r.squared

#ANOVA
anova(linReg_model_bmi, linReg_model_age)

#bmi
bmi_aov <- anova(linReg_model_bmi)


#age
age_aov <- anova(linReg_model_age)




#E
model_final <- lm(health_bills ~ bmi + age , data = num_no_miss_df)
summary(model_final)



#F
set.seed(42)
samp_1 <- num_no_miss_df[sample(nrow(num_no_miss_df), 100), ]

#a
df_90percent <- samp_1[sample.int(n = nrow(samp_1), size = floor(0.9*nrow(samp_1)), replace = F),]

#Scatter plot for bmi
ggplot(samp_1, aes(x = bmi, y = health_bills) ) +
  geom_point(color = 'purple') +
  ggtitle("scatter plot for bmi and health_bills")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
  )


#Scatter plot for age
ggplot(samp_1, aes(x = age, y = health_bills) ) +
  geom_point(color = 'purple') +
  ggtitle("scatter plot for age and health_bills")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
  )


#linear regression with bmi
model_1 <- lm(health_bills ~ bmi, data = df_90percent)
smry_model_1 <- summary(model_1)

#nearly normal residuals for bmi
hist(model_1$residuals, col="lightseagreen")

qqnorm(model_1$residuals, col="lightseagreen")
qqline(model_1$residuals)


#constant variability of residuals for bmi
plot(model_1$residuals~ model_1$fitted.values, col = "lightseagreen")
abline(h = 0, lty = 4, col="mediumvioletred")


#linear regression with age
model_2 <- lm(health_bills ~ age, data = df_90percent)
smry_model_2 <- summary(model_2)


#nearly normal residuals for age
hist(model_2$residuals, col="lightseagreen")

qqnorm(model_2$residuals, col="lightseagreen")
qqline(model_2$residuals)


#constant variability of residuals for age
plot(model_2$residuals~ model_2$fitted.values, col = "lightseagreen")
abline(h = 0, lty = 4, col="mediumvioletred")



#b
confint(model_1,"bmi", level=0.95)
confint(model_2,"age", level=0.95)


#c
set.seed(12345)
test_data <- samp_1[-sample.int(n = nrow(samp_1), size = floor(0.9*nrow(samp_1)), replace = F),]

y_pred_1 <- predict(model_1, test_data, type="response")
y_pred_2 <- predict(model_2, test_data, type="response")



#d
library(Metrics)
#success rate for first model
actuals_preds_1 <- data.frame(cbind(actuals=test_data$health_bills, predicteds=y_pred_1))

ratio <- list()
for(i in 1:10){
  ratio[i] <- abs(actuals_preds_1$actuals[i] - actuals_preds_1$predicteds[i])
}

ratio <- data.frame(matrix(unlist(ratio), nrow=10, byrow=TRUE),stringsAsFactors=FALSE)
colnames(ratio) <- c("ratio")
actuals_preds_1$label <- ratio$ratio
actuals_preds_1$new <- ifelse((actuals_preds_1$label) <380, '1','0')
success_rate <- length(actuals_preds_1$new[actuals_preds_1$new == 1]) / 10

cor(actuals_preds_1)
mse(actuals_preds_1$actuals, actuals_preds_1$predicteds)
mae(actuals_preds_1$actuals, actuals_preds_1$predicteds)
mape(actuals_preds_1$actuals, actuals_preds_1$predicteds)
rmse(actuals_preds_1$actuals, actuals_preds_1$predicteds)
min_max_accuracy <- mean(apply(actuals_preds_1, 1, min) / apply(actuals_preds_1, 1, max))



#success rate for second model
actuals_preds_2 <- data.frame(cbind(actuals=test_data$health_bills, predicteds=y_pred_2))

ratio2 <- list()
for(i in 1:10){
  ratio2[i] <- abs(actuals_preds_2$actuals[i]-actuals_preds_2$predicteds[i])
}

ratio2 <- data.frame(matrix(unlist(ratio2), nrow=10, byrow=TRUE),stringsAsFactors=FALSE)
colnames(ratio2) <- c("ratio")
actuals_preds_2$label <- ratio$ratio
actuals_preds_2$new <- ifelse((actuals_preds_2$label) <380, '1','0')
success_rate2 <- length(actuals_preds_2$new[actuals_preds_2$new == 1]) / 10


cor(actuals_preds_2)
mse(actuals_preds_2$actuals, actuals_preds_2$predicteds)
mae(actuals_preds_2$actuals, actuals_preds_2$predicteds)
mape(actuals_preds_2$actuals, actuals_preds_2$predicteds)
rmse(actuals_preds_2$actuals, actuals_preds_2$predicteds)
min_max_accuracy_2 <- mean(apply(actuals_preds_2, 1, min) / apply(actuals_preds_2, 1, max))









############Question 5################
library(dplyr)
non_miss_df <- data[rowSums(is.na(data)) == 0, ]
str(non_miss_df)

#A
MakeNum <- function(x) as.numeric(as.factor(x))

non_miss_df <- mutate(non_miss_df, across(c(2,6,7,8,11), MakeNum))
str(non_miss_df)


upper.panel_2<-function(x, y){
  
  points(x,y, pch = 14, col = "lightseagreen")
}

panel.cor2 <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0( r)
  cex.cor <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(non_miss_df[c(13,2,3,4,5,6,7,8,9,10,11, 12)],
      lower.panel = panel.cor2,
      upper.panel = upper.panel_2
)
pairs(non_miss_df[c(13,3,10, 12)],
      lower.panel = panel.cor2,
      upper.panel = upper.panel_2
)

#B
new_df <- data[rowSums(is.na(data)) == 0, ]
new_df$stroke <- as.factor(new_df$stroke)
multi_model <- lm(health_bills ~ stroke + bmi + age, data = new_df)
multi_model_sumry <- summary(multi_model)


#C
multi_model_sumry$r.squared
multi_model_sumry$adj.r.squared


#D
ggplot(data=new_df, aes(multi_model$residuals)) +
  geom_histogram(binwidth = 30, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
  )

plot(multi_model$residuals ~ multi_model$fitted.values)
abline(h = 0, lty = 4, col="mediumvioletred")

plot(multi_model$residuals)
abline(h = 0, lty = 4, col="mediumvioletred")

#E
library(SignifReg)

#forward
forwardSelection_adjr2<- function(df, response, max_steps = NaN, verbose = TRUE){
  
  selected_vars = c()
  remain_vars <- names(df)
  remain_vars <- remain_vars[-which(remain_vars == response)] # removing response variable
  if (is.na(max_steps)){
    max_steps <- length(remain_vars)
  }
  current_adjR <- 0
  best_formula <- ""
  
  step = 1
  while(step <= max_steps){
    results = c()
    for (var in remain_vars){
      if (is.null(selected_vars)){
        vars <- var
      } else{
        vars <- paste(paste(selected_vars, collapse = ' + '), var, sep = ' + ')    
      }
      formula = paste(response, vars, sep = ' ~ ')
      summary <- summary(lm(as.formula(formula), data = df))
      results <- c(results, summary$adj.r.squared)
    }
    
    if (max(results) > current_adjR){
      current_adjR <- max(results)
      selected_vars <- c(selected_vars, remain_vars[which.max(results)])
      remain_vars <- remain_vars[-which.max(results)]
      best_formula <- paste(response, paste(selected_vars, collapse = ' + '), sep = ' ~ ')
      if (verbose){
        cat(paste("\n\nStep ", step, ":\n"))
        print(best_formula)
        cat(paste("Adjusted R Squared: ", current_adjR))
      }
    } else{
      if (verbose){
        cat(paste("\n\nNo improvment in Adjusted R Squared, finished in step ", step-1))
      }
      break
    }
    step <- step + 1
  }
  return (lm(formula = as.formula(best_formula), data = df))
}



reg_forw_adjr2 <- forwardSelection_adjr2(new_df[-c(1)], response = "health_bills", verbose = TRUE)

fitt_1 <- lm(health_bills ~ 1, data =new_df[-c(1)])
nullmodel = lm(health_bills~1, new_df[-c(1)])
fullmodel = lm(health_bills~., new_df[-c(1)])
scope = list(lower=formula(nullmodel),upper=formula(fullmodel))
reg_forw_pval <- SignifReg(fitt_1, scope = scope,alpha = 0.05,
                           direction = "forward",criterion = "p-value",trace = TRUE)

reg_forw_pval$steps.info['max_pvalue']


#backward
backwardSelection_adjr2 <- function(df, response, max_steps = NaN, verbose = TRUE){
  
  selected_vars <- names(df)
  selected_vars <- selected_vars[-which(selected_vars == response)] # removing response variable
  if (is.na(max_steps)){
    max_steps <- length(selected_vars) - 1
  }
  current_adjR <- 0
  best_formula <- paste(response, paste(selected_vars, collapse = ' + '), sep = ' ~ ')
  
  step = 1
  while(step <= max_steps){
    results = c()
    for (var in selected_vars){
      vars <- paste(selected_vars[-which(selected_vars == var)], collapse = ' + ')    
      
      formula = paste(response, vars, sep = ' ~ ')
      summary <- summary(lm(as.formula(formula), data = df))
      results <- c(results, summary$adj.r.squared)
    }
    
    if (max(results) > current_adjR){
      current_adjR <- max(results)
      selected_vars <- selected_vars[-which.max(results)]
      best_formula <- paste(response, paste(selected_vars, collapse = ' + '), sep = ' ~ ')
      if (verbose){
        cat(paste("\n\nStep ", step, ":\n"))
        print(best_formula)
        cat(paste("Adjusted R Squared: ", current_adjR))
      }
    } else{
      if (verbose){
        cat(paste("\n\nNo improvment in Adjusted R Squared, finished in step ", step-1))
      }
      break
    }
    step <- step + 1
  }
  
  return (lm(formula = as.formula(best_formula), data = df))
}



reg_backw_adjr2 <- backwardSelection_pval(new_df[-c(1)], response = "health_bills", verbose = TRUE)
 

fitt_2 <- lm(health_bills ~ ., data =new_df[-c(1)])
reg_backw_pval <- SignifReg(fitt_2, scope = scope,alpha = 0.05,
                           direction = "backward",criterion = "p-value",adjust.method = "fdr",
                           trace = TRUE)


reg_backw_pval$steps.info[c(1,10)]


#Best model
best_model <- lm(health_bills ~ age + heart_disease + ever_married + work_type +
                   Residence_type + avg_glucose_level + bmi + stroke, data = new_df[-c(1)])

summary(best_model)





#F
#linearity
library(ggplot2)
scatter_plot <- function(explanatory, xlabel, title){
  ggplot(non_miss_df, aes(x = explanatory, y = health_bills) ) +
    geom_point(color = 'mediumaquamarine')+
    xlab(xlabel)+
    ggtitle(title)+
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
    )
}

scatter_plot(non_miss_df$age, "age", "scatter plot for age and health_bills")
scatter_plot(non_miss_df$avg_glucose_level,'avg_glucose_level', "scatter plot for avg_glucose_level and health_bills")
scatter_plot(non_miss_df$bmi,'bmi',"scatter plot for bmi and health_bills")
scatter_plot(non_miss_df$stroke,'stroke', "scatter plot for stroke and health_bills")


#nearly normal residuals
hist(best_model$residuals, col="lightseagreen")

qqnorm(best_model$residuals, col="lightseagreen")
qqline(best_model$residuals)


#constant variability of residuals
plot(best_model$residuals~ best_model$fitted.values, col = "lightseagreen")
abline(h = 0, lty = 4, col="mediumvioletred")

plot(abs(best_model$residuals)~ best_model$fitted.values, col = "lightseagreen")
abline(h = 0, lty = 4, col="mediumvioletred")




#G
library(caret)

#training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)

# Train the model in B
model_B <- train(health_bills ~ stroke + bmi + age, data = new_df, method = "lm",
               trControl = train.control)
# Summarize the results
print(model_B)


# Train the model in E
model_E <- train(health_bills ~ age + heart_disease + ever_married + work_type +
                   Residence_type + avg_glucose_level + bmi + stroke, 
                 data = new_df, method = "lm",
                 trControl = train.control)
# Summarize the results
print(model_E)








#################question 6###################
new_df$gender[new_df['gender'] == "Other"] <- "Male"

#A
glm_model <- glm(heart_disease ~ age + gender + avg_glucose_level +
                   smoking_status + Residence_type, 
                 data = new_df, family = binomial)
glm_summary <- summary(glm_model)


#B
set.seed(42)
x_axis <-round(sort(runif(1000, min=0, max=1)),4)

var <- exp(glm_summary$coefficients[3])
y_axis <-(var * (x_axis / (1 - x_axis))) / (1 + (var * (x_axis / (1 - x_axis))))

plot(x_axis, y_axis, type="l",xlab="P(heart disease | ~male)", ylab="P(heart disease | male)")
abline(a = 0 , b = 1, col = "red", lty = 3)
title("Male odds ratio curve ")


#C
library(pROC)

pred <- predict(glm_model, type = "response")

par(pty = "s")
roc_ <- roc(new_df$heart_disease, pred, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
            xlab = "False Positive Percentage", ylab = "True Positive Percentage",
            col = "#377eb8", lwd = 4, print.auc = TRUE)


#E
significant_model <- glm(heart_disease ~ age + gender + avg_glucose_level,
                         data = new_df, family = binomial)
summary(significant_model)


#F
set.seed(123) 
threshold <- round(sort(runif(200, min=0, max=1)),4)
utility_list <- list()

c <- 0 

for (i in threshold){
  predicted_values <- ifelse(predict(significant_model,type="response")>i,1,0)
  actual_values <- significant_model$y
  conf_matrix <- table(predicted_values,actual_values)
  c <- c + 1
  if(is.na(conf_matrix[3]) && is.na(conf_matrix[4])){
    utility <- conf_matrix[1] - (5 * conf_matrix[2])
    utility_list[[c]] <- utility
  }
  else{
    utility <- conf_matrix[1] + conf_matrix[4] - (5 * conf_matrix[2]) - (10 * conf_matrix[3])
    utility_list[[c]] <- utility
  }
  
}

utility_df <- data.frame(matrix(unlist(utility_list), nrow=200, byrow=TRUE),stringsAsFactors=FALSE)
colnames(utility_df) <- c('utility')
utility_df$threshold <- threshold

plot(utility_df$threshold, utility_df$utility, type="l",xlab="P",
     ylab="U")

y <- c(max(utility_df$utility))
x <- utility_df$threshold[utility_df$utility == y][1]
points(x, y, pch = 19, col ="red")
title("Utility Curve")








#################question 7###################
median_healthBills <- median(new_df$health_bills)

new_df$high_medical_costs <- ifelse(new_df$health_bills > median_healthBills , 1,0)
View(new_df)


high_medical_costs_glm <- glm(high_medical_costs ~ age + work_type + 
                                hypertension + gender, data = new_df, family = binomial)


high_medical_costs_smry <- summary(high_medical_costs_glm)

