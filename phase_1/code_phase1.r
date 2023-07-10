library(ggplot2)

data <- read.csv("HealthCare.csv")
View(data)



##########QUESTION 1############

#A

ggplot(data, aes(x =avg_glucose_level))+
  geom_histogram(aes(y=..density..),bins = 30, color = "black", fill="Gray")+
  geom_density(alpha=0.2, fill ="#FF6666")+
  scale_y_continuous(expand = c(0, 0))+
  ggtitle("histogram and density plot for average glucose level")+
  labs(x= "avrage glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )



#B

ggplot(data, aes(sample=avg_glucose_level))+
  stat_qq(color = "MediumVioletRed") + stat_qq_line(color = "#191970", lwd = 1)+ 
  ggtitle("Q-Q plot for compare normal distribution with avg_gloucose variable distribution")+
  labs(x = "normal distribution", y = "average glucose distribution")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )  



#C
library(moments)

skewness(data$avg_glucose_level)



#D
ggplot(data, aes(y = avg_glucose_level)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot(fill = "#008B8B") + 
  ggtitle("box plot for average glucose level")+
  labs(y= "avrage glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
  )+ coord_flip()



#E
glu_mean <- mean(data$avg_glucose_level)
glue_median <- median(data$avg_glucose_level)
var(data$avg_glucose_level)
sd(data$avg_glucose_level)



#F
ggplot(data, aes(x = avg_glucose_level)) +
  geom_density(color = "PaleVioletRed", fill="Pink", alpha=0.5)+
  geom_vline(xintercept=glu_mean, size=1, color="DarkOrange")+
  annotate("text", x=glu_mean, y=0, label= "mean", color = "DarkOrange", size = 5)+
  geom_vline(xintercept=glue_median, size=1, color="MediumSeaGreen")+
  annotate("text", x=glue_median, y=0.002, label= "median", color = "MediumSeaGreen", size = 5)+
  ggtitle("density plot with mean and median for average glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )



#G
category_glucose_level <- data.frame((table(cut(data$avg_glucose_level, 
          breaks=c(0, glu_mean / 4  , glu_mean / 2 , 3 * glu_mean / 4, glu_mean),
          label = c("A", "B", "C", "D")))))

value <- category_glucose_level$Freq / sum(category_glucose_level$Freq)

ggplot(category_glucose_level, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(value*100), "%","\n", "category:",Var1)), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D"))+
  labs(x = NULL, y = NULL, fill = NULL, title = "Pie chart for categorize 
       average glucose level based on mean")+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))



#H

lower <- quantile(data$avg_glucose_level, 0.25)
upper <- quantile(data$avg_glucose_level, 0.75)
whisker_L <- min(data$avg_glucose_level)
whisker_U <- upper + 1.5*(upper - lower)


ggplot(data, aes(y = avg_glucose_level)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot(fill = "LightPink") + 
  annotate("text", y=lower, x=0.3, label= "Q1", color = "#3CB371", size = 5)+
  annotate("text", y=upper, x=0.3, label= "Q3", color = "#3CB371", size = 5)+
  annotate("text", y=whisker_L, x=0.23, label= " lower whisker", 
           color = "purple", size = 5)+
  annotate("text", y=whisker_U, x=0.23, label= " upper whisker", 
           color = "purple", size = 5)+
  annotate("text", y=95, x=0.43, label= "IQR", color = "MidnightBlue", size = 5)+
  geom_segment(aes(x = 0.2, y = whisker_U - 2, xend = 0.1, yend = whisker_U),
               arrow = arrow(length = unit(0.5, "cm")), color = "Purple")+
  geom_segment(aes(x = 0.2, y = whisker_L - 2, xend = 0.1, yend = whisker_L),
               arrow = arrow(length = unit(0.5, "cm")), color = "Purple")+
  geom_segment(aes(x = 0.39, y = lower, xend = 0.39, yend = upper),
               color = "darkorange", lwd = 1)+
  labs(y= "avrage glucose level")+
  ggtitle("box plot for average glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
  )+ coord_flip()





##########QUESTION 2###############

#A
Residence <- data.frame(table(data$Residence_type))
Residence$per <- format(round(prop.table(data.frame(table(data$Residence_type))$Freq)
                            *100, 2), nsmall = 2)

colnames(Residence) <- c("Group", "Count", "percent")

#B
ggplot(Residence, aes(x=Group, y=Count, fill=Group)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(percent,"%")), vjust=1.6, color="black", size=4)+
  ggtitle("barplot for Residence type")+
  labs(x = "Group", y= "Count")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )



#C
Residence <- Residence[order(Residence$Count),]

ggplot(Residence, aes(x=Group, y=Count, fill=Group)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(percent,"%")), vjust=1.6, color="black", size=4)+ 
  ggtitle("barplot for Residence type")+
  labs(x = "Group", y= "Count")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )+ coord_flip()



#D
ggplot(data, aes(x=Residence_type, y = avg_glucose_level, fill=Residence_type)) +
  geom_violin()+
  geom_boxplot(width=0.06, fill="white")+
  scale_fill_brewer(palette="Dark2")+
  ggtitle("violin plot for average glucose based on Residence type")+
  labs(x = "Group", y= "average glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )
  







############QUESTION 3#############

#B
non_miss_df <- data[rowSums(is.na(data)) == 0, ]

ggplot(non_miss_df, aes(x = bmi, y= avg_glucose_level)) +
  geom_point(color = "SeaGreen")+
  ggtitle("scatter plot for bmi and average glucose level")+
  labs(x = "bmi", y= "average glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
  )



#C

cf <- cor(non_miss_df$bmi, non_miss_df$avg_glucose_level)



#E
cor.test(data$bmi, data$avg_glucose_level)



#F
ggplot(non_miss_df, aes(x = bmi , y=avg_glucose_level, group = gender, color = gender)) +
  geom_point()+
  scale_color_brewer(palette='Set1')+
  ggtitle("scatter plot for bmi and average glucose level")+
  labs(x = "bmi", y= "average glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
  )



#G
library(hexbin)
library(cowplot)


#sol1
hexbin <- ggplot(non_miss_df, aes(x = bmi , y=avg_glucose_level))+
  geom_hex(bins = 30)+ 
  geom_smooth(method = "gam", color = "red", formula = y ~ s(x, bs = "cs"))+ 
  labs(x = "bmi", y= "average glucose level")+
  ggtitle("Hexbin plot")+
  scale_fill_continuous(type = "viridis")+
  theme(
    legend.position = "left",
    plot.title = element_text(hjust = 0.5, size = 16),
  )


histTop <- axis_canvas(hexbin, axis = "x")+
  geom_histogram(data = non_miss_df, aes(bmi), bins = 40, color = "black", fill="#7B68EE")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
  )

histRight <- axis_canvas(hexbin, axis = "y", coord_flip = TRUE)+
  geom_histogram(data = non_miss_df,aes(avg_glucose_level), bins = 40, color = "black", fill="#7B68EE")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
  )+coord_flip()


x <- insert_xaxis_grob(hexbin, histTop, position = "top" )
y <- insert_yaxis_grob(x, histRight, position = "right")

ggdraw(y)


#sol2
library(ggExtra)

hexbin <- ggplot(non_miss_df, aes(x = bmi , y=avg_glucose_level))+
  geom_point()+
  geom_hex(bins = 30)+ 
  geom_smooth(method = "gam", color = "red", formula = y ~ s(x, bs = "cs"))+ 
  labs(x = "bmi", y= "average glucose level")+
  ggtitle("Hexbin plot")+
  scale_fill_continuous(type = "viridis")+
  theme(
    legend.position = "left",
    plot.title = element_text(hjust = 0.5, size = 16),
  )

ggMarginal(hexbin,
           type = 'histogram',
           margins = 'both',
           size = 5,
           colour = 'black',
           fill = '#7B68EE')



#F
#1
ggplot(non_miss_df, aes(x = bmi , y=avg_glucose_level))+
  geom_density_2d()+
  geom_density_2d_filled(contour_var = "ndensity")+
  ggtitle("2D density plot  for bmi and average glucose level")+
  labs(x = "bmi", y= "average glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
  )

#2
ggplot(non_miss_df, aes(x = bmi , y=avg_glucose_level))+
  geom_density_2d(color = "purple")+
  ggtitle("2D density plot  for bmi and average glucose level")+
  labs(x = "bmi", y= "average glucose level")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )






#############Question 4################

library(GGally)

numeric_data <- non_miss_df[, c(3, 9, 10, 13)]

ggpairs(numeric_data, progress = F,
        lower = list(continuous = wrap("points", color = "darkred", alpha = 0.5)),
        upper = list(continuous = wrap("density")))



#B
library(ggcorrplot)
library(Hmisc) 

cormat <- round(cor(numeric_data,  method = "pearson"), 2)
p.mat <- cor_pmat(numeric_data)

ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           lab = TRUE, p.mat =p.mat, sig.level = .05)+
 geom_text(aes(label = c(5.0751e-63, 2.9762e-35, 7.1875e-37,
                         9.5158e-128, 5.9126e-105, 5.1980e-212)), 
           vjust = 3, hjust = 0.5)



#C
library(scatterplot3d)

cols <- c("#4B0082", "yellow", "#008080", "MediumVioletRed")
group <- as.numeric(as.factor(non_miss_df$smoking_status))

scatterplot3d(x = non_miss_df$bmi, 
                        y = non_miss_df$age,
                        z = non_miss_df$avg_glucose_level,
                        pch = 16, color = cols[group],
                        main="3D Scatter Plot",
                        xlab = "bmi",
                        ylab = "age",
                        zlab = "average glucose level")

legend("right", legend = levels(as.factor(non_miss_df$smoking_status)),
       col = cols, pch = 16)








############Question 5############# 

#A
con_table <- table(data$gender, data$smoking_status)
addmargins(con_table)
df_2Catg <- data.frame(con_table)

colnames(df_2Catg)<- c("gender", "smoke_type", "count")

#B
ggplot(df_2Catg, aes(x = smoke_type, y = count, fill = gender))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Smoke type" , y = "count")+
  geom_text(aes(label=count), vjust=-0.4, color="black", size=4,
            position = position_dodge(.9))



#C
ggplot(df_2Catg, aes(x = smoke_type, y = count, fill = gender, label = count))+
  geom_bar(stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.6))+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Smoke type" , y = "count")



#D
library(ggmosaic)
df_2Catg$per <- format(round(data.frame(prop.table(con_table, margin = 2))$Freq
                                *100, 2), nsmall = 2)


#sol1
ggplot(df_2Catg, aes(x =smoke_type, y = count, fill = gender))+
  geom_bar(stat="identity",position = "fill")+
  scale_fill_brewer(palette = "Set2")+
  geom_text(aes(label= paste0(per, '%')), vjust=1.2, color="black", size=3.2,
            position = "fill")+
  labs(x = "Smoke type" , y = "count")


#sol2
ggplot(df_2Catg)+
  geom_mosaic(aes(x = product(smoke_type), weight = count, fill = gender))+
  scale_fill_brewer(palette = "Set2")+
  geom_text(aes(y =1,
                x = as.numeric(rep(c(0.1, 0.35, 0.63, 0.85), each = 3)),
                label= paste0(per, '%')), vjust=0, color="black", size=3.2,
            position = "fill")
  








###########Question 6#############

#A
set.seed(12345)

sample_total <- non_miss_df[sample(nrow(non_miss_df), 100), ]

plot(density(sample_total$health_bills))
qqnorm(sample_total$health_bills)
qqline(sample_total$health_bills, col = "steelblue", lwd = 2)

#sol1
library(Rmisc)

ci <- CI(sample_total$health_bills, ci = 0.95)
 
#sol2
SE = sd(sample_total$health_bills)/sqrt(length(sample_total$health_bills))
conf_left = mean(sample_total$health_bills) - qnorm(0.95) * SE
conf_right = mean(sample_total$health_bills) + qnorm(0.95) * SE



#C
ggplot(sample_total, aes(x =health_bills))+
  geom_histogram(aes(y=..density..),bins = 20, color = "MediumVioletRed", fill="MediumSeaGreen")+
  geom_vline(aes(xintercept = mean(health_bills)), col = "Gold",
             size = 1)+
  geom_vline(aes(xintercept = ci["upper"]), col = "darkred", 
             linetype = "dashed",size = 1)+
  geom_vline(aes(xintercept = ci["lower"]), col = "darkred", 
             linetype = "dashed", size = 1)+
  ggtitle("histogram for Health bills")+
  labs(x= "Health bills")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
  )



#D
z_stat <- (mean(sample_total$health_bills) - 3100) / SE 
p_val <- pnorm(z_stat, lower.tail = FALSE)


#F

mu0   <- 3100
mua   <- mean(non_miss_df$health_bills)  

alpha <- 0.05  

# critical value for a level alpha test
crit <- qnorm(1-alpha, mu0, SE)

# power: probability for values > critical value under H1
(pow <- pnorm(crit, mua, SE, lower.tail=FALSE))


# probability for type II error: 1 - power
(beta <- 1-pow)









###############Question 7###############

#A
set.seed(12345)

sample_25 <-non_miss_df[sample(nrow(non_miss_df), 25),c(9,10)]
View(sample_25)

#b
sample_25$diff <- sample_25$avg_glucose_level - sample_25$bmi
View(sample_25)

t.test(sample_25$diff, data = sample_25, alternative = "two.sided")



#B
n1 = nrow(non_miss_df)
n2 = n1/2

set.seed(12345)
smp2 <-non_miss_df[sample(1:n2+1, 100), c(1,9)]
smp1 <- non_miss_df[sample(n2+2 : n1 , 100),c(1, 10)]

#sol1
t.test(smp2$avg_glucose_level,smp1$bmi, alternative = "two.sided", 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#sol2
df = length(smp1$bmi) - 1
SE = sqrt((var(smp1$bmi)/length(smp1$bmi)) + 
            (var(smp2$avg_glucose_level)/length(smp2$avg_glucose_level)))

t_statistic <- (mean(smp2$avg_glucose_level) - mean(smp1$bmi)) / SE

left <- (mean(smp2$avg_glucose_level) - mean(smp1$bmi)) - (-qt(0.025, df= df) * SE)
right <- (mean(smp2$avg_glucose_level) - mean(smp1$bmi)) + (-qt(0.025, df= df) * SE)

p_value_orginal <- 2 * pt(t_statistic, df = df, lower.tail = FALSE)










##############Question 8#############
library(boot)

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}

#A
set.seed(12345)
sample_25 <-data.frame(non_miss_df[sample(nrow(non_miss_df), 25),c(9)])
colnames(sample_100) <- c("avg_glocuse_level")


Bootstrap_1 <- boot(sample_25, statistic=meanfun, R=1000)

plot(Bootstrap_1)

#compute CI with percentile method
smp_CI <- quantile(Bootstrap_1$t, c(0.025, 0.975))



#B
set.seed(12345)
sample_20 <-data.frame(non_miss_df[sample(nrow(non_miss_df), 20),c(9)])
colnames(sample_20) <- c("avg_glocuse_level")


Bootstrap_2 <- boot(sample_20, statistic=meanfun, R=1000)

plot(Bootstrap_2)

#compute CI with standard error method
df1 <- length(Bootstrap$t) - 1

ci_left <- Bootstrap$t0 - (-qt(0.025, df = df1) * sd(Bootstrap$t))
ci_right <- Bootstrap$t0 + (-qt(0.025, df = df1) * sd(Bootstrap$t))










##############Question 9#############

anova_one_way <- aov(health_bills ~ work_type, data = non_miss_df)
summary(anova_one_way)


TukeyHSD(anova_one_way)

plot(TukeyHSD(anova_one_way), las = 1,cex.axis=0.5)

