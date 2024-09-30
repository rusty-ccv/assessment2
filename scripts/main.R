# Script Information ----

# Stat2003 Assessment 2 R script 
# For Data collection and Quantitative Analysis (STAT2003-2004-T4)

# Title: Pan trap invertebrate abundance ANOVA analysis
# Purpose: Analysis with two way ANOVA orthogonal design.
# Null hypotheses:
#    The colour of the pan traps have no effect on abundance of winged invertebrates caught,
#    The direct sunlight on the pan traps has no effect on abundance of winged invertebrates caught,
#    There is no interaction between the effect of pan colour and sunlight.
# Data files used: data/invertebrate_data.csv
# Date script created: "Sat Sep 21 11:21:41 2024"
# Date script modified:
date()

## Author details ----
# Author: Duncan Sage
# Student ID: 24034122

## Package dependencies ----

### Install and load packages ----

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggpubr)) install.packages("ggpubr")
if (!require(lmtest)) install.packages("lmtest")
if (!require(GGally)) install.packages("GGally")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggfortify)) install.packages("ggfortify")
if (!require(car)) install.packages("car")
if (!require(psych)) install.packages("psych")
if (!require(nlme)) install.packages("nlme")
if (!require(lmerTest)) install.packages("lmerTest")
if (!require(dplyr)) install.packages("dplyr")

library(tidyverse)
library(ggpubr)
library(lmtest)
library(GGally)
library(ggplot2) 
library(ggfortify)
library(car)
library(psych)
library(nlme)
library(lmerTest)
library(dplyr)

### Package and r, RStudio version ----

sessionInfo()
xfun::session_info()



rm(list=ls())  # Remove any variables from memory.

getwd()
# setwd()


# Main script ----

## Chart themes and colour palette ----
chart_palette <- c("#333eff","#FF5733","#FFC300")

bar_chart_theme <- theme(panel.background=element_rect(fill= "white"))+
  theme(axis.line=element_line(colour="black",
                               linewidth = 1, linetype="solid"))+
  theme(axis.ticks=element_line(colour="black", linewidth = 1, linetype = "solid"))+
  theme(axis.text.x = element_text(size = 12,
                                   colour = "black"))+
  theme(axis.text.y = element_text(size = 12,
                                   colour = "black"))+
  theme(text=element_text(size = 12,
                          colour = "black"))+
  theme(aspect.ratio = 3/4)


legend_theme <- theme(legend.background = element_rect(fill="transparent",
                                                       linewidth = 1, linetype="solid",
                                                       colour ="transparent"))+
  theme(legend.text = element_text (size = 11,
                                    colour = "black"))+
  theme(legend.key = element_rect(color = "transparent", fill = "transparent"),
        legend.key.size = unit(.5, "cm"))+
  theme(legend.position = c(0.7, 0.92), 
        legend.justification = c(0.5, 0.5), 
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.title.align = 0.5)

## Load data ----
insect.data <- read.csv("data/invertebrate_data.csv")

## View data ----

### Data object ----
# View(insect.data)
str(insect.data)

### Basic plots ----
boxplot(abundance ~ pan_colour * canopy, data = insect.data)
describe(abundance ~ pan_colour * canopy, data =insect.data)
autoplot(lm(abundance ~ pan_colour + canopy + pan_colour:canopy,
            data = insect.data), which = 1:6, ncol = 2, label.size = 2)


## ANOVA ----
# Two-factor ANOVA with an orthogonal design
anova_insect <- aov(abundance~pan_colour+canopy+pan_colour:canopy,data=insect.data)  
summary(anova_insect)

## Plots ----

### Plot object ----
plot_insect_abundance <- insect.data %>% 
  group_by(pan_colour, canopy) %>%
  summarise(
    abundance_mean = mean(abundance),
    abundance_se = sd(abundance) / sqrt(length(abundance)),
    .groups = 'drop'
  ) %>%
  as.data.frame()

### Checking for abundance ----
interaction.plot(x.factor = insect.data$pan_colour, trace.factor = insect.data$canopy, response = insect.data$abundance)
# view(plot_insect_abundance) 

# cheese_plot$hsd_result <- c("a","a","a","a")
                            
### Bar chart ----
ggplot(plot_insect_abundance, aes(fill=pan_colour, y=abundance_mean, x=canopy)) + # Bars for mean
  geom_bar(position="dodge", stat="identity", color="black") + # Format bars
  scale_fill_manual(values=chart_palette,labels=c ('Blue','Red','Yellow'),
                    guide = guide_legend(title.position = "top")) + # Format legend 
  geom_errorbar(aes(ymin=abundance_mean-abundance_se, ymax=abundance_mean+abundance_se), width=.3, position=position_dodge(0.9))+ # Standard error bars
  # geom_text(aes(label = hsd_result, y = abundance_mean + abundance_se), vjust = -0.5, position=position_dodge(0.9))+ # Add labels for HSD results.
  labs(y="Mean insect abundance (\u00B1 SE)",x = expression(paste("Trap Location and Colour (", italic(n), "=5)")), fill="Pan trap colours") + # Label
  scale_x_discrete (labels=c ('Canopy','Open Area')) + # Label x axis.
  scale_y_continuous(limits=c(0,20), expand = expansion(mult=c(0,0)), breaks=seq(0,20,4)) + # Set scale on y axis
  bar_chart_theme + 
  legend_theme

chart_insect_bar <- last_plot()
## Export ----

### Export plots ----

ggsave(
  filename = "output/insect_mean.jpeg",
  plot = chart_insect_bar,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = 22,
  height = 16,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)

### Export tables ----

 

a2_int_fixed <- aov(Ant_Abundance ~ Chicken_Beef_Honey + Light_Shade + Chicken_Beef_Honey:Light_Shade, data =Raw_data)
coef(a2_int_fixed)
summary(a2_int_fixed)
par(mfrow=c(1,2))
plot(a2_int_fixed,c(1,2)) 
par(mfrow=c(1,1))
a2_fixed_residuals <- residuals(object = a2_int_fixed )
shapiro.test(x =aov_Raw_data_residuals)
bartlett.test(Ant_Abundance ~ interaction(Chicken_Beef_Honey, Light_Shade), data=Raw_data) 
summary(a2_int_fixed)
interaction.plot(x.factor = Raw_data$Chicken_Beef_Honey, trace.factor = Raw_data$Light_Shade, response = Raw_data$Ant_Abundance)
TukeyHSD(a2_int_fixed)
sink("anova_output.txt") 
summary(a2_int_fixed)
TukeyHSD(a2_int_fixed)
sink()


Raw_data$Light_Shade <- as.factor(Raw_data$Light_Shade)
str(Raw_data)
aov_Raw_data<- aov(Ant_Abundance~Chicken_Beef_Honey+Light_Shade+Chicken_Beef_Honey:Light_Shade,data=Raw_data)
summary(aov_Raw_data)
Raw_data$Chicken_Beef_Honey <- as.factor(Raw_data$Chicken_Beef_Honey)
aov_Raw_data_residuals <- residuals(object = aov_Raw_data)

