# Script Information ----
# Show script outline Ctrl + Shift + o for easy navigation within RSudio

# Title: Pan trap invertebrate abundance ANOVA analysis
# Purpose: Analysis with two way ANOVA orthogonal design.
# Null hypotheses:
#    The colour of the pan traps have no effect on abundance of winged invertebrates caught,
#    The direct sunlight on the pan traps has no effect on abundance of winged invertebrates caught,
#    There is no interaction between the effect of pan colour and sunlight.
# Data files used: data/invertebrate_data.csv
# Date script created: "Sat Sep 21 11:21:41 2024"
# Date script modified: "Tue Oct  1 17:19:35 2024"

## Author details ----
# Author: Duncan Hitchins Sage
# Student ID: 24034122

## Package dependencies ----
### Install and load packages ----
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggpubr)) install.packages("ggpubr")
if (!require(GGally)) install.packages("GGally")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggfortify)) install.packages("ggfortify")
if (!require(car)) install.packages("car")
if (!require(psych)) install.packages("psych")
if (!require(dplyr)) install.packages("dplyr")
if (!require(Matrix)) install.packages("Matrix")
if (!require(broom)) install.packages("broom")

library(tidyverse)
library(ggpubr)
library(GGally)
library(ggplot2) 
library(ggfortify)
library(car)
library(psych)
library(dplyr)
library(Matrix)
library(broom)

### Package, r, RStudio version and computer information ----
# R version 4.3.3 (2024-02-29 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22631), RStudio 2024.9.0.375
# Locale:
#   LC_COLLATE=English_Barbados.utf8  LC_CTYPE=English_Barbados.utf8    LC_MONETARY=English_Barbados.utf8
# LC_NUMERIC=C                      LC_TIME=English_Barbados.utf8    
# 
# Package version:
# abind_1.4-5           askpass_1.2.0         backports_1.5.0       base64enc_0.1.3       bit_4.0.5             bit64_4.0.5           blob_1.2.4           
# boot_1.3.30           broom_1.0.6           broom.helpers_1.17.0  bslib_0.8.0           cachem_1.1.0          callr_3.7.6           car_3.1-2            
# carData_3.0-5         cards_0.2.2           cellranger_1.1.0      cli_3.6.3             clipr_0.8.0           colorspace_2.1-1      compiler_4.3.3       
# conflicted_1.2.0      corrplot_0.94         cowplot_1.1.3         cpp11_0.4.7           crayon_1.5.3          curl_5.2.1            data.table_1.14.8    
# DBI_1.2.3             dbplyr_2.5.0          Deriv_4.1.3           digest_0.6.37         doBy_4.6.22           dplyr_1.1.4           dtplyr_1.3.1         
# evaluate_0.24.0       fansi_1.0.6           farver_2.1.2          fastmap_1.2.0         fontawesome_0.5.2     forcats_1.0.0         fs_1.6.4             
# gargle_1.5.2          generics_0.1.3        GGally_2.2.1          ggfortify_0.4.17      ggplot2_3.5.1         ggpubr_0.6.0          ggrepel_0.9.6        
# ggsci_3.2.0           ggsignif_0.6.4        ggstats_0.6.0         glue_1.7.0            googledrive_2.1.1     googlesheets4_1.1.1   GPArotation_2024.3.1 
# graphics_4.3.3        grDevices_4.3.3       grid_4.3.3            gridExtra_2.3         gtable_0.3.5          haven_2.5.4           highr_0.11           
# hms_1.1.3             htmltools_0.5.8.1     httr_1.4.7            ids_1.0.1             isoband_0.2.7         jquerylib_0.1.4       jsonlite_1.8.8       
# knitr_1.48            labeling_0.4.3        labelled_2.13.0       lattice_0.22-6        lifecycle_1.0.4       lme4_1.1.35.5         lubridate_1.9.3      
# magrittr_2.0.3        MASS_7.3.60.0.1       Matrix_1.6-5          MatrixModels_0.5.3    memoise_2.0.1         methods_4.3.3         mgcv_1.9.1           
# microbenchmark_1.4.10 mime_0.12             minqa_1.2.8           mnormt_2.1.1          modelr_0.1.11         munsell_0.5.1         nlme_3.1-166         
# nloptr_2.1.1          nnet_7.3.19           numDeriv_2016.8.1.1   openssl_2.2.1         parallel_4.3.3        patchwork_1.2.0       pbkrtest_0.5.3       
# pillar_1.9.0          pkgconfig_2.0.3       plyr_1.8.9            polynom_1.4.1         prettyunits_1.2.0     processx_3.8.4        progress_1.2.3       
# ps_1.7.7              psych_2.4.6.26        purrr_1.0.2           quantreg_5.98         R6_2.5.1              ragg_1.3.2            rappdirs_0.3.3       
# RColorBrewer_1.1-3    Rcpp_1.0.11           RcppEigen_0.3.4.0.2   readr_2.1.5           readxl_1.4.3          rematch_2.0.0         rematch2_2.1.2       
# reprex_2.1.1          rlang_1.1.4           rmarkdown_2.28        rstatix_0.7.2         rstudioapi_0.16.0     rvest_1.0.4           sass_0.4.9           
# scales_1.3.0          selectr_0.4.2         SparseM_1.84.2        splines_4.3.3         stats_4.3.3           stringi_1.8.4         stringr_1.5.1        
# survival_3.7.0        sys_3.4.2             systemfonts_1.1.0     textshaping_0.4.0     tibble_3.2.1          tidyr_1.3.1           tidyselect_1.2.1     
# tidyverse_2.0.0       timechange_0.3.0      tinytex_0.52          tools_4.3.3           tzdb_0.4.0            utf8_1.2.4            utils_4.3.3          
# uuid_1.2.1            vctrs_0.6.5           viridisLite_0.4.2     vroom_1.6.5           withr_3.0.1           xfun_0.47             xml2_1.3.6           
# yaml_2.3.10  

# Setup ----
rm(list=ls())  # Remove any objects from memory.
getwd()

# Main script ----
## Chart themes and colour palette ----
chart_palette <- c("#333eff","#FF5733","#FFC300") # Set colour palette for charts

bar_chart_theme <- theme(panel.background=element_rect(fill= "white"))+
  theme(axis.line=element_line(colour="black", 
                               linewidth = 1,
                               linetype="solid"))+ # Axis line colour, type and width
  theme(axis.ticks=element_line(colour="black",
                                linewidth = 1,
                                linetype = "solid"))+ # Axis tick colour, width and type
  theme(axis.text.x = element_text(size = 12,
                                   colour = "black"))+ # Axis text size and colour
  theme(axis.text.y = element_text(size = 12,
                                   colour = "black"))+ # Axis text size and colour
  theme(text=element_text(size = 12,
                          colour = "black"))+ # Text size and colour
  theme(aspect.ratio = 3/4) # Aspect ratio for plot

legend_theme <- theme(legend.background = element_rect(fill="transparent",
                                                       linewidth = 1,
                                                       linetype="solid",
                                                       colour ="transparent"))+ # Legend background colour, width, type and colour
  theme(legend.text = element_text(size = 11,
                                   colour = "black"))+ # Legend text size and colour
  theme(legend.key = element_rect(color = "transparent", 
                                  fill = "transparent"),
                                  legend.key.size = unit(.5, "cm"))+ # Legend key colour, fill and size
  theme(legend.position = "inside",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.position.inside = c(0.70, 0.75),
        legend.justification = c(0.5, 0.5), 
        legend.title = element_text(hjust = 0.5)) # Legend position, justification, direction, box and title alignment

## Load data to object ----
insect.data <- read.csv("data/invertebrate_data.csv")

## View data ----
### Data object ----
View(insect.data)
str(insect.data)

### Basic plots ----
boxplot(abundance ~ pan_colour * canopy, data = insect.data) # Boxplot
describe(abundance ~ pan_colour * canopy, data =insect.data) # Descriptive statistics
autoplot(lm(abundance ~ pan_colour + canopy + pan_colour:canopy, 
            data = insect.data),
            which = 1:6,
            ncol = 2,
            label.size = 2) # Diagnostic plots

## ANOVA ----
# Two-factor ANOVA with an orthogonal design
anova_insect <- aov(abundance~pan_colour+canopy+pan_colour:canopy, 
                    data=insect.data)  # Load Two way ANOVA to object
summary(anova_insect) # Summary of 2 way ANOVA on untransformed data

# Normality p value > 0.05 data is normal
anova_fixed_residuals <- residuals(object = anova_insect) # Residuals from ANOVA
shapiro.test(x = anova_fixed_residuals) # Shapiro test on residuals

# Homogeneous test > 0.05 data is Homogeneous
bartlett.test(abundance ~ interaction(pan_colour, canopy), data = insect.data) # Bartlett test on data
tukey_results <- TukeyHSD(anova_insect) # Post hoc test on untransformed data

## Create ANOVA table ----
anova_summary <- tidy(anova_insect) %>%
  select(term, df, sumsq, meansq, statistic, p.value) %>%
  rename(Term = term, DF = df, SS = sumsq, MS = meansq, F = statistic, p.value = p.value)

## Plots ----
### Plot object ----
#creating object for plotting, summarising and exporting
plot_insect_abundance <- insect.data %>% 
  group_by(pan_colour, canopy) %>%
  summarise(abundance_mean = mean(abundance), 
            abundance_se = sd(abundance) / sqrt(length(abundance)), 
            .groups = 'drop') %>%
  as.data.frame() 

# Add hard coded letters for significant differences form Tukey HSD
plot_insect_abundance$hsd_result <- c("abc","a","cd","ab","d","bcd")
                            
### Bar chart ----
ggplot(plot_insect_abundance,
       aes(fill=pan_colour,
           y=abundance_mean,
           x=canopy)) + # Set axis and fill
  geom_bar(position="dodge",
           stat="identity",
           color="black") + # Create bar chart
  scale_fill_manual(values=chart_palette,
                    labels=c ('Blue','Red','Yellow')) + # Set colour palette and labels
  guides(fill = guide_legend(title.position = "top", title.hjust=0.5))+ # Set legend position
  geom_errorbar(aes(ymin=abundance_mean-abundance_se,
                    ymax=abundance_mean+abundance_se),
                    width=.3,
                    position=position_dodge(0.9))+ # Add error bars
  geom_text(aes(label = hsd_result, y = abundance_mean + abundance_se),
            vjust = -0.2,
            position=position_dodge(0.9),
            hjust = -0.3)+ # Add labels for HSD results
  labs(y="Mean insect abundance (\u00B1 SE)",
       x = expression(paste("Trap Location and Colour (", italic(n), "=5)")),
       fill="Pan trap colours") + # Label
  scale_x_discrete (labels=c ('Canopy','Open Area')) + # Label x axis.
  scale_y_continuous(limits=c(0,20),
                     expand = expansion(mult=c(0,0)),
                     breaks=seq(0,20,4)) + # Set scale on y axis
  # Set chart themes
  bar_chart_theme + 
  legend_theme

chart_insect_bar <- last_plot() # Save plot to object

## Export ----
### Export tables ----
sink("output/anova_output.txt") 
print(anova_summary) # Summary of 2 way ANOVA on untransformed data
tukey_results # Post hoc test on untransformed data
sink()

### Export plots ----
ggsave(
  filename = "output/insect_mean.jpeg",
  plot = chart_insect_bar,
  device = "jpeg",
  scale = 1,
  width = 22,
  height = 16,
  units = c("cm"),
  dpi = "retina",
  limitsize = TRUE,
  bg = NULL)