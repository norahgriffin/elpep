# Import packages

package.list <- c("dplyr", "ggplot2", "lubridate", "tidyr", "gmodels", "bbmle", "glmnet")
for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}

# Identify directory locations

dir  <- "C:/users/mcarter/Documents/estimating eligible population counts"
input <- "C:/users/mcarter/Documents/estimating eligible population counts/input"
output <- "C:/users/mcarter/Documents/estimating eligible population counts/output"

# -------------------------------------- #
# PURPOSE: This file compares statistics available in the ACS public-use files to 
# statistics derived from our estimates (produced using small area estimation or
# some other method).
# DATA SETS USED BY THIS CODE: 
# R VERSION: 3.6.3

# AUTHOR: Michele Carter
# DATE CREATED: 2020-08-30
# NOTES:

# -------------------------------------- #

# ------------------------------------------------------ #
#  Prepare data ----
# ------------------------------------------------------ #

# ------------------------------------ #
# Import our estimates
# ------------------------------------ #

# Import tract-level small area estimates. These estimates are produced by the file 
# "smallareaestimation_acs.R".

estimates_sae_tract <- read.csv(file.path(output, "smallareaestimation_acs_tract.csv"))

# Import county-level small area estimates. These estimates are produced by the file 
# "smallareaestimation_acs.R".

estimates_sae_county <- read.csv(file.path(output, "smallareaestimation_acs_county.csv"))

# ------------------------------------ #
# Construct ACS PUMA-level, 1-year statistics 
# ------------------------------------ #

# Import ACS microdata for Illinois.

acs1microdata <- read.csv(file.path(input, "acs_20200617.csv")) %>%
                 filter(STATEFIP==17)

# Construct family-level data set (raw data is organized at the individual level).
# Drop vacant households and households residing in group quarters. 
# Flag program-eligible families (for now, families with children between ages 0 and 5
# whose family income falls at or below the federal poverty level).

acs1microdata <- acs1microdata %>% 
                  filter(GQ==1 | GQ==2) %>%
                  mutate(
                    UNDER5 = 1*(AGE>=0 & AGE<=5),
                  ) %>%
                  group_by(YEAR, SERIAL, FAMUNIT, HHWT, STATEFIP, PUMA) %>% 
                  summarize(
                    FAM_UNDER5 = sum(UNDER5),
                    POVERTY = max(POVERTY)
                  ) %>%
                  mutate(ELIGIBLE = 1*(FAM_UNDER5>0 & POVERTY>=1 & POVERTY<=100))

# Calculate program-eligible share and count variables for families by PUMA.

acs1puma <- acs1microdata %>%
              group_by(STATEFIP, PUMA) %>%
              summarize(
                COUNT_ALL = sum(HHWT),
                COUNT_ELIGIBLE = sum(HHWT*ELIGIBLE),
                SD_ELIGIBLE = sd(HHWT*ELIGIBLE),
                SHARE_ELIGIBLE = sum((HHWT/sum(HHWT))*ELIGIBLE),
                SD_SHARE_ELIGIBLE = sd((HHWT/sum(HHWT))*ELIGIBLE)) %>%
              select(STATEFIP, PUMA, SHARE_ELIGIBLE)

# ------------------------------------ #
# Import 2010 Census tract-to-2010 PUMA crosswalk
# I downloaded this crosswalk here: www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt
# ------------------------------------ #

# Produce 2010 Census tract-to-2010 PUMA crosswalk.

cw_tract_puma <- read.csv(file.path(input, "2010_Census_Tract_to_2010_PUMA.txt"))
cw_tract_puma <- cw_tract_puma %>%
                  select(STATEFP, COUNTYFP, TRACTCE, PUMA5CE) %>%
                  rename(STATEFIP = STATEFP,
                         COUNTYFIP = COUNTYFP,
                         TRACTFIP = TRACTCE,
                         PUMA = PUMA5CE) %>%
                  filter(STATEFIP==17)

# Produce 2010 county-to-2010 PUMA crosswalk.

cw_county_puma <- cw_tract_puma %>%
                  select(STATEFIP, COUNTYFIP, PUMA) %>%
                  distinct()

# ------------------------------------------------------ #
#  Compare ACS PUMA-level, 1-year statistics to statistics derived from our estimates ----
# ------------------------------------------------------ #

# ------------------------------------ #
# Aggregate our estimates to the PUMA-level
# ------------------------------------ #

# Aggregate our tract-level small area estimates to the PUMA level.

sae_tract_puma <- merge(estimates_sae_tract,
                        cw_tract_puma,
                        by = c("STATEFIP", "COUNTYFIP", "TRACTFIP"),
                        all.x = TRUE,
                        all.y = FALSE)

sae_tract_puma <- sae_tract_puma %>%
                  group_by(STATEFIP, PUMA) %>%
                  summarize(
                    SHARE_ELIGIBLE_SAE_TRACT_PUMA       = sum((TRACT_FAMILIES/sum(TRACT_FAMILIES))*SHARE_ELIGIBLE_SAE),
                    SHARE_ELIGIBLE_SAE_LASSO_TRACT_PUMA = sum((TRACT_FAMILIES/sum(TRACT_FAMILIES))*SHARE_ELIGIBLE_SAE_LASSO)
                  )

# Aggregate our county-level small area estimates to the PUMA level

sae_county_puma <- merge(estimates_sae_county,
                         cw_county_puma,
                         by = c("STATEFIP", "COUNTYFIP"),
                         all.x = TRUE,
                         all.y = FALSE)

sae_county_puma <- sae_county_puma %>%
                    group_by(STATEFIP, PUMA) %>%
                    summarize(
                      SHARE_ELIGIBLE_SAE_COUNTY_PUMA       = sum((COUNTY_FAMILIES/sum(COUNTY_FAMILIES))*SHARE_ELIGIBLE_SAE),
                      SHARE_ELIGIBLE_SAE_LASSO_COUNTY_PUMA = sum((COUNTY_FAMILIES/sum(COUNTY_FAMILIES))*SHARE_ELIGIBLE_SAE_LASSO)
                    )

# Merge our aggregated estimates to ACS PUMA-level, 1-year statistics.

compare_puma <- merge(sae_tract_puma,
                      sae_county_puma,
                      by = c("STATEFIP", "PUMA"),
                      all.x = TRUE,
                      all.y = TRUE)

compare_puma <- merge(compare_puma,
                      acs1puma,
                      by = c("STATEFIP", "PUMA"),
                      all.x = TRUE,
                      all.y = TRUE) 

# Construct new variables that measure the difference between our estimates
# and ACS PUMA-level, 1-year statistics.

compare_puma <- compare_puma %>%
                mutate(DIFF_SAE_TRACT        = SHARE_ELIGIBLE_SAE_TRACT_PUMA - SHARE_ELIGIBLE,
                       DIFF_SAE_LASSO_TRACT  = SHARE_ELIGIBLE_SAE_LASSO_TRACT_PUMA - SHARE_ELIGIBLE,
                       DIFF_SAE_COUNTY       = SHARE_ELIGIBLE_SAE_COUNTY_PUMA - SHARE_ELIGIBLE,
                       DIFF_SAE_LASSO_COUNTY = SHARE_ELIGIBLE_SAE_LASSO_COUNTY_PUMA - SHARE_ELIGIBLE,
                       PCT_SAE_TRACT         = (DIFF_SAE_TRACT/SHARE_ELIGIBLE)*100,
                       PCT_SAE_LASSO_TRACT   = (DIFF_SAE_LASSO_TRACT/SHARE_ELIGIBLE)*100,
                       PCT_SAE_COUNTY        = (DIFF_SAE_COUNTY/SHARE_ELIGIBLE)*100,
                       PCT_SAE_LASSO_COUNTY = (DIFF_SAE_LASSO_COUNTY/SHARE_ELIGIBLE)*100
                      )

# ------------------------------------ #
# Analyze differences between ACS PUMA-level, 1-year statistics and statistics derived from our estimates
# ------------------------------------ #

# Write a function to plot the kernel density and underlying histogram.

kernel_plot <- function(input_data, x_var, x_lab, file_name){
  
  plot <- ggplot(input_data, aes_string(x = x_var)) +
            geom_histogram(aes(y=..density..), colour = "black", fill = "white", position = "identity") +
            geom_density(alpha = 0.2, fill = "#FF6666") + 
            labs(x = x_lab,
                 y = "Density",
                 title = "Kernel Density and Histogram")
  
  aspect_ratio <- 1.5
  ggsave(file.path(output, file_name), height = 7, width = 7*aspect_ratio)
  
  return(plot)
  
}

# Write a function to produce a scatter plot

scatter_plot <- function(input_data, x_var, y_var, x_lab, y_lab, file_name){
  
  plot <- ggplot(input_data, aes_string(x = x_var, y = y_var)) +
            geom_point() +
            geom_abline(aes(intercept = 0, slope = 1, colour = "black")) +
            scale_color_identity("", labels = c("Line with Slope 1, Intercept 1"), guide = "legend") +
            labs(x = x_lab,
                 y = y_lab,
                 title = "Scatter Plot")
  
  aspect_ratio <- 1.5
  ggsave(file.path(output, file_name), height = 7, width = 7*aspect_ratio)
  
  return(plot)
  
}

# Write a function to produce a scatter plot with standardized variables (so that
# slope of fitted OLS line equals pearson correlation coefficient)

scatter_std_plot <- function(input_data, x_var, y_var, x_lab, y_lab, file_name){
  
  plot_data <- input_data %>%
                mutate(x_std = (eval(parse(text=x_var)) - mean(eval(parse(text=x_var))))/sd(eval(parse(text=x_var))),
                       y_std = (eval(parse(text=y_var)) - mean(eval(parse(text=y_var))))/sd(eval(parse(text=y_var)))
                       )
  
  plot <- ggplot(plot_data, aes_string(x = "x_std", y = "y_std")) +
    geom_point() +
    stat_smooth(aes(colour = "blue"), method = "lm", se = FALSE) +
    geom_abline(aes(intercept = 0, slope = 1, colour = "black")) +
    scale_color_identity("", labels = c("Line with Slope 1, Intercept 0", "Line with Slope Equal to Pearson Correlation Coefficient"), guide = "legend") +
    labs(x = x_lab,
         y = y_lab,
         title = "Scatter Plot for Standardized Variables")
  
  aspect_ratio <- 1.5
  ggsave(file.path(output, file_name), height = 7, width = 7*aspect_ratio)
  
  return(plot)
  
}

# Produce bar chart with observed PUMA values, plus our estimates as colored dots, for tract-level estimates.

ggplot(compare_puma) +
      geom_bar(aes(x = reorder(PUMA, SHARE_ELIGIBLE), y = SHARE_ELIGIBLE, fill = "PUMA ACS 1-Year Statistic"), stat = "identity") +
      geom_point(aes(x = reorder(PUMA, SHARE_ELIGIBLE), y = SHARE_ELIGIBLE_SAE_TRACT_PUMA, color = "Tract Small Area Estimate"), size = 3) + 
      geom_point(aes(x = reorder(PUMA, SHARE_ELIGIBLE), y = SHARE_ELIGIBLE_SAE_LASSO_TRACT_PUMA, color = "Tract LASSO Small Area Estimate"), size = 3) +
      labs(x = "PUMA, Ordered by Observed PUMA-Level Statistic",
           y = "Share of Families in PUMA That Are Program-Eligible",
           title = "Observed PUMA-Level Statistic Compared to PUMA-Aggregated Estimates") +
      scale_fill_manual(name = "Observed Statistic", values = c("PUMA ACS 1-Year Statistic" ="grey")) +
      scale_color_manual(name = "PUMA-Aggregated Estimates", values = c("Tract Small Area Estimate" = "#00BFC4", "Tract LASSO Small Area Estimate" = "#F8766D")) + 
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

aspect_ratio <- 1.5
ggsave(file.path(output, "barscatterplot_tract_puma.png"), height = 7, width = 7*aspect_ratio)

# Produce bar chart with observed PUMA values, plus our estimates as colored dots, for county-level estimates.

ggplot(compare_puma) +
  geom_bar(aes(x = reorder(PUMA, SHARE_ELIGIBLE), y = SHARE_ELIGIBLE, fill = "PUMA ACS 1-Year Statistic"), stat = "identity") +
  geom_point(aes(x = reorder(PUMA, SHARE_ELIGIBLE), y = SHARE_ELIGIBLE_SAE_COUNTY_PUMA, color = "County Small Area Estimate"), size = 3) + 
  geom_point(aes(x = reorder(PUMA, SHARE_ELIGIBLE), y = SHARE_ELIGIBLE_SAE_LASSO_COUNTY_PUMA, color = "County LASSO Small Area Estimate"), size = 3) +
  labs(x = "PUMA, Ordered by Observed PUMA-Level Statistic",
       y = "Share of Families in PUMA That Are Program-Eligible",
       title = "Observed PUMA-Level Statistic Compared to PUMA-Aggregated Estimates") +
  scale_fill_manual(name = "Observed Statistic", values = c("PUMA ACS 1-Year Statistic" ="grey")) +
  scale_color_manual(name = "PUMA-Aggregated Estimates", values = c("County Small Area Estimate" = "#00BFC4", "County LASSO Small Area Estimate" = "#F8766D")) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

aspect_ratio <- 1.5
ggsave(file.path(output, "barscatterplot_county_puma.png"), height = 7, width = 7*aspect_ratio)

# Plot the kernel density and the underlying histogram for the specified variables.

var_list1 <- c("DIFF_SAE_TRACT", "DIFF_SAE_LASSO_TRACT", 
              "DIFF_SAE_COUNTY", "DIFF_SAE_LASSO_COUNTY",
              "PCT_SAE_TRACT", "PCT_SAE_LASSO_TRACT",
              "PCT_SAE_COUNTY", "PCT_SAE_LASSO_COUNTY")

var_lab_list1 <- c("Difference between PUMA-Aggregated Tract Small Area Estimates and Observed PUMA-Level Statistic",
                  "Difference between PUMA-Aggregated Tract LASSO Small Area Estimates and Observed PUMA-Level Statistic",
                  "Difference between PUMA-Aggregated County Small Area Estimates and Observed PUMA-Level Statistic",
                  "Difference between PUMA-Aggregated County LASSO Small Area Estimates and Observed PUMA-Level Statistic",
                  "% Deviation, PUMA-Aggregated Tract Small Area Estimates From Observed PUMA-Level Statistic",
                  "% Deviation, PUMA-Aggregated Tract LASSO Small Area Estimates From Observed PUMA-Level Statistic",
                  "% Deviation, PUMA-Aggregated County Small Area Estimates From Observed PUMA-Level Statistic",
                  "% Deviation, PUMA-Aggregated County LASSO Small Area Estimates From Observed PUMA-Level Statistic"
                  )

file_list1 <- c("kernelplot_diff_sae_tract_puma.png",
               "kernelplot_diff_sae_lasso_tract_puma.png",
               "kernelplot_diff_sae_county_puma.png",
               "kernelplot_diff_sae_lasso_county_puma.png",
               "kernelplot_pct_diff_sae_tract_puma.png",
               "kernelplot_pct_diff_sae_lasso_tract_puma.png",
               "kernelplot_pct_diff_sae_county_puma.png",
               "kernelplot_pct_diff_sae_lasso_county_puma.png")

for (i in seq_along(var_list1)){
  kernel_plot(compare_puma, var_list1[i], var_lab_list1[i], file_list1[i])
  }

# Produce a scatter plot for the specified estimate against the PUMA ACS 1-year statistic.
# Produce same plot for standardized variables (so that slope of the fitted line equals the pearson
# correlation coefficient).

y_list2 <- c("SHARE_ELIGIBLE_SAE_TRACT_PUMA", 
            "SHARE_ELIGIBLE_SAE_LASSO_TRACT_PUMA",
            "SHARE_ELIGIBLE_SAE_COUNTY_PUMA",
            "SHARE_ELIGIBLE_SAE_LASSO_COUNTY_PUMA")

y_lab_list2 <- c("PUMA-Aggregated Tract Small Area Estimates",
                "PUMA-Aggregated Tract LASSO Small Area Estimates",
                "PUMA-Aggregated County Small Area Estimates",
                "PUMA-Aggregated County LASSO Small Area Estimates")

y_lab_std_list2 <- c("Standardized PUMA-Aggregated Tract Small Area Estimates",
                    "Standardized PUMA-Aggregated Tract LASSO Small Area Estimates",
                    "Standardized PUMA-Aggregated County Small Area Estimates",
                    "Standardized PUMA-Aggregated County LASSO Small Area Estimates")

file_list2 <- c("scatterplot_sae_tract_puma.png",
               "scatterplot_sae_lasso_tract_puma.png",
               "scatterplot_sae_county_puma.png",
               "scatterplot_sae_lasso_county_puma.png")

file_std_list2 <- c("scatterstdplot_sae_tract_puma.png",
                   "scatterstdplot_sae_lasso_tract_puma.png",
                   "scatterstdplot_sae_county_puma.png",
                   "scatterstdplot_sae_lasso_county_puma.png")

for (i in seq_along(y_list2)){
  scatter_plot(compare_puma, "SHARE_ELIGIBLE", y_list2[i], "Observed PUMA-Level Statistic", y_lab_list2[i], file_list2[i])
  scatter_std_plot(compare_puma, "SHARE_ELIGIBLE", y_list2[i], "Standardized Observed PUMA-Level Statistic", y_lab_std_list2[i], file_std_list2[i])
}

