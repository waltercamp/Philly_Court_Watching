# Title: Philly Court Watching Power Analysis
# Author: Walter Campbell
# Initial Date: 05/26/2025
# Last Edited Date: 05/26/2026
# Purpose: To produce a couple different estimates of power for the 
# Philly Court Watching Study, 960000-1701-000-01240.

# Clear workspace
rm(list=ls())

# Install and load packages - More than I ended up using, but can't hurt to have all
packages <- c(
  "arrow", "dataReporter", "data.table", "fs", "Hmisc", "here", "haven",
  "janitor", "lubridate", "magrittr", "purrr", "readr", "skimr", "stringr",
  "tidyr", "tidyselect", "labelled", "dplyr", "DescTools", "broom", 
  "janitor", "gmodels", "crosstable", "flextable", "pwr", "ggplot2", "SimEngine", 
  "tidyverse", "pwrss", "effectsize"
)
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
}
install_missing_packages(packages)
invisible(lapply(packages, library, character.only = TRUE))

## Determining a Reasonable MDEs ##
#Odds ratios from the BWC paper - also using a 12 percentage point change from the attorney type paper
ors <- c(0.897, 0.897)
#Base rate for convictions for felonies and misdemeanors
bases <- c(0.40, 0.23)
percents <- data.frame(ors, bases)
percents <- percents %>%
  mutate(arrs = oddsratio_to_arr(OR=ors, p0=bases))
percents
# 2.57 percentage point change for the BWC paper

#Determine effect sizes associated with changes: 2.5%, 7.25%, 12%
prop1 <- c(0.40, 0.23, 0.40, 0.23, 0.40, 0.23)
prop2 <- c(0.375, 0.205, 0.3275, 0.1575, 0.28, 0.11)
hs <-data.frame(prop1, prop2)
hs <- hs %>%
  mutate(MDEs = ES.h(p1=prop1, p2=prop2))
hs

## Classic power estimate ##
library(pwr)
#Varying Sample Size to Get MDES
#Set the power analysis parameters to obtain the MDEs for a test of two proportions
samplesize <- c(1846, 1026)
power <- (0.80)
siglev <- (0.05)
alternative <- ("two.sided")
#Run the power analysis
mdes <- sapply(samplesize, 
               FUN = function(x) {
                 pwr.2p.test(n = x, 
                             power = power, sig.level = siglev, 
                             alternative = alternative)$h})
plot_df <- data.frame(samplesize, mdes)
library(ggplot2)
ggplot(plot_df, aes(x=samplesize,
                    y=mdes))+geom_point()+geom_line()

## Simulation power estimate ##
#NOTE: We assume a 7.25% change in conviction rates, and baseline conviction rates
#of 40% for felonies and 23% for misdemeanors. I use the above sample sizes as my
#middle point, but also explore MDEs under slightly smaller and large sample sizes
#as the actual number of cases per month may vary. With simple random assignment,
#we're assuming a fairly straightforward model at this point, using the treatment 
#indicator as our impact estimate. While we assume we'll add in control variables,
#we don't have that data yet from the Philly DAO, but will add it for an updated power
#analysis. An updated power analysis will also examine power for other outcomes.

#Create a function that will simulate data with different sizes and baseline proportions
my_power_function <- function(baseline_prop, sample_size) {
  #Store results here
  sig_results <- c()
  #Simulate data 1000 times
  for (i in 1:1000) {
    #Create the treatment group
    treatment <- data.frame(condition=rep(1, sample_size), convic=rbinom(n=sample_size, size=1, prob=baseline_prop-0.0725))
    #Create the comparison group
    comparison <- data.frame(condition=rep(0, sample_size), convic=rbinom(n=sample_size, size=1, prob=baseline_prop))
    #Combine conditions 
    sample <- rbind(treatment, comparison)
    #Run a rudimentary model
    mylogit <- glm(convic ~ condition, data = sample, family = "binomial")
    summary(mylogit)
    #Store whether the impact of the legislation is significant at an 0.05 level
    sig_results[i]  <- tidy(mylogit)$p.value[2] <= .05
  }
  #Store mean of significance
  sig_results %>%
    mean() %>%
    return()
}

#Try different sample sizes for each court type
felony_sample_sizes_to_try <- c(1674, 1764, 1864, 1964, 2064)
misdemeanor_sample_sizes_to_try <- c(826, 926, 1026, 1126, 1226)

#Store the findings for court types
power_levels_fel <- c()
power_levels_misd <- c()

#Run for felonies
for (i in 1:5) {
  power_levels_fel[i] <- my_power_function(0.40, felony_sample_sizes_to_try[i])
}
power_levels_fel

# Where do we cross 80%?
power_results_fel <- tibble(sample = felony_sample_sizes_to_try,
                        power = power_levels_fel)
power_results_fel

#Plot the results
ggplot(power_results_fel, 
       aes(x = sample, y = power)) +
  geom_line(color = 'red', linewidth = 1.5) + 
  #Add a horizontal line at 80%
  geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
  #Make it look nice
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Sample Size', y = 'Power') +
  ggtitle("40% Conviction")

#Run for 23% recidivism rate
for (i in 1:5) {
  power_levels_misd[i] <- my_power_function(0.23, misdemeanor_sample_sizes_to_try[i])
}
power_levels_misd

# Where do we cross 80%?
power_results_misd <- tibble(sample = misdemeanor_sample_sizes_to_try,
                        power = power_levels_misd)
power_results_misd

#Plot the results
ggplot(power_results_misd, 
       aes(x = sample, y = power)) +
  geom_line(color = 'red', size = 1.5) + 
  #Add a horizontal line at 80%
  geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
  #Make it look nice
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Sample Size', y = 'Power') +
  ggtitle("23% Conviction")

knitr::stitch('Philly Court Watching Power Analysis.r')
