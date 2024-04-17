# HUEL Satiety Functions

# Define a custom function to calculate mean and standard deviation
calculate_mean_sd <- function(data, question) {
  # Subset the data for the specified question
  question_data <- subset(data, Question == question)
  
  # Calculate mean and standard deviation of 'Measurement' grouped by 'Intervention' and 'Timepoint'
  mean_sd <- aggregate(Measurement ~ Intervention + Timepoint, data = question_data, 
                       FUN = function(x) c(mean = round(mean(x), 1), sd = round(sd(x),1)))
  
  # Convert the Measurement matrix into separate columns
  mean_sd$Mean <- mean_sd$Measurement[, "mean"]
  mean_sd$SD <- mean_sd$Measurement[, "sd"]
  
  # Remove the original Measurement variable
  mean_sd <- mean_sd[, !(names(mean_sd) %in% "Measurement")]
  
  # Convert Timepoint to factor with desired order
  mean_sd$Timepoint <- factor(mean_sd$Timepoint, levels = c("0", "15", "30", "45", "60", "90", "120", "150", "180", "210", "240"))
  
  # Order the data frame by Timepoint
  mean_sd <- mean_sd[order(mean_sd$Timepoint), ]
  
  # Convert to wide
  
  mean_sd <- pivot_wider(mean_sd, names_from = Intervention, values_from = c(Mean, SD))
  
  
  return(mean_sd)
}

# Define a function to create ggplot
plot_mean_sd <- function(data) {
  # Create ggplot
  p <- ggplot(data, aes(x = Timepoint)) +
    geom_line(aes(y = Mean_Cornflakes, color = "Cornflakes"), group = 1) +
    geom_point(aes(y = Mean_Cornflakes, color = "Cornflakes")) +
    geom_errorbar(aes(ymin = Mean_Cornflakes - SD_Cornflakes, ymax = Mean_Cornflakes + SD_Cornflakes, color = "Cornflakes"), width = 0.2) +
    geom_line(aes(y = Mean_HUEL, color = "HUEL"), group = 2) +
    geom_point(aes(y = Mean_HUEL, color = "HUEL")) +
    geom_errorbar(aes(ymin = Mean_HUEL - SD_HUEL, ymax = Mean_HUEL + SD_HUEL, color = "HUEL"), width = 0.2) +
    labs(title = "Mean and Standard Deviation by Timepoint",
         x = "Timepoint", y = "Mean") +
    scale_color_manual(values = c("Cornflakes" = "blue", "HUEL" = "red"),
                       labels = c("Cornflakes", "HUEL")) +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_y_continuous(limits = c(0, 100))
  
  return(p)
}

  

# Function to subset data
subset_data <- function(data, Question){
  subset_data <- data %>%
    filter(Question == !!Question) %>%  # Use !! to unquote the input parameter
    select(Participant, Timepoint, Measurement, Intervention, Question)
  
  return(subset_data)
}

# Function to perform two-way ANOVA, check assumptions, and plot diagnostic graphs
two_way_anova_with_plots <- function(data) {
  # Perform two-way ANOVA
  anova_results <- aov(Measurement ~ Intervention * Timepoint, data = data)
  
  # Check assumptions
  # 1. Homogeneity of variances
  levene_test <- car::leveneTest(residuals(anova_results) ~ Intervention * Timepoint, data = data)
  homogeneity_passed <- levene_test$p.value > 0.05
  
  # Print levene_test and its p-value
  print(levene_test)
  print(levene_test$p.value)
  
  
  # 2. Independence of residuals
  independence_test <- !cor.test(residuals(anova_results)[-1], residuals(anova_results)[-length(residuals(anova_results))])$p.value > 0.05
  
  # Set smaller plot margins
  par(mar = c(4, 4, 2, 2))  # Set top, bottom, left, and right margins
  
  # Diagnostic plots
  plot1 <- plot(anova_results, 1)  # Residuals vs. Fitted
  plot2 <- plot(anova_results, 2)  # Normal Q-Q plot
  plot3 <- plot(anova_results, 3)  # Scale-Location plot
  plot4 <- plot(anova_results, 4)  # Residuals vs. Leverage
  
  # Store plots in a list
  plots <- list(Residuals_vs_Fitted = plot1,
                QQ_Plot = plot2,
                Scale_Location_Plot = plot3,
                Residuals_vs_Leverage = plot4)
  
  # Store ANOVA results and assumptions in a list
  results <- list(
    "ANOVA_results" = summary(anova_results),
    "Homogeneity" = homogeneity_passed,
    "Independence" = independence_test,
    "Plots" = plots
  )
  
  # Print ANOVA results
  print(summary(anova_results))
  
  # Print assumption
  cat("Independence of residuals:", independence_test, "\n")
  
  # Return the results
  return(results)
}

#Function to perform linear mixed model where we account for participants, as well as assumptions


lmm_with_plots <- function(data) {
  # Perform linear mixed-effects model
  lmer_model <- lmer(Measurement ~ Timepoint * Intervention + (1|Participant), data = data)
  
  # Print ANOVA results
  print(anova(lmer_model))
  
  
}

