##Load required libraries --------------------------------------------------------------------------------------------------------------------------

library(forcats) # For reordering the plot categories
library(ggplot2) # For creating plots
library(ivmodel) # For running IV analysis
library(patchwork) # For combining the plots with | and +
library(psych)  # For calculating Cronbach's alpha
library(simDAG) # For simulating data
library(tidyverse)

##Simulated data ------------------------------------------------------------------------------------------------------------------------------------

# Set random seed for reproducibility

set.seed(123) 

## Define the two models

# Expressive responding model
dag_express <- empty_dag() +
  node("age", type="rnorm", mean=0, sd=1) +
  node("scept", type="gaussian", parents=c("age"), betas=c(1),
       intercept=0, error=0.5) +
  node("con", type="gaussian", parents=c("scept"), betas=c(-1),
       intercept=0, error=0.5)

# True belief model
dag_true <- empty_dag() +
  node("age", type="rnorm", mean=0, sd=1) +
  node("scept", type="gaussian", parents=c("age","con"), betas=c(1,-1),
       intercept=0, error=0.5) +
  node("con", type="rnorm", mean=0,sd =1)


# Simulate the data
sim_express <- sim_from_dag(dag_express, n_sim=1000)
sim_true <- sim_from_dag(dag_true, n_sim=1000)

# Plot the data
bold_theme <- theme(
  axis.title = element_text(face = "bold"),   # Bolds the X and Y axis titles
  axis.text = element_text(face = "bold"),    # Bolds the numbers on the axes
  legend.title = element_text(face = "bold"), # Bolds the legend title ("Age")
  legend.text = element_text(face = "bold")   # Bolds the numbers in the legend
)

## Expressive Responding Plot
# Plot 1: Age vs. Scepticism
p_exp_1 <- ggplot(sim_express, aes(x = scept, y = age)) +
  geom_point(alpha = 0.6, color = "seagreen") +
  labs(
    x = "Vaccine Scepticism",
    y = "Age"
  ) +
  theme_classic() +
  bold_theme

# Plot 2: Age vs. Conspiracy Beliefs
p_exp_2 <- ggplot(sim_express, aes(x = age, y = con)) +
  geom_point(alpha = 0.6, color = "royalblue") +
  labs(
    x = "Age",
    y = "Conspiracy Beliefs"
  ) +
  theme_classic() +
  bold_theme

# Plot 3: Scepticism vs. Conspiracy Beliefs (coloured by Age)
p_exp_3 <- ggplot(sim_express, aes(x = con, y = scept, color = age)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() + # A nice, perceptually uniform colour scale
  labs(
    x = "Conspiracy Beliefs",
    y = "Vaccine Scepticism",
    color = "Age"
  ) +
  theme_classic() +
  bold_theme

# Combine the plots into a single figure with a title
# Group the individual plots first
combined_plots_exp <- (p_exp_1 | p_exp_2 | p_exp_3) 

# Now, add the annotation to the combined object
figure_expressive <- combined_plots_exp +
  plot_annotation(
    title = "Figure 1a: 'Expressive Responding' Model (Age \U2192 Scepticism \U2192 Conspiracy)",
    # Using Unicode arrows for a cleaner look in the plot title
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

# Display the final combined plot
print(figure_expressive)

ggsave("figs/expressive_responding.png",plot= figure_expressive,dpi=120,width=15, height=5)

## True Belief Plot
# Plot 1: Age vs. Scepticism
p_true_1 <- ggplot(sim_true, aes(x = scept, y = age)) +
  geom_point(alpha = 0.6, color = "seagreen") +
  labs(
    x = "Vaccine Scepticism",
    y = "Age"
  ) +
  theme_classic() +
  bold_theme

# Plot 2: Age vs. Conspiracy Beliefs (should be no correlation by design)
p_true_2 <- ggplot(sim_true, aes(x = age, y = con)) +
  geom_point(alpha = 0.6, color = "royalblue") +
  labs(
    x = "Age",
    y = "Conspiracy Beliefs"
  ) +
  theme_classic() +
  bold_theme

# Plot 3: Scepticism vs. Conspiracy Beliefs (colored by Age)
p_true_3 <- ggplot(sim_true, aes(x = con, y = scept, color = age)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  labs(
    x = "Conspiracy Beliefs",
    y = "Vaccine Scepticism",
    color = "Age"
  ) +
  theme_classic() +
  bold_theme

# Combine the plots into a single figure with a title
# Group the individual plots first
combined_plots_true <- (p_true_1 | p_true_2 | p_true_3)

# Then add the overall annotation
figure_true_belief <- combined_plots_true +
  plot_annotation(
    title = "Figure 1b: 'True Belief' Model (Age \U2192 Scepticism \U2190 Conspiracy)",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

# Display the final combined plot
print(figure_true_belief)

ggsave("figs/true_belief.png",plot= figure_true_belief,dpi=120,width=15, height=5)


## Standardise the data and analyse using IVModel

# Convert DAGs to data frames
sim_express2 <- as.data.frame(sim_express)
sim_true2 <- as.data.frame(sim_true)

# Standardise data
sim_express_scaled <- sim_express2
sim_express_scaled$age <- scale(sim_express2$age)
sim_express_scaled$v_int <- scale(sim_express2$v_int)
sim_express_scaled$con <- scale(sim_express2$con)

sim_true_scaled <- sim_true2
sim_true_scaled$age <- scale(sim_true2$age)
sim_true_scaled$v_int <- scale(sim_true2$v_int)
sim_true_scaled$con <- scale(sim_true2$con)

# Flip the sign of the intention variable so high score = high scepticism
sim_express_scaled$v_int <- sim_express_scaled$v_int * -1
sim_true_scaled$v_int <- sim_true_scaled$v_int * -1

sim_express_iv_analysis <- ivmodel(Y = sim_express_scaled$con, 
                                   D = sim_express_scaled$v_int, 
                                   Z = sim_express_scaled$age)

sim_true_iv_analysis <- ivmodel(Y = sim_true_scaled$con, 
                                D = sim_true_scaled$v_int, 
                                Z = sim_true_scaled$age)

# Print the full summary
summary(sim_express_iv_analysis)
summary(sim_true_iv_analysis)

##Real data ----------------------------------------------------------------------------------------------------------------------------------------

# Import the data
dataloc <- "data/study1_vaccine conspiracy_expressive responding_nums.csv"

df <- read_csv(dataloc)

# Clean the data
df <- df %>%
  rename(Duration = `Duration (in seconds)`) %>%
  mutate_at(c('Duration'), as.integer)

# Find lower quartile of Duration
lower_quartile <- quantile(df$Duration, probs = 0.25)

df <- df %>% filter(Duration> lower_quartile*0.5)

# Attention check & other data quality measures
df <- df %>% 
  filter(Vaccine_conspiracy_7=="2") %>%
  filter(Vaccine_att_7=="5")

# n = 490

# Drop att check columns
df <- df %>%
  select(!Vaccine_conspiracy_7) %>%
  select(!Vaccine_att_7)

# Convert to integers
df <- df %>%
  mutate(across(Vaccine_conspiracy_1:Vaccine_conspiracy_8, as.integer)) %>%
  mutate(across(Vaccine_att_1:Vaccine_att_11, as.integer)) %>%
  mutate(across(Trust_NHS_1:Trust_UKGov_1, as.integer))

df$Year_born <- as.integer(df$Year_born)

# Construct summary variables
df <- df %>%
  mutate(C = rowMeans(select(., starts_with("Vaccine_conspiracy_")), na.rm = TRUE)) %>%
  mutate(T = rowMeans(select(., starts_with("Trust_")), na.rm = TRUE)) %>%
  mutate(Vaccine_att_1 = 8 - Vaccine_att_1) %>% #Reverse code so higher numbers are more sceptical attitudes
  mutate(Vaccine_att_2 = 8 - Vaccine_att_2) %>%
  mutate(V = rowMeans(select(., starts_with("Vaccine_att_")), na.rm = TRUE)) %>%
  mutate(A = 2023 - Year_born)

# Calculate missing Cronbach's Alpha
scepticism_items <- df[, c("Vaccine_att_1", "Vaccine_att_2", "Vaccine_att_3", "Vaccine_att_4", "Vaccine_att_5", "Vaccine_att_6", "Vaccine_att_8", "Vaccine_att_9", "Vaccine_att_10", "Vaccine_att_11")]
alpha_results <- alpha(scepticism_items)
print(alpha_results)

# Calculate standardised coefficient
columns <- c(40,42,43)
data_subset <- df[, columns, with=FALSE]

df_scaled <- data_subset
df_scaled$age <- scale(data_subset$A)
df_scaled$scepticism <- scale(data_subset$V)
df_scaled$conspiracy <- scale(data_subset$C)

# IV Analysis
iv_analysis <- ivmodel(Y = df_scaled$conspiracy, 
                       D = df_scaled$scepticism, 
                       Z = df_scaled$age)

# Print the full summary to see everything
summary(iv_analysis)

# Create a data frame with the results
plot_data <- data.frame(
  model = c("Sim 1: 'Expressive' (Effect is Real)", 
            "Sim 2: 'True Belief' (Effect is Zero)", 
            "Real Data (Effect is Unknown)"),
  estimate = c(0.93, 0.00, 0.91),
  ci_lower = c(0.90, -0.10, 0.15),
  ci_upper = c(0.95, 0.09, 2.28),
  
  # This new column will control the colour
  highlight_group = c("Simulations", "Simulations", "Real Data") 
)

# Manually set the order of the 'model' column by converting it to a factor
plot_data$model <- factor(plot_data$model, levels = c(
  "Real Data (Effect is Unknown)",           # This will be at the bottom of the plot
  "Sim 2: 'True Belief' (Effect is Zero)",   # This will be in the middle
  "Sim 1: 'Expressive' (Effect is Real)"     # This will be at the top
))

# Define the colour palette
my_colours <- c("Simulations" = "gray70", "Real Data" = "#0072B2")

# Create the a horizontal plot with bold labels
ci_plot_horizontal_bold <- ggplot(plot_data, aes(x = estimate, y = model)) +
  
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                 height = 0.1, linewidth = 1, color = "gray40") +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  geom_point(aes(fill = highlight_group), size = 5, shape = 21, color = "black") +
  
  scale_fill_manual(values = my_colours) +
  
  labs(
    title = "Causal Effect of Scepticism on Conspiracy Belief (IV Analysis)",
    x = "Standardized Beta Coefficient (β)",
    y = ""
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    # Centre and bold the plot title
    plot.title = element_text(hjust = 0.5, face = "bold"),
    
    # Bold the axis titles (only X axis has a title)
    axis.title.x = element_text(face = "bold"),
    
    # Bold the text ON the axes (the numbers and model names)
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    
    # Other theme elements
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none"
  )

# Display the plot
print(ci_plot_horizontal_bold)

# Save the plot
ggsave("figs/ci plot bold.png",plot= ci_plot_horizontal_bold,dpi=120)