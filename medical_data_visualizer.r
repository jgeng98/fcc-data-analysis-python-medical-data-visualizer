library(tidyverse)
library(reshape2)

df <- read_csv("medical_examination.csv")

# Function to calculate BMI (weight in kg divided by square height in m)
bmi_calc <- function(weight, height) {
    if ((weight / (height / 100)^2) > 25) {
        return(1)
    } else {
        return(0)
    }
}

# Add overweight column to the data based on BMI
# BMI > 25 -> 1 (overweight)
# BMI < 25 -> 0 (not overweight)
df$overweight <- mapply(bmi_calc, df$weight, df$height)


# Normalize the data
# cholesterol > 1 -> 1, gluc > 1 -> 1 (bad)
# cholesterol = 1 -> 0, gluc = 1 -> 0 (good)
df$cholesterol <- with(df, ifelse(cholesterol > 1, 1, 0))
df$gluc <- with(df, ifelse(gluc > 1, 1, 0))


# Create a categorical barplot faceted on cardio, showing the totals of the 6 numeric variables (active, alco, cholesterol, gluc, overweight, smoke)
df_cat <- df %>% dplyr::select(cardio, cholesterol, gluc, smoke, alco, active, overweight)

df_cat <- df_cat %>% pivot_longer(!cardio, names_to = "variable")

df_cat <- df_cat %>%
    group_by(cardio, variable, value) %>%
    summarise(total = n())

df_cat %>% ggplot(aes(x = variable, y = total)) +
    geom_bar(aes(fill = factor(value)), position = "dodge", stat = "identity") +
    facet_grid(~cardio, labeller = label_both) +
    labs(x = "Variable", y = "Total", fill = "Value")


# Clean the data
# Remove patients whose ap_lo > ap_hi
df_heat <- df %>% filter(ap_lo <= ap_hi)

# Remove patients with heights that are below the 2.5th percentile or above the 97.5th percentile
df_heat <- df_heat %>% filter((height >= quantile(df$height, 0.025)) & (height <= quantile(df$height, 0.975)))

# Remove patients with weights that are above the 2.5th percentile or above the 97.5th percentile
df_heat <- df_heat %>% filter((weight >= quantile(df$weight, 0.025)) & (weight <= quantile(df$weight, 0.975)))

# Create a correlation matrix
corr_mat <- round(cor(df_heat), 2)

# Mask the upper triangle of the correlation matrix
get_lower_tri <- function(corr_mat) {
    corr_mat[upper.tri(corr_mat)] <- NA
    return(corr_mat)
}

corr_mat <- melt(get_lower_tri(corr_mat), na.rm = TRUE)

# Plot the masked correlation matrix in a heatmap
corr_mat %>% ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(Var1, Var2, label = value), color = "white") +
    labs(fill = "Correlation") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())