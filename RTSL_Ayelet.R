################### Epidemiological Analysis ########################

######### General Steeps:

### Steep 1: Set working directory folder
setwd("C:\\users\\hp\\Desktop\\RSL\\RTSL_Ayele")

### Steep 2: Install important library for anlysis 
library(tidyverse)     #For data manipulation & visualization
library (dplyr)        #For data manipulation
library(tidyr)         # to help to create tidy data
library(gt)            # for table formatting 
library(sf)            # for shape file 
library(ggplot2)       # For custom visualizations
library(readxl)        # to read the excel file for maternal data




#############################################################
########## Weekly Priority Diseases #########################

### Steep 1: Data Generation and Understand the Data
# create data frame 
df_wk <- read.csv("C:\\Users\\hp\\Desktop\\RSL\\data\\weekly_data_by_region.csv")

### Steep 2: Data cleaning
# check and understand the data
str(df_wk)           # Structure of the dataset to get each variables structures including how the question structured and response;
# Rows 340 and 27 Columns, we can use also glimpse(df_wk) 
summary(df_wk)       # Summary statistics for each variables (Minimum value, 1st quartile, mean, 3rd quartile and the maximum value)


#Check the missing values
sum(is.na(df_wk))  # Count total missing values and we have 0 missing value
colSums(is.na(df_wk))  #Count missing values per column gives missing value for each variables (columns). No missing value. 

### Steep 3: Exploratory Data Analysis (EDA)
#Descriptive Statistics:


###### table 1
# 1. Define the diseases we want to keep
selected_diseases <- c(
  "AFP", "Anthrax", "Cholera", "Diarrhoea.Non.Bloody",
  "Malaria", "Measles", "Monkeypox", "Plague", "Typhoid.fever"
)

# 2. Create mapping between disease names and column name patterns
disease_mapping <- list(
  "AFP" = list(
    suspected = "AFP.suspected",
    tested = "AFP.Sent.to.Lab",
    confirmed = "AFP.Confirmed"
  ),
  "Anthrax" = list(
    suspected = "Anthrax.suspected",
    tested = "Anthrax.sent.to.lab",
    confirmed = "Anthrax.confirmed"
  ),
  "Cholera" = list(
    suspected = "Cholera.suspected",
    tested = "Cholera.sent.to.lab",
    confirmed = "Cholera.confirmed"
  ),
  "Diarrhoea.Non.Bloody" = list(
    suspected = "Diarrhoea.Non.Bloody.suspecteded",
    tested = "Diarrhoea.Non.Bloody.sent.to.lab",
    confirmed = "Diarrhoea.Non.Bloody.confirmed"
  ),
  "Malaria" = list(
    suspected = "Malaria.suspected",
    tested = "Malaria.sent.to.lab",
    confirmed = "Malaria.confirmed"
  ),
  "Measles" = list(
    suspected = "Measles.suspected",
    tested = "Measles.sent.to.lab",
    confirmed = "Measles.confirmed"
  ),
  "Monkeypox" = list(
    suspected = "Monkeypox.suspected",
    tested = "Monkeypox.sent.to.lab",
    confirmed = "Monkeypox.confirmed"  # Note lowercase if that's what exists
  ),
  "Plague" = list(
    suspected = "Plague.suspected",
    tested = "Plague.sent.to.Lab",  # Capital L to match actual data
    confirmed = "Plague.confirmed"
  ),
  "Typhoid.fever" = list(
    suspected = "Typhoid.fever.suspected",
    tested = "Typhoid.fever.sent.to.Lab",
    confirmed = "Typhoid.fever.confirmed"
  )
)
# 3. function to handle potential missing columns
process_disease <- function(data, disease_name) {
  cols <- disease_mapping[[disease_name]]
  
  # Helper function to safely sum columns
  safe_sum <- function(col_name, df = data) {
    if (col_name %in% names(df)) sum(df[[col_name]], na.rm = TRUE) else 0
  }
  
  # Filter for EXACT week 34 match
  week34_data <- data %>%
    filter(period == "2024W34") %>%  # Exact match for week 34
    summarise(
      Week_34_Suspected = safe_sum(cols$suspected, .),
      Week_34_Tested = safe_sum(cols$tested, .),
      Week_34_Confirmed = safe_sum(cols$confirmed, .)
    )
  
  # Calculate cumulative totals from ALL weeks
  cumulative_data <- data %>%
    summarise(
      Cumulative_Suspected = safe_sum(cols$suspected),
      Cumulative_Tested = safe_sum(cols$tested),
      Cumulative_Confirmed = safe_sum(cols$confirmed)
    )
  
  # Combine results
  bind_cols(week34_data, cumulative_data) %>%
    mutate(Disease = case_when(
      disease_name == "Diarrhoea.Non.Bloody" ~ "Non-bloody Diarrhoea",
      disease_name == "Typhoid.fever" ~ "Typhoid Fever",
      TRUE ~ disease_name
    ))
}
# 4. Process each disease and combine
final_table <- bind_rows(
  lapply(selected_diseases, function(d) process_disease(df_wk, d))
) %>%
  select(Disease, everything()) %>%
  arrange(factor(Disease, levels = c(
    "AFP", "Anthrax", "Cholera", "Non-bloody Diarrhoea",
    "Malaria", "Measles", "Monkeypox", "Plague", "Typhoid Fever"
  )))
# 5. Format column names to match desired output
final_table <- final_table %>%
  rename(
    `Week 34_Suspected` = Week_34_Suspected,
    `Week 34_Tested` = Week_34_Tested,
    `Week 34_Confirmed` = Week_34_Confirmed,
    `Week 1 to 34, Cumulative Total_Suspected` = Cumulative_Suspected,
    `Week 1 to 34, Cumulative Total_Tested` = Cumulative_Tested,
    `Week 1 to 34, Cumulative Total_Confirmed` = Cumulative_Confirmed
  )

# 6. Create the final formatted table
final_formatted_table <- final_table %>%
  gt::gt() %>%
  gt::tab_spanner(
    label = "Week 34",
    columns = 2:4
  ) %>%
  gt::tab_spanner(
    label = "Week 1 to 34, Cumulative Total",
    columns = 5:7
  ) %>%
  gt::fmt_number(
    columns = 2:7,
    decimals = 0
  ) %>%
  gt::cols_label(
    Disease = "Disease/Event/Condition",
    `Week 34_Suspected` = "Suspected",
    `Week 34_Tested` = "Tested",
    `Week 34_Confirmed` = "Confirmed",
    `Week 1 to 34, Cumulative Total_Suspected` = "Suspected",
    `Week 1 to 34, Cumulative Total_Tested` = "Tested",
    `Week 1 to 34, Cumulative Total_Confirmed` = "Confirmed"
  ) %>%
  gt::cols_align(
    align = "center",
    columns = everything()
  )

# Display the final table
final_formatted_table



########################################################
############ Measles Lab data#########################

### Steep 1: Data Generation and Understand the Data
# Create data frame
df_measle <- read.csv("C:\\Users\\hp\\Desktop\\RSL\\data\\measles_lab.csv")

### Steep 2: Data cleaning
# check and understand the data
str(df_measle)           # Structure of the data-set 
summary(df_measle)       # Summary statistics for each variables 

#Check the missing values
sum(is.na(df_measle))  # Count total missing values and we have 0 missing value
colSums(is.na(df_measle))  #Count missing values per column gives missing value for each variables (columns). No missing value. 


### Steep 3: Data visualization (stacked column)

# Data preparation
df_clean <- na.omit(df_measle[, c("Province.Of.Residence", "IgM.Results")])
df_clean$IgM.Results <- factor(df_clean$IgM.Results,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Positive", "Negative", "Indeterminate",
                                          "Not done", "Pending"))

# Create frequency table
result_table <- table(df_clean$Province.Of.Residence, df_clean$IgM.Results)
all_categories <- c("Positive", "Negative", "Indeterminate", "Not done", "Pending")
result_table <- result_table[, match(all_categories, colnames(result_table))]
result_table[is.na(result_table)] <- 0

# Set colors
colors <- c(
  "Positive" = "#e41a1c",    # Red 
  "Negative" = "#08519c",      # Darker/stronger blue
  "Indeterminate" = "#a6d854", # Lighter Green
  "Not done" = "#d3d3d3",    # Lighter Grey
  "Pending" = "#984ea3")     # Light Purple


# Adjust plot margins (bottom, left, top, right)
par(mar = c(7, 4, 4, 10), xpd = TRUE)

# Create plot with specified y-axis
barplot(t(result_table),
        main = "Measles IgM Test Results by Region",
        ylab = "Number of Tests",
        col = colors,
        ylim = c(0, 150),  # Fixed y-axis limit
        yaxt = "n",  # Remove default y-axis
        cex.names = 0.8,
        las = 2,
        space = 0.2,
        width = 0.6)

# Add custom y-axis with specified breaks
axis(2, at = seq(0, 150, by = 50), las = 1)

# Add legend in top-right corner
legend(x = nrow(result_table)*0.9,
       y = 150,  # Matches ylim max
       legend = all_categories,
       fill = colors,
       title = "Test Result",
       cex = 0.8,
       bty = "n")

# Add zero line
abline(h = 0, col = "gray40")

# Reset graphical parameters
par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)


##################################################################
####################### Maternal data ############################

### Steep 1: Data Generation and Understand the Data
# Create data frame
df_mater <- read.csv("C:\\Users\\hp\\Desktop\\RSL\\data\\Maternal_deaths.csv")
str(df_mater) 

### Steep 2: Data cleaning
# check and understand the data
str(df_mater)           # Structure of the data-set 
summary(df_mater)       # Summary statistics for each variables 

#Check the missing values
sum(is.na(df_mater))      # Count total missing values and we have 0 missing value
colSums(is.na(df_mater))  #Count missing values per column gives missing value for each variables (columns). No missing value. 


### Steep 3: Bar Plot 

# Standardize cause names
df_mater <- df_mater %>%
  mutate(Short.Name = case_when(
    grepl("(?i)Non[- ]?obstetric complication", Short.Name) ~ "Non-obstetric complications",
    grepl("(?i)Obstetric hemorrhage", Short.Name) ~ "Obstetric haemorrhage",
    TRUE ~ Short.Name
  ))

# Calculate percentage
shortname_pct <- df_mater %>%
  count(Short.Name) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Horizontal bar plot
ggplot(shortname_pct, aes(x = reorder(Short.Name, percentage), 
                          y = percentage)) +
  geom_bar(stat = "identity", fill = "#2b8cbe") +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.1, size = 3) +
  labs(
    title = "Distribution of Maternal Deaths by Cause",
    x = "Short Name",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#######################
### steep 4 shape file 

# create data frame 
eth_region_shape <- st_read("C:\\Users\\hp\\Desktop\\RSL\\data\\shapefiles\\eth_admbnda_adm1_csa_bofedb_2021.shp")

# to understand on the variable 
str(eth_region_shape)  

# To view the top list from the shape file
head(eth_region_shape, 3)

# To view the shape file by region and its futures 
plot(eth_region_shape)

####################### work out the question 
# read excel file
df_total_death <- read_xlsx("C:\\Users\\hp\\Desktop\\RSL\\data\\Maternal deaths.xlsx", sheet = "MD summary")
total_m_death <- df_total_death %>%
  group_by(region) %>%
  summarise(`Total_Maternal_Death`)

# Join shape file data with maternal deaths
eth_map_data <- eth_region_shape %>%
  left_join(total_m_death, by = c("ADM1_EN" = "region"))

# Replace NA with NA
eth_map_data$Total_Maternal_Death[is.na(eth_map_data$Total_Maternal_Death)] <- NA

# Plot
ggplot(eth_map_data) +
  geom_sf(aes(fill = Total_Maternal_Death), color = "black", size = 0.3) +
  scale_fill_gradient(
    name = "Cumulative Maternal Deaths",
    low = "#ece7f2",
    high = "#2b0080",
    na.value = "white"
  ) +
  geom_sf_text(
    aes(label = paste0(
      ADM1_EN, "(", 
      ifelse(is.na(Total_Maternal_Death), "NA", Total_Maternal_Death), 
      ")"
    )),
    size = 3
  ) +
  theme_minimal() +
  theme(legend.position = "left")































