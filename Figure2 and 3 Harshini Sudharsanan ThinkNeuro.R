if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl") 
library(tidyverse)
library(readxl)
windowsFonts(Helvetica = windowsFont("Arial"))
setwd("C:/Users/FATIMA DIALLO/Desktop/ThinkNeuro_Data")
countries_raw <- read_csv("countries_analysis.csv", show_col_types = FALSE)
print("Generating Figures with Helvetica-Mapped Typography...")
top_countries <- countries_raw %>%
  arrange(desc(Papers)) %>%
  head(10) %>%
  as.data.frame()

scale_factor <- max(top_countries$Total_Citations) / max(top_countries$Papers)

p <- ggplot(top_countries, aes(x = reorder(Country, -Papers))) +
  geom_bar(aes(y = Papers), stat = "identity", fill = "#003366", alpha = 0.85) +
  geom_line(aes(y = Total_Citations / scale_factor, group = 1), color = "#3399FF", linewidth = 1.2) +
  geom_point(aes(y = Total_Citations / scale_factor), color = "#000080", size = 3) +
  scale_y_continuous(name = "Papers (Bar)", sec.axis = sec_axis(~.*scale_factor, name = "Total Citations (Line)")) +
  
  theme_minimal() + 
  labs(title = "Top 10 Countries Contributing to BCi Research", 
       subtitle = " ", 
       x = "Country") +
  theme(
    text = element_text(family = "Helvetica"), 
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Helvetica"),
    axis.title = element_text(family = "Helvetica"),
    plot.title = element_text(family = "Helvetica", face = "bold"),
    plot.subtitle = element_text(family = "Helvetica")
  )

ggsave("Top_Countries_DualAxis_1000dpi.png", plot = p, width = 10, height = 6, dpi = 1000)
print("Execution Complete! Check your folder for your deliverables.")
