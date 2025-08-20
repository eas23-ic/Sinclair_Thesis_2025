#set wd

setwd("C:/Users/User/Downloads")

#Download necassary packages and then the master dataset 
library(readr)
library(bipartite)
library(reshape2)
library(dplyr)
library(ggplot2)
library(nlme)

# Load the CSV file
full_dataset <- read_csv("GC_Working_spreadsheet_BB_observ_181921222324(in).csv")

#filter for columns

bee_filtered_all <- full_dataset %>%
  select(
    Sample_No,
    Year,
    Activity_Flying_Foraging,
    Bombus_Species,
    Flower_Species,
    Bombus_Species_Certainty,
    Caste_Bee,
    Caste_Certainty,
    Head_width_mm,
    ITD_mm,
    Wing_length_mm
    
  )

# Step 3: View the first few rows to confirm
head(bee_filtered_all)

# Filter for rows where Activity_Flying_Foraging is "Foraging"
bee_filtered_all <- bee_filtered_all %>%
  filter(Activity_Flying_Foraging == "Foraging")

# Filter out rows where Bombus_Species is NA
bee_filtered_all <- bee_filtered_all %>%
  filter(!is.na(Bombus_Species))

# Filter out rows where Bee species Certainty.1..3. is lower than 2 or 3
bee_filtered_all <- bee_filtered_all %>%
  filter(Bombus_Species_Certainty %in% c(2, 3))

# Check the result
head(bee_filtered_all)

#Filter out rows where Caste certainty is less than 2 or 3
bee_filtered_all <- bee_filtered_all %>%
  filter(Caste_Certainty %in% c(2, 3))

# Check the result
head(bee_filtered_all)

#check for inconclusive species string i.e nothing where multiple species are entered
unique(bee_filtered_all$Bombus_Species)

#Filter out the inconclusive species strings

# Define the list of Bombus_Species_ID values to exclude
exclude_species <- c(
  "alpinus/polaris/monticola",
  "jonellus/lucorum",
  "balteatus/lucorum",
  "monticola/lapponicus",
  "bohemicus",
  "soroeensis",
  "wurflenii",
  "hortorum",
  "hyperboreus",
  "lucorum/sporadicus",
  "hypnorum/cingulatus",
  "norvegicus/sylvestris",
  "flavidus",
  "hortorum/jonellus",
  "cingulatus",
  "balteatus/lucorum",
  "hypnorum"
)

# Filter out those rows
bee_filtered_all <- bee_filtered_all %>%
  filter(!Bombus_Species %in% exclude_species)

# Check remaining unique species
unique(bee_filtered_all$Bombus_Species)

#check to see the unquie enteries in the Caste_bee column so it can be filtered properly
unique(bee_filtered_all$Caste_Bee)

#filter out the males and unwanted strings
bee_filtered_all <- bee_filtered_all %>%
  filter(
    !is.na(Caste_Bee) & 
      !Caste_Bee %in% c("D", "W/D", "M")
  )

# Check the result
unique(bee_filtered_all$Caste_Bee)


#check the unique strings under the Flower_Species column
unique(bee_filtered_all$Flower_Species)

#Filter out all the flower NAs 
bee_filtered_all <- bee_filtered_all %>%
  filter(!is.na(Flower_Species))

#Filter out any inconclusive species strings
bee_filtered_all <- bee_filtered_all %>%
  filter(Flower_Species != "Unknown")

# Filter out unwanted Flower_Species values
bee_filtered_all <- bee_filtered_all %>%
  filter(!Flower_Species %in% c("Did not see which plant individual was being foraged on", 
                                "Whiteflower", 
                                "?"))

# Update values in Flower_Species column
bee_filtered_all <- bee_filtered_all %>%
  mutate(Flower_Species = case_when(
    Flower_Species %in% c("Salix", "Salix ?") ~ "Salix spp.",
    TRUE ~ Flower_Species
  ))

# View to confirm
unique(bee_filtered_all$Flower_Species)

#Split the whole dataset into years

# Ensure Year is treated as a factor or numeric
bee_filtered_all$Year <- as.factor(bee_filtered_all$Year)

# Split the dataset by Year into a named list of data frames
bee_by_year <- split(bee_filtered_all, bee_filtered_all$Year)

# Optionally assign each year to its own variable
bee_2019 <- bee_by_year[["2019"]]
bee_2021 <- bee_by_year[["2021"]]
bee_2022 <- bee_by_year[["2022"]]
bee_2023 <- bee_by_year[["2023"]]
bee_2024 <- bee_by_year[["2024"]]

#Filtering each year for their own flowers interactions

#2019

#check the unique strings under the Flower_Species column
unique(bee_2019$Flower_Species)

# Count the number of interactions per flower species
flower_interaction_counts <- bee_2019 %>%
  count(Flower_Species, sort = TRUE)

# View the result
print(flower_interaction_counts, n = Inf)

#All rare salix species go into Salix spp. 
bee_2019 <- bee_2019 %>%
  mutate(Flower_Species = case_when(
    Flower_Species %in% c("Salix_myrsinites", 
                          "Salix_hastata", "Salix_myrsinifolia", "Salix_phylicifolia", "Salix_glauca", "Salix_lanata") ~ "Salix spp.",
    TRUE ~ Flower_Species  # keep all other values unchanged
  ))

#Creating the Rare Species node:
# Step 1: Count flower species interactions
flower_counts_2019 <- bee_2019 %>%
  count(Flower_Species)

# Step 2: Join back to original data and reclassify rare flowers
bee_2019 <- bee_2019 %>%
  left_join(flower_counts_2019, by = "Flower_Species") %>%
  mutate(Flower_Species = if_else(n < 15, "Rare Species", Flower_Species)) %>%
  select(-n)  # Remove the count column if no longer needed

unique(bee_2019$Flower_Species)

#2021

#check the unique strings under the Flower_Species column
unique(bee_2021$Flower_Species)

# Count the number of interactions per flower species
flower_interaction_counts <- bee_2021 %>%
  count(Flower_Species, sort = TRUE)

# View the result
print(flower_interaction_counts, n = Inf)

#All rare salix species go into Salix spp. 
bee_2021 <- bee_2021 %>%
  mutate(Flower_Species = case_when(
    Flower_Species %in% c("Salix_myrsinites", 
                          "Salix_hastata", "Salix_myrsinifolia", "Salix_phylicifolia", "Salix_glauca", "Salix_lanata") ~ "Salix spp.",
    TRUE ~ Flower_Species  # keep all other values unchanged
  ))

#Creating the Rare Species node:
# Step 1: Count flower species interactions
flower_counts_2021 <- bee_2021 %>%
  count(Flower_Species)

# Step 2: Join back to original data and reclassify rare flowers
bee_2021 <- bee_2021 %>%
  left_join(flower_counts_2021, by = "Flower_Species") %>%
  mutate(Flower_Species = if_else(n < 15, "Rare Species", Flower_Species)) %>%
  select(-n)  # Remove the count column if no longer needed

#2022

unique(bee_2022$Flower_Species)

# Count the number of interactions per flower species
flower_interaction_counts <- bee_2022 %>%
  count(Flower_Species, sort = TRUE)

# View the result
print(flower_interaction_counts, n = Inf)

#All salix species go into Salix spp. 
bee_2022 <- bee_2022 %>%
  mutate(Flower_Species = case_when(
    Flower_Species %in% c("Salix_myrsinites", 
                          "Salix_hastata", "Salix_myrsinifolia", "Salix_phylicifolia", "Salix_glauca", "Salix_lanata") ~ "Salix spp.",
    TRUE ~ Flower_Species  # keep all other values unchanged
  ))

#Creating the Rare Species node:
# Step 1: Count flower species interactions
flower_counts_2022 <- bee_2022 %>%
  count(Flower_Species)

# Step 2: Join back to original data and reclassify rare flowers
bee_2022 <- bee_2022 %>%
  left_join(flower_counts_2022, by = "Flower_Species") %>%
  mutate(Flower_Species = if_else(n < 15, "Rare Species", Flower_Species)) %>%
  select(-n)  # Remove the count column if no longer needed

#2023

unique(bee_2023$Flower_Species)

# Count the number of interactions per flower species
flower_interaction_counts <- bee_2023 %>%
  count(Flower_Species, sort = TRUE)

# View the result
print(flower_interaction_counts, n = Inf)

#All rare salix species go into Salix spp. 
bee_2023 <- bee_2023 %>%
  mutate(Flower_Species = case_when(
    Flower_Species %in% c("Salix_myrsinites", 
                          "Salix_hastata", "Salix_myrsinifolia", "Salix_phylicifolia", "Salix_glauca", "Salix_lanata") ~ "Salix spp.",
    TRUE ~ Flower_Species  # keep all other values unchanged
  ))

#Creating the Rare Species node:
# Step 1: Count flower species interactions
flower_counts_2023 <- bee_2023 %>%
  count(Flower_Species)

# Step 2: Join back to original data and reclassify rare flowers
bee_2023 <- bee_2023 %>%
  left_join(flower_counts_2023, by = "Flower_Species") %>%
  mutate(Flower_Species = if_else(n < 15, "Rare Species", Flower_Species)) %>%
  select(-n)  # Remove the count column if no longer needed

#2024

unique(bee_2024$Flower_Species)

# Count the number of interactions per flower species
flower_interaction_counts <- bee_2024 %>%
  count(Flower_Species, sort = TRUE)

# View the result
print(flower_interaction_counts, n = Inf)

#All rare salix species go into Salix spp. 
bee_2024 <- bee_2024 %>%
  mutate(Flower_Species = case_when(
    Flower_Species %in% c("Salix_myrsinites", 
                          "Salix_hastata", "Salix_myrsinifolia", "Salix_phylicifolia", "Salix_glauca", "Salix_lanata") ~ "Salix spp.",
    TRUE ~ Flower_Species  # keep all other values unchanged
  ))

#Creating the Rare Species node:
# Step 1: Count flower species interactions
flower_counts_2024 <- bee_2024 %>%
  count(Flower_Species)

# Step 2: Join back to original data and reclassify rare flowers
bee_2024 <- bee_2024 %>%
  left_join(flower_counts_2024, by = "Flower_Species") %>%
  mutate(Flower_Species = if_else(n < 15, "Rare Species", Flower_Species)) %>%
  select(-n)  # Remove the count column if no longer needed

#Doing 2018 seperately, becuase it was entered slightly differently that year where no be caste was rated 
bee_filtered_2018 <- full_dataset %>%
  filter(Year == 2018)

bee_filtered_2018 <- bee_filtered_2018 %>%
  select(
    Sample_No,
    Year,
    Activity_Flying_Foraging,
    Bombus_Species,
    Flower_Species,
    Bombus_Species_Certainty,
    Caste_Bee,
    Caste_Certainty,
    Head_width_mm,
    ITD_mm,
    Wing_length_mm
    
  )

# Filter for rows where Activity_Flying_Foraging is "Foraging"
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(Activity_Flying_Foraging == "Foraging")

# Filter out rows where Bombus_Species is NA
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(!is.na(Bombus_Species))

# Filter out rows where Bee species Certainty.1..3. is lower than 2 or 3
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(Bombus_Species_Certainty %in% c(2, 3))

# Check the result
head(bee_filtered_2018)

#check for inconclusive species string i.e nothing where multiple species are entered
unique(bee_filtered_2018$Bombus_Species)

# Filter out those rows
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(!Bombus_Species %in% exclude_species)

#filter out the males and unwanted strings
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(
    !is.na(Caste_Bee) & 
      !Caste_Bee %in% c("D", "W/D", "M", "Q/D")
  )

# Check the result
unique(bee_filtered_2018$Caste_Bee)

#check the unique strings under the Flower_Species column
unique(bee_filtered_2018$Flower_Species)

#Filter out all the flower NAs 
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(!is.na(Flower_Species))

#Filter out any inconclusive species strings
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(Flower_Species != "Unknown")

# Filter out unwanted Flower_Species values
bee_filtered_2018 <- bee_filtered_2018 %>%
  filter(!Flower_Species %in% c("Yellow flower"))

# Count the number of interactions per flower species
flower_interaction_counts <- bee_filtered_2018 %>%
  count(Flower_Species, sort = TRUE)

unique(bee_filtered_2018$Flower_Species)

# View the result
print(flower_interaction_counts)
print(flower_interaction_counts, n = Inf)

#All rare salix species go into Salix spp. 
bee_filtered_2018 <- bee_filtered_2018 %>%
  mutate(Flower_Species = case_when(
    Flower_Species %in% c("Salix_myrsinites", 
                          "Salix_hastata", "Salix_myrsinifolia", "Salix_lanata", "Salix_glauca", "Salix_phylicifolia") ~ "Salix spp.",
    TRUE ~ Flower_Species  # keep all other values unchanged
  ))

#Creating the Rare Species node:
# Step 1: Count flower species interactions
flower_counts <- bee_filtered_2018 %>%
  count(Flower_Species)

# Step 2: Join back to original data and reclassify rare flowers
bee_filtered_2018 <- bee_filtered_2018 %>%
  left_join(flower_counts, by = "Flower_Species") %>%
  mutate(Flower_Species = if_else(n < 15, "Rare Species", Flower_Species)) %>%
  select(-n)  

#Creating Functional Identity nodes using multidimensional trait space

#Bind all years together so that to create a pca 

bee_filtered_2018 <- bee_filtered_2018 %>% mutate(Year = as.numeric(as.character(Year)))
bee_2019 <- bee_2019 %>% mutate(Year = as.numeric(as.character(Year)))
bee_2021 <- bee_2021 %>% mutate(Year = as.numeric(as.character(Year)))
bee_2022 <- bee_2022 %>% mutate(Year = as.numeric(as.character(Year)))
bee_2023 <- bee_2023 %>% mutate(Year = as.numeric(as.character(Year)))
bee_2024 <- bee_2024 %>% mutate(Year = as.numeric(as.character(Year)))


bee_filtered_all <- bind_rows(
  bee_filtered_2018,
  bee_2019,
  bee_2021,
  bee_2022,
  bee_2023,
  bee_2024
)

#Filter out rows with missing measurements
bee_pca_data <- bee_filtered_all %>%
  filter(!is.na(ITD_mm), 
         !is.na(Head_width_mm), 
         !is.na(Wing_length_mm))

#Select the columns needed for PCA
pca_input <- bee_pca_data %>%
  select(ITD_mm, Head_width_mm, Wing_length_mm)

#Run PCA 
bee_pca <- prcomp(pca_input, scale. = TRUE)

#Extract PC1 scores and combine with bee_id 
pc1_scores <- as.data.frame(bee_pca$x[, 1, drop = FALSE]) %>%
  rename(PC1 = PC1) %>%
  mutate(Sample_No = bee_pca_data$Sample_No)

#Join the PC1 scores back to the original dataset
bee_filtered_all <- bee_filtered_all %>%
  left_join(pc1_scores, by = "Sample_No")

# View first few PC1 scores
head(bee_filtered_all$PC1)

#filter for only bees that have a PC1 score 

bee_pca_all <- bee_filtered_all %>%
  filter(!is.na(PC1))

#Creating plots to show the distribution of PC1 scores across bumblebee species

library(MetBrewer)

# Assign colors
bee_colors <- met.brewer("VanGogh1", 8)
names(bee_colors) <- c("lucorum", "pascuorum", "jonellus", "pratorum",
                       "balteatus", "lapponicus", "monticola", "alpinus/polaris")

bee_pca_all <- bee_pca_all %>%
  mutate(Bombus_Species = factor(Bombus_Species, 
                                 levels = names(bee_colors)))

bee_pca_all$PC1 <- -bee_pca_all$PC1

#Box
ggplot(bee_pca_all, aes(x = Bombus_Species, y = PC1, fill = Bombus_Species)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = bee_colors) +
  labs(
    x = "Bombus Species",
    y = "PC1 Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

#Jitter
ggplot(bee_pca_all, aes(x = Bombus_Species, y = PC1, color = Bombus_Species)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  scale_color_manual(values = bee_colors) +
  labs(
    x = "Bombus Species",
    y = "PC1 Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

#Creating 8 functional trait clusters to view the same bumblebee comminuty through a functional lense

set.seed(123)

kmeans_result <- kmeans(bee_pca_all$PC1, centers = 8)

bee_pca_all <- bee_pca_all %>%
  mutate(PC1_cluster = kmeans_result$cluster)

#Compute mean PC1 score per original cluster
cluster_order <- bee_pca_all %>%
  group_by(PC1_cluster) %>%
  summarise(mean_PC1 = mean(PC1, na.rm = TRUE)) %>%
  arrange(mean_PC1) %>%
  mutate(Ordered_Cluster = row_number())

# Step 2: Join back the new ordered cluster numbers
bee_pca_all <- bee_pca_all %>%
  left_join(cluster_order, by = "PC1_cluster") %>%
  mutate(PC1_cluster = as.factor(Ordered_Cluster)) %>%
  select(-Ordered_Cluster)  # optional, if you want to clean up

#Getting the clsuter parameters and putting them in the correct order

#Add original cluster assignments 
bee_with_clusters <- bee_pca_all %>%
  mutate(Original_Cluster = kmeans_result$cluster)

#Compute mean PC1 for each original cluster and rank them
cluster_order <- bee_with_clusters %>%
  group_by(Original_Cluster) %>%
  summarise(mean_PC1 = mean(PC1, na.rm = TRUE)) %>%
  arrange(mean_PC1) %>%
  mutate(New_Cluster = row_number())

#Join back to assign new ordered cluster numbers
bee_with_clusters <- bee_with_clusters %>%
  left_join(cluster_order, by = c("Original_Cluster" = "Original_Cluster")) %>%
  mutate(PC1_cluster_ordered = as.factor(New_Cluster))

#Summarize PC1 ranges for new cluster labels
cluster_summary <- bee_with_clusters %>%
  group_by(PC1_cluster_ordered) %>%
  summarise(
    min_PC1 = min(PC1, na.rm = TRUE),
    max_PC1 = max(PC1, na.rm = TRUE),
    mean_PC1 = mean(PC1, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(as.numeric(PC1_cluster_ordered))

print(cluster_summary)

#Creating a plot to show the distribution of PC1 scores in their functional trait clusters

#PC1 ranges per cluster
cluster_ranges <- bee_pca_all %>%
  group_by(PC1_cluster) %>%
  summarise(min_PC1 = round(min(PC1), 2),
            max_PC1 = round(max(PC1), 2)) %>%
  mutate(label = paste0(min_PC1, " to ", max_PC1)) %>%
  arrange(min_PC1)  # Order from low to high PC1

#Map cluster number to range label
cluster_label_map <- setNames(cluster_ranges$label, cluster_ranges$PC1_cluster)

#Update dataset with new factor levels for labels
bee_pca_all <- bee_pca_all %>%
  mutate(Cluster_Label = factor(cluster_label_map[as.character(PC1_cluster)],
                                levels = cluster_ranges$label))  # Ensure proper order

#Assign colors 
cluster_colors <- setNames(met.brewer("Hiroshige", 8), cluster_ranges$label)

#Box Plot
ggplot(bee_pca_all, aes(x = Cluster_Label, y = PC1, fill = Cluster_Label)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +
  scale_fill_manual(values = cluster_colors) +
  labs(
    x = "PC1 Cluster Range",
    y = "PC1 Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 30, hjust = 1)
  )

#Jitter Plot
ggplot(bee_pca_all, aes(x = Cluster_Label, y = PC1, color = Cluster_Label)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  scale_color_manual(values = cluster_colors) +
  labs(
    x = "PC1 Cluster Range",
    y = "PC1 Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 30, hjust = 1)
  )

#Split the dataset by Year
bee_pca_2018 <- subset(bee_pca_all, Year == 2018)
bee_pca_2019 <- subset(bee_pca_all, Year == 2019)
bee_pca_2021 <- subset(bee_pca_all, Year == 2021)
bee_pca_2022 <- subset(bee_pca_all, Year == 2022)
bee_pca_2023 <- subset(bee_pca_all, Year == 2023)
bee_pca_2024 <- subset(bee_pca_all, Year == 2024)

