##Interannual Turnover Stats##

#Run Data Filtering script first

#packages
library(car)        
library(emmeans)    
library(effectsize) 
library(broom)
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

##Annual species and PC1 cluster composition##

#Plotting taxonomic composition bar graphs

# Define your custom color mapping for Bombus species
species_colors <- c(
  "lucorum" = "#4D4F7C",
  "pascuorum" = "#61658E",
  "jonellus" = "#757BA1",
  "pratorum" = "#878FB0",
  "balteatus" = "#9EB0C5",
  "lapponicus" = "#A6C1B3",
  "monticola" = "#90A878",
  "alpinus/polaris" = "#72945B"
)

#Prepare taxonomic data
taxonomic_summary <- bee_filtered_all %>%
  mutate(Bombus_Species = factor(Bombus_Species, levels = names(species_colors))) %>%
  group_by(Year, Bombus_Species) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(percentage = count / sum(count) * 100)

#Create the plot
ggplot(taxonomic_summary, aes(x = factor(Year), y = percentage, fill = Bombus_Species)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(
    x = "Year",
    y = "Percentage of Bee Species",
    fill = "Bombus Species"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

#Plotting Functional composition bar graph

# Define PC1 cluster labels (ordered 1–8)
cluster_labels <- sort(unique(bee_pca_all$Cluster_Label))

#Assign colours
cluster_colors <- colorRampPalette(met.brewer("Hiroshige"))(length(cluster_labels))
names(cluster_colors) <- cluster_labels

#Prepare data 
functional_summary <- bee_pca_all %>%
  mutate(Cluster_Label = factor(Cluster_Label, levels = cluster_labels)) %>%
  group_by(Year, Cluster_Label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(percentage = count / sum(count) * 100)

#Create the functional composition plot
ggplot(functional_summary, aes(x = factor(Year), y = percentage, fill = Cluster_Label)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cluster_colors) +
  labs(
    x = "Year",
    y = "Percentage of PC1 Clusters",
    fill = "PC1 Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

##Type II ANOVA for PC1 interannual trait turnover##

# keep complete cases for the variables we need
dat <- bee_pca_all %>%
  select(PC1, Year, Caste_Bee) %>%
  filter(!is.na(PC1), !is.na(Year), !is.na(Caste_Bee)) %>%
  mutate(
    Year      = factor(Year),          # treat Year as a factor for ANOVA
    Caste_Bee = factor(Caste_Bee)      # ensure it’s a factor (e.g., W, Q/W, Q)
  )

m_main <- lm(PC1 ~ Year + Caste_Bee, data = dat)

# Type-II ANOVA is appropriate when there’s no interaction term
anova_main <- car::Anova(m_main, type = 2)
anova_main

#Checking residuals 

par(mfrow = c(2,2))
plot(m_main)        # or plot(m_int) if you used the interaction model
par(mfrow = c(1,1))

# Homogeneity of variances across years (Levene’s test)
car::leveneTest(PC1 ~ Year, data = dat)

# Normality (large samples: rely more on the QQ-plot than a Shapiro p-value)
qqnorm(residuals(m_main)); qqline(residuals(m_main))

#EMMs 
emm_year <- emmeans(m_main, ~ Year)  # or m_int if you included interaction, but see note below
pairs(emm_year, adjust = "tukey")    # Tukey across all years
summary(emm_year)

#table

tab <- broom::tidy(anova_main) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

tab

#Adjust the Sd to make the model more robust

bee_pca_all$Year <- factor(bee_pca_all$Year)
fit <- lm(PC1 ~ Year + Caste_Bee, data = bee_pca_all)
car::Anova(fit, type = "II", white.adjust = "hc3")
fit <- lm(PC1 ~ Year + Caste_Bee, data = bee_pca_all)

# Robust (HC3) coefficient tests
library(lmtest); library(sandwich)
coeftest(fit, vcov = vcovHC(fit, type = "HC3"))

# Robust Type II ANOVA
library(car)
Anova(fit, type = "II", white.adjust = "hc3")

#Re-testing emms for this more robust model 

fit_fac <- lm(PC1 ~ Year + Caste_Bee, data = bee_pca_all)

# Robust omnibus you already ran:
car::Anova(fit_fac, type = "II", white.adjust = "hc3")

# Adjusted means by year (controlling for caste)
emm_year <- emmeans(fit_fac, ~ Year)

# Pairwise year comparisons with multiplicity control
pairs(emm_year, adjust = "tukey")        # Tukey-HSD style

# A simple plot
plot(emm_year)       


##Calculating Bray-Curtis distance to compare the taxonomic composition of every year##

library(vegan)

#Create a frequency table: rows = Year, columns = Bombus species
bee_matrix <- with(bee_filtered_all, table(Year, Bombus_Species))

#Convert to matrix 
bee_matrix <- as.matrix(bee_matrix)

#Calculate Bray–Curtis dissimilarity
bray_curtis_dist <- vegdist(bee_matrix, method = "bray")

#Calculating significance of distances

# Use PERMANOVA to test for significant compositional differences by year
adonis_result <- adonis2(bee_matrix ~ rownames(bee_matrix), method = "bray")

# View result
print(adonis_result)

#Putting distances into a table

library(reshape2)

# Convert to full matrix
bray_matrix <- as.matrix(bray_curtis_dist)

# Turn it into a long-format table
bray_df <- melt(bray_matrix, varnames = c("Sample1", "Sample2"), value.name = "BrayCurtis")

#Calculating the Bray-Curtis distances for the functional compositions 

# Create a frequency table: rows = Year, columns = Bombus species
bee_matrix_pca <- with(bee_pca_all, table(Year, PC1_cluster))

# Convert to matrix (in case it comes out as a table)
bee_matrix_pca <- as.matrix(bee_matrix_pca)

# Calculate Bray–Curtis dissimilarity
bray_curtis_dist_pca <- vegdist(bee_matrix_pca, method = "bray")

#Calculating significance of distances

#Use PERMANOVA to test for significant compositional differences by year
adonis_result_funct <- adonis2(bee_matrix_pca ~ rownames(bee_matrix_pca), method = "bray")

# View result
print(adonis_result_funct)

#Putting distances into a table

# Convert to full matrix
bray_matrix_pca <- as.matrix(bray_curtis_dist_pca)

# Turn it into a long-format table
bray_df_pca <- melt(bray_matrix_pca, varnames = c("Sample1", "Sample2"), value.name = "BrayCurtis")


##Redoing heat maps to avoid repetition###
plot_bray_heatmap <- function(df, title, colours = c("grey90","lightblue","blue")) {
  years <- sort(unique(c(df$Sample1, df$Sample2)))
  full_grid <- expand.grid(Sample1 = years, Sample2 = years, 
                           stringsAsFactors = FALSE)
  full_df <- full_grid %>%
    left_join(df, by = c("Sample1","Sample2")) %>%
    mutate(
      BrayCurtis = case_when(
        Sample1 == Sample2            ~ 0,
        !is.na(BrayCurtis)            ~ BrayCurtis,
        TRUE                          ~ NA_real_
      ),
      Sample1 = factor(Sample1, levels = years),
      Sample2 = factor(Sample2, levels = years)
    ) %>%
    filter(as.numeric(Sample1) <= as.numeric(Sample2))
  
  ggplot(full_df, aes(x = Sample1, y = Sample2, fill = BrayCurtis)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colours = colours,
      values  = scales::rescale(c(0, 0.01, 0.5, 1)),
      limits  = c(0, max(full_df$BrayCurtis, na.rm = TRUE)),
      name    = "Bray–Curtis"
    ) +
    labs(title = title, x = "Year", y = "") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid  = element_blank()
    ) +
    coord_fixed()
}

# taxonomic map (blue scale)
plot_bray_heatmap(
  bray_df,
  title   = "Taxonomic Distances",
  colours = c("grey90","lightblue","blue")
)

# functional map (red → yellow)
plot_bray_heatmap(
  bray_df_pca,
  title   = "Functional Diversity",
  colours = c("grey90","lightpink","red")
)

##Plotting the differentials: 

# assume bray_df   has columns Sample1, Sample2, BrayCurtis (taxonomic)
#        bray_df_pca has columns Sample1, Sample2, BrayCurtis (functional)

# 1) First rename the two distance columns so they don't collide on join
tax_df <- bray_df   %>% rename(Taxonomic   = BrayCurtis)
fun_df <- bray_df_pca %>% rename(Functional = BrayCurtis)

# 2) build a full year×year grid
years <- sort(unique(c(tax_df$Sample1, tax_df$Sample2)))
full_grid <- expand.grid(
  Sample1 = years,
  Sample2 = years,
  stringsAsFactors = FALSE
)

# 3) join in both sets of distances and compute the difference
diff_df <- full_grid %>%
  left_join(fun_df, by = c("Sample1","Sample2")) %>%     # add Functional
  left_join(tax_df, by = c("Sample1","Sample2")) %>%     # add Taxonomic
  mutate(
    diff = Functional - Taxonomic,
    # force zero on the diagonal
    diff = ifelse(Sample1 == Sample2, 0, diff),
    # turn into factors so ggplot respects the ordering
    Sample1 = factor(Sample1, levels = years),
    Sample2 = factor(Sample2, levels = years)
  ) %>%
  # keep only the upper triangle (incl. diagonal)
  filter(as.integer(Sample1) <= as.integer(Sample2))

# 4) Plot
ggplot(diff_df, aes(x = Sample1, y = Sample2, fill = diff)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    name     = "Func − Taxon",
    low      = "blue",    # negative diffs → blue
    mid      = "grey90",   # zero → white
    high     = "red",     # positive diffs → red
    midpoint = 0
  ) +
  labs(
    title = "Diversity Differentials",
    x     = "Year",
    y     = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  ) +
  coord_fixed()

