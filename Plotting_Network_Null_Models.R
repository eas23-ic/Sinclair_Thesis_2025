##Calculating and plotting biparite networks##

#Run Data filtering script first#

#package
library(bipartite)

#Split the dataset by Year
bee_pca_2018 <- subset(bee_pca_all, Year == 2018)
bee_pca_2019 <- subset(bee_pca_all, Year == 2019)
bee_pca_2021 <- subset(bee_pca_all, Year == 2021)
bee_pca_2022 <- subset(bee_pca_all, Year == 2022)
bee_pca_2023 <- subset(bee_pca_all, Year == 2023)
bee_pca_2024 <- subset(bee_pca_all, Year == 2024)

#Creating null models to draw comparisons on whether my networks have more/less 
#connectedness or modularity than due to chance

#2018 species-based network 

#Ensuring the network is read as a matrix 
interaction_df_2018 <- bee_filtered_2018 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_18_sp <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2018)
network_matrix_18_sp <- as.matrix(network_matrix_18_sp)

#Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_18_sp, N = 100, method = "vaznull")

#Calculate observed metrics
obs_conn <- networklevel(network_matrix_18_sp, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_18_sp, index = "modularity")

#Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

#Z-scores
z_conn_18_sp <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_18_sp <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_18_sp <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_18_sp <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

#Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_18_sp, "\n")
cat("p-value:", p_conn_18_sp, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_18_sp, "\n")
cat("p-value:", p_mod_18_sp, "\n")

#2019 species-based data

interaction_df_2019 <- bee_2019 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_19_sp <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2019)
network_matrix_19_sp <- as.matrix(network_matrix_19_sp)

class(network_matrix_19_sp)  # should say "matrix"

#Example matrix (replace with your matrix)
network_matrix_19_sp <- mat_2019  

#Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_19_sp, N = 100, method = "vaznull")

#Calculate observed metrics
obs_conn <- networklevel(network_matrix_19_sp, index = "weighted connectance")
# however you built your 2019 table, e.g.
network_matrix_19_sp <- xtabs(~ Bombus_Species + Flower_Species,
                              data = interaction_df_2019)

#Make it an actual matrix
network_matrix_19_sp <- as.matrix(network_matrix_19_sp)

#Modularity
obs_mod <- networklevel(network_matrix_19_sp,
                        index = "modularity")
obs_mod <- networklevel(network_matrix_19_sp, index = "modularity")

#Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

#Z-scores
z_conn_19_sp <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_19_sp <- (obs_mod - mean(null_mod)) / sd(null_mod)

#Calculate two-tailed p-values
p_conn_19_sp <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_19_sp <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

#Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_19_sp, "\n")
cat("p-value:", p_conn_19_sp, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_19_sp, "\n")
cat("p-value:", p_mod_19_sp, "\n")

#2021 species-based network 

#Ensuring the network is read as a matrix 
interaction_df_2021 <- bee_2021 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_21_sp <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2021)
network_matrix_21_sp <- as.matrix(network_matrix_21_sp)

#Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_21_sp, N = 100, method = "vaznull")

#Calculate observed metrics
obs_conn <- networklevel(network_matrix_21_sp, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_21_sp, index = "modularity")

#Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

#Z-scores
z_conn_21_sp <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_21_sp <- (obs_mod - mean(null_mod)) / sd(null_mod)

#Calculate two-tailed p-values
p_conn_21_sp <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_21_sp <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

#Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_21_sp, "\n")
cat("p-value:", p_conn_21_sp, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_21_sp, "\n")
cat("p-value:", p_mod_21_sp, "\n")

#2022 species-based network 

#Ensuring the network is read as a matrix 
interaction_df_2022 <- bee_2022 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_22_sp <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2022)
network_matrix_22_sp <- as.matrix(network_matrix_22_sp)

#Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_22_sp, N = 100, method = "vaznull")

#Calculate observed metrics
obs_conn <- networklevel(network_matrix_22_sp, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_22_sp, index = "modularity")

#Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_22_sp <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_22_sp <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_22_sp <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_22_sp <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_22_sp, "\n")
cat("p-value:", p_conn_22_sp, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_22_sp, "\n")
cat("p-value:", p_mod_22_sp, "\n")

#2023 species-based network 

#Ensuring the network is read as a matrix 
interaction_df_2023 <- bee_2023 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_23_sp <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2023)
network_matrix_23_sp <- as.matrix(network_matrix_23_sp)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_23_sp, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_23_sp, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_23_sp, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_23_sp <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_23_sp <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_23_sp <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_23_sp <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_23_sp, "\n")
cat("p-value:", p_conn_23_sp, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_23_sp, "\n")
cat("p-value:", p_mod_23_sp, "\n")

##2024 species-based network 

#Ensuring the network is read as a matrix 
interaction_df_2024 <- bee_2024 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_24_sp <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2024)
network_matrix_24_sp <- as.matrix(network_matrix_24_sp)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_24_sp, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_24_sp, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_24_sp, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_24_sp <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_24_sp <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_24_sp <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_24_sp <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_24_sp, "\n")
cat("p-value:", p_conn_24_sp, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_24_sp, "\n")
cat("p-value:", p_mod_24_sp, "\n")

#2018 trait-based network 

#Ensuring the network is read as a matrix 
interaction_df_2018_tr <- bee_pca_2018 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_18_tr <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2018_tr)
network_matrix_18_tr <- as.matrix(network_matrix_18_tr)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_18_tr, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_18_tr, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_18_tr, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_18_tr <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_18_tr <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_18_tr <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_18_tr <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_18_tr, "\n")
cat("p-value:", p_conn_18_tr, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_18_tr, "\n")
cat("p-value:", p_mod_18_tr, "\n")

#2019 trait-based network 

#Ensuring the network is read as a matrix 
interaction_df_2019_tr <- bee_pca_2019 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_19_tr <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2019_tr)
network_matrix_19_tr <- as.matrix(network_matrix_19_tr)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_19_tr, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_19_tr, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_19_tr, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_19_tr <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_19_tr <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_19_tr <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_19_tr <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_19_tr, "\n")
cat("p-value:", p_conn_19_tr, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_19_tr, "\n")
cat("p-value:", p_mod_19_tr, "\n")
#2021 trait-based network 

#Ensuring the network is read as a matrix 
interaction_df_2021_tr <- bee_pca_2021 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_21_tr <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2021_tr)
network_matrix_21_tr <- as.matrix(network_matrix_21_tr)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_21_tr, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_21_tr, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_21_tr, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_21_tr <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_21_tr <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_21_tr <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_21_tr <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_21_tr, "\n")
cat("p-value:", p_conn_21_tr, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_21_tr, "\n")
cat("p-value:", p_mod_21_tr, "\n")

#2022 trait-based network 

#Ensuring the network is read as a matrix 
interaction_df_2022_tr <- bee_pca_2022 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_22_tr <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2022_tr)
network_matrix_22_tr <- as.matrix(network_matrix_22_tr)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_22_tr, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_22_tr, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_22_tr, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_22_tr <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_22_tr <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_22_tr <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_22_tr <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_22_tr, "\n")
cat("p-value:", p_conn_22_tr, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_22_tr, "\n")
cat("p-value:", p_mod_22_tr, "\n")

#2023 trait-based network 

#Ensuring the network is read as a matrix 
interaction_df_2023_tr <- bee_pca_2023 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_23_tr <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2023_tr)
network_matrix_23_tr <- as.matrix(network_matrix_23_tr)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_23_tr, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_23_tr, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_23_tr, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_23_tr <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_23_tr <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_23_tr <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_23_tr <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_23_tr, "\n")
cat("p-value:", p_conn_23_tr, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_23_tr, "\n")
cat("p-value:", p_mod_23_tr, "\n")

#2024 trait-based network 

#Ensuring the network is read as a matrix 
interaction_df_2024_tr <- bee_pca_2024 %>%
  select(Bombus_Species, Flower_Species)

network_matrix_24_tr <- xtabs(~ Bombus_Species + Flower_Species, data = interaction_df_2024_tr)
network_matrix_24_tr <- as.matrix(network_matrix_24_tr)

# Generate 100 null networks using vaznull
set.seed(123)
null_models <- nullmodel(network_matrix_24_tr, N = 100, method = "vaznull")

# Calculate observed metrics
obs_conn <- networklevel(network_matrix_24_tr, index = "weighted connectance")
obs_mod <- networklevel(network_matrix_24_tr, index = "modularity")

# Extract null metric values
null_conn <- sapply(null_models, function(mat) {
  networklevel(mat, index = "weighted connectance")
})

null_mod <- sapply(null_models, function(mat) {
  networklevel(mat, index = "modularity")
})

# Calculate Z-scores
z_conn_24_tr <- (obs_conn - mean(null_conn)) / sd(null_conn)
z_mod_24_tr <- (obs_mod - mean(null_mod)) / sd(null_mod)

# Calculate two-tailed p-values
p_conn_24_tr <- mean(abs(null_conn - mean(null_conn)) >= abs(obs_conn - mean(null_conn)))
p_mod_24_tr <- mean(abs(null_mod - mean(null_mod)) >= abs(obs_mod - mean(null_mod)))

# Output results
cat("=== Weighted Connectedness ===\n")
cat("Observed:", obs_conn, "\n")
cat("Z-score:", z_conn_24_tr, "\n")
cat("p-value:", p_conn_24_tr, "\n\n")

cat("=== Modularity ===\n")
cat("Observed:", obs_mod, "\n")
cat("Z-score:", z_mod_24_tr, "\n")
cat("p-value:", p_mod_24_tr, "\n")

#PLotting the biparitite networks 

#2018 species based 

bee_2018_long <- bee_filtered_2018 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

ggplot(bee_2018_long,
       aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Bumblebee Species", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "viridis") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

#2019 species based network

bee_2019_long <- bee_2019 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

ggplot(bee_2019_long,
       aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Bumblebee Species", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "viridis") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

#2021 species network

bee_2021_long <- bee_2021 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

ggplot(bee_2021_long,
       aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Bumblebee Species", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "viridis") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

#2022 species network

bee_2022_long <- bee_2022 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

ggplot(bee_2022_long,
       aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Bumblebee Species", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "viridis") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

#2023 species network 

bee_2023_long <- bee_2023 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

ggplot(bee_2023_long,
       aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Bumblebee Species", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "viridis") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

#2024 species network

bee_2024_long <- bee_2024 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

ggplot(bee_2024_long,
       aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Bumblebee Species", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "viridis") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

#2018 trait based network

install.packages("scico")  # run this once
library(scico)


bee_pca_2018_long <- bee_pca_2018 %>%
  count(PC1_cluster = as.factor(PC1_cluster), Flower_Species, name = "Interactions")

custom_red_pink <- c(
  "#800026",  # deep burgundy
  "#C51B8A",  # rich pink
  "#E31A1C",  # bright red
  "#FC4E2A",  # red-orange
  "#FD8D3C",  # strong orange
  "#FDBB84",  # soft peachy orange
  "#FED976",  # yellow-orange
  "#FEE8C8"   # warm cream
)

ggplot(bee_pca_2018_long,
       aes(axis1 = PC1_cluster, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.95) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Functional Cluster", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) 

#2019 trait network

bee_pca_2019_long <- bee_pca_2019 %>%
  count(PC1_cluster = as.factor(PC1_cluster), Flower_Species, name = "Interactions")

ggplot(bee_pca_2019_long,
       aes(axis1 = PC1_cluster, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.95) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Functional Cluster", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) 

#2021 trait network

bee_pca_2021_long <- bee_pca_2021 %>%
  count(PC1_cluster = as.factor(PC1_cluster), Flower_Species, name = "Interactions")

ggplot(bee_pca_2021_long,
       aes(axis1 = PC1_cluster, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.95) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Functional Cluster", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) 

#2022 trait network

bee_pca_2022_long <- bee_pca_2022 %>%
  count(PC1_cluster = as.factor(PC1_cluster), Flower_Species, name = "Interactions")

ggplot(bee_pca_2022_long,
       aes(axis1 = PC1_cluster, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.95) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Functional Cluster", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) 

#2023 trait network

bee_pca_2023_long <- bee_pca_2023 %>%
  count(PC1_cluster = as.factor(PC1_cluster), Flower_Species, name = "Interactions")

ggplot(bee_pca_2023_long,
       aes(axis1 = PC1_cluster, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.95) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Functional Cluster", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) 

#2024 trait network

bee_pca_2024_long <- bee_pca_2024 %>%
  count(PC1_cluster = as.factor(PC1_cluster), Flower_Species, name = "Interactions")

ggplot(bee_pca_2024_long,
       aes(axis1 = PC1_cluster, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.95) +
  geom_stratum(width = 0.35, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, color = "black", hjust = 0.5) +
  scale_x_discrete(limits = c("Functional Cluster", "Flower Species"),
                   expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) 

#Plotting them as Tripartite for better visualisation

library(cowplot)

#2024 tripartite 

# Prep data
bee_species_flower <- bee_pca_2024 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

bee_flower_traits <- bee_pca_2024 %>%
  count(Flower_Species, PC1_cluster, name = "Interactions")

# Define color palettes
species_colors <- viridis::viridis(length(unique(bee_species_flower$Bombus_Species)), option = "D")

custom_red_pink <- c(
  "#800026",  # deep burgundy
  "#C51B8A",  # rich pink
  "#E31A1C",  # bright red
  "#FC4E2A",  # red-orange
  "#FD8D3C",  # strong orange
  "#FDBB84",  # soft peachy orange
  "#FED976",  # yellow-orange
  "#FEE8C8"   # warm cream
)

# Left plot: Species → Flower
p1 <- ggplot(bee_species_flower,
             aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.4, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  scale_x_discrete(limits = c("Species", "Flower"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = species_colors) +
  theme_void() +
  theme(legend.position = "none")

# Right plot: Flower → Trait Cluster
p2 <- ggplot(bee_flower_traits,
             aes(axis1 = Flower_Species, axis2 = as.factor(PC1_cluster), y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.5, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Flower", "Trait Cluster"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(legend.position = "none")

# Combine plots
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 1))

#2019 tripartite

# Prep data
bee_species_flower <- bee_pca_2019 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

bee_flower_traits <- bee_pca_2019 %>%
  count(Flower_Species, PC1_cluster, name = "Interactions")

# Define color palettes
species_colors <- viridis::viridis(length(unique(bee_species_flower$Bombus_Species)), option = "D")

custom_red_pink <- c(
  "#800026",  # deep burgundy
  "#C51B8A",  # rich pink
  "#E31A1C",  # bright red
  "#FC4E2A",  # red-orange
  "#FD8D3C",  # strong orange
  "#FDBB84",  # soft peachy orange
  "#FED976",  # yellow-orange
  "#FEE8C8"   # warm cream
)

# Left plot: Species → Flower
p1 <- ggplot(bee_species_flower,
             aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.4, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  scale_x_discrete(limits = c("Species", "Flower"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = species_colors) +
  theme_void() +
  theme(legend.position = "none")

# Right plot: Flower → Trait Cluster
p2 <- ggplot(bee_flower_traits,
             aes(axis1 = Flower_Species, axis2 = as.factor(PC1_cluster), y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.5, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Flower", "Trait Cluster"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(legend.position = "none")

# Combine plots
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 1))

#2018 Tripartite

# Prep data
bee_species_flower <- bee_pca_2018 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

bee_flower_traits <- bee_pca_2018 %>%
  count(Flower_Species, PC1_cluster, name = "Interactions")

# Define color palettes
species_colors <- viridis::viridis(length(unique(bee_species_flower$Bombus_Species)), option = "D")

custom_red_pink <- c(
  "#800026",  # deep burgundy
  "#C51B8A",  # rich pink
  "#E31A1C",  # bright red
  "#FC4E2A",  # red-orange
  "#FD8D3C",  # strong orange
  "#FDBB84",  # soft peachy orange
  "#FED976",  # yellow-orange
  "#FEE8C8"   # warm cream
)

# Left plot: Species → Flower
p1 <- ggplot(bee_species_flower,
             aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.4, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  scale_x_discrete(limits = c("Species", "Flower"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = species_colors) +
  theme_void() +
  theme(legend.position = "none")

# Right plot: Flower → Trait Cluster
p2 <- ggplot(bee_flower_traits,
             aes(axis1 = Flower_Species, axis2 = as.factor(PC1_cluster), y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.5, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Flower", "Trait Cluster"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(legend.position = "none")

# Combine plots
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 1))

#2021 Tripartite

# Prep data
bee_species_flower <- bee_pca_2021 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

bee_flower_traits <- bee_pca_2021 %>%
  count(Flower_Species, PC1_cluster, name = "Interactions")

# Define color palettes
species_colors <- viridis::viridis(length(unique(bee_species_flower$Bombus_Species)), option = "D")

custom_red_pink <- c(
  "#800026",  # deep burgundy
  "#C51B8A",  # rich pink
  "#E31A1C",  # bright red
  "#FC4E2A",  # red-orange
  "#FD8D3C",  # strong orange
  "#FDBB84",  # soft peachy orange
  "#FED976",  # yellow-orange
  "#FEE8C8"   # warm cream
)

# Left plot: Species → Flower
p1 <- ggplot(bee_species_flower,
             aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.4, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  scale_x_discrete(limits = c("Species", "Flower"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = species_colors) +
  theme_void() +
  theme(legend.position = "none")

# Right plot: Flower → Trait Cluster
p2 <- ggplot(bee_flower_traits,
             aes(axis1 = Flower_Species, axis2 = as.factor(PC1_cluster), y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.5, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Flower", "Trait Cluster"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(legend.position = "none")

# Combine plots
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 1))

#2022 Tripartite

# Prep data
bee_species_flower <- bee_pca_2022 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

bee_flower_traits <- bee_pca_2022 %>%
  count(Flower_Species, PC1_cluster, name = "Interactions")

# Define color palettes
species_colors <- viridis::viridis(length(unique(bee_species_flower$Bombus_Species)), option = "D")

custom_red_pink <- c(
  "#800026",  # deep burgundy
  "#C51B8A",  # rich pink
  "#E31A1C",  # bright red
  "#FC4E2A",  # red-orange
  "#FD8D3C",  # strong orange
  "#FDBB84",  # soft peachy orange
  "#FED976",  # yellow-orange
  "#FEE8C8"   # warm cream
)

# Left plot: Species → Flower
p1 <- ggplot(bee_species_flower,
             aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.4, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  scale_x_discrete(limits = c("Species", "Flower"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = species_colors) +
  theme_void() +
  theme(legend.position = "none")

# Right plot: Flower → Trait Cluster
p2 <- ggplot(bee_flower_traits,
             aes(axis1 = Flower_Species, axis2 = as.factor(PC1_cluster), y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.5, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Flower", "Trait Cluster"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(legend.position = "none")

# Combine plots
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 1))

#2023 Tripartite

# Prep data
bee_species_flower <- bee_pca_2023 %>%
  count(Bombus_Species, Flower_Species, name = "Interactions")

bee_flower_traits <- bee_pca_2023 %>%
  count(Flower_Species, PC1_cluster, name = "Interactions")

# Define color palettes
species_colors <- viridis::viridis(length(unique(bee_species_flower$Bombus_Species)), option = "D")

custom_red_pink <- c(
  "#800026",  # deep burgundy
  "#C51B8A",  # rich pink
  "#E31A1C",  # bright red
  "#FC4E2A",  # red-orange
  "#FD8D3C",  # strong orange
  "#FDBB84",  # soft peachy orange
  "#FED976",  # yellow-orange
  "#FEE8C8"   # warm cream
)

# Left plot: Species → Flower
p1 <- ggplot(bee_species_flower,
             aes(axis1 = Bombus_Species, axis2 = Flower_Species, y = Interactions)) +
  geom_alluvium(aes(fill = Bombus_Species), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.4, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
  scale_x_discrete(limits = c("Species", "Flower"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = species_colors) +
  theme_void() +
  theme(legend.position = "none")

# Right plot: Flower → Trait Cluster
p2 <- ggplot(bee_flower_traits,
             aes(axis1 = Flower_Species, axis2 = as.factor(PC1_cluster), y = Interactions)) +
  geom_alluvium(aes(fill = as.factor(PC1_cluster)), width = 0.35, alpha = 0.9) +
  geom_stratum(width = 0.5, fill = "grey95", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Flower", "Trait Cluster"), expand = c(0.1, 0.1)) +
  scale_fill_manual(values = custom_red_pink) +
  theme_void() +
  theme(legend.position = "none")

# Combine plots
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 1))


##Plotting the null model visually##

years <- c(2018, 2019, 2021, 2022, 2023, 2024)
types <- c("sp","tr")

null_list <- list()
obs_list  <- list()

for(year in years) {
  # grab the last two characters of the year
  yy <- substr(as.character(year), 3, 4)
  for(tp in types) {
    removal_label <- if (tp=="sp") "FG_removal" else "Species_removal"
    # build exactly your object names:
    null_conn_name <- paste0("null_conn_", yy, "_", tp)
    obs_conn_name  <- paste0("obs_conn_",  yy, "_", tp)
    null_mod_name  <- paste0("null_mod_",  yy, "_", tp)
    obs_mod_name   <- paste0("obs_mod_",   yy, "_", tp)
    
    # pull the vectors from your environment
    null_conn <- get(null_conn_name)
    obs_conn  <- get(obs_conn_name)
    null_mod  <- get(null_mod_name)
    obs_mod   <- get(obs_mod_name)
    
    # build little data-frames
    null_list[[paste(yy,tp,"conn")]] <- data.frame(
      Year         = year,
      Removal_Type = removal_label,
      Metric       = "Connectance",
      Value        = null_conn
    )
    null_list[[paste(yy,tp,"mod")]]  <- data.frame(
      Year         = year,
      Removal_Type = removal_label,
      Metric       = "Modularity",
      Value        = null_mod
    )
    
    obs_list[[paste(yy,tp,"conn")]]  <- data.frame(
      Year         = year,
      Removal_Type = removal_label,
      Metric       = "Connectance",
      Value        = obs_conn
    )
    obs_list[[paste(yy,tp,"mod")]]   <- data.frame(
      Year         = year,
      Removal_Type = removal_label,
      Metric       = "Modularity",
      Value        = obs_mod
    )
  }
}

null_df <- bind_rows(null_list)
obs_df  <- bind_rows(obs_list)

# recode to “Species Network” / “Functional Network”
plot_df <- null_df %>% 
  mutate(Network_Type = case_when(
    Removal_Type == "FG_removal"      ~ "Species Network",
    Removal_Type == "Species_removal" ~ "Functional Network",
    TRUE                               ~ Removal_Type
  )) %>%
  mutate(Network_Type = factor(Network_Type, c("Species Network","Functional Network")))

obs_plot_df <- obs_df %>% 
  mutate(Network_Type = case_when(
    Removal_Type == "FG_removal"      ~ "Species Network",
    Removal_Type == "Species_removal" ~ "Functional Network",
    TRUE                               ~ Removal_Type
  )) %>%
  mutate(Network_Type = factor(Network_Type, c("Species Network","Functional Network")))

# colour mapping
my_cols <- c(
  "Species Network"    = "#E67E22",
  "Functional Network" = "#1ABC9C"
)

# Connectance panel
p_conn <- plot_df %>%
  filter(Metric=="Connectance") %>%
  ggplot(aes(Network_Type, Value, fill=Network_Type))+
  geom_boxplot(alpha=0.6, outlier.shape=NA)+
  geom_point(data=obs_plot_df%>%filter(Metric=="Connectance"),
             aes(x=Network_Type,y=Value), color="red", size=3)+
  facet_wrap(~Year, ncol=3)+
  scale_fill_manual(values=my_cols)+
  labs(y="Weighted Connectance")+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=45,hjust=1),
        strip.background=element_rect(fill="grey90",color=NA))

# Modularity panel
p_mod <- plot_df %>%
  filter(Metric=="Modularity") %>%
  ggplot(aes(Network_Type, Value, fill=Network_Type))+
  geom_boxplot(alpha=0.6, outlier.shape=NA)+
  geom_point(data=obs_plot_df%>%filter(Metric=="Modularity"),
             aes(x=Network_Type,y=Value), color="red", size=3)+
  facet_wrap(~Year, ncol=3)+
  scale_fill_manual(values=my_cols)+
  labs(y="Modularity")+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=45,hjust=1),
        strip.background=element_rect(fill="grey90",color=NA))

# display
print(p_conn)
print(p_mod)

##Calculating averages##

# vector of years (just to keep track)
years <- c(2018, 2019, 2021, 2022, 2023, 2024)

# collect the six species‐network modularities
mod_sp <- c(
  obs_mod_18_sp,
  obs_mod_19_sp,
  obs_mod_21_sp,
  obs_mod_22_sp,
  obs_mod_23_sp,
  obs_mod_24_sp
)

# collect the six functional‐network modularities
mod_tr <- c(
  obs_mod_18_tr,
  obs_mod_19_tr,
  obs_mod_21_tr,
  obs_mod_22_tr,
  obs_mod_23_tr,
  obs_mod_24_tr
)

# simple summary
mean_sp <- mean(mod_sp)
mean_tr <- mean(mod_tr)

sd_sp   <- sd(mod_sp)
sd_tr   <- sd(mod_tr)

# print out
cat("Species‐based networks modularity:\n",
    "  Years:", paste(years, collapse=", "), "\n",
    "  Mean modularity =", round(mean_sp,3),
    " (SD =", round(sd_sp,3), ")\n\n")

cat("Functional‐based networks modularity:\n",
    "  Years:", paste(years, collapse=", "), "\n",
    "  Mean modularity =", round(mean_tr,3),
    " (SD =", round(sd_tr,3), ")\n")

#calculating averaged connectedness:

# vector of years
years <- c(2018, 2019, 2021, 2022, 2023, 2024)

# collect the six species‐network connectance values
conn_sp <- c(
  obs_conn_18_sp,
  obs_conn_19_sp,
  obs_conn_21_sp,
  obs_conn_22_sp,
  obs_conn_23_sp,
  obs_conn_24_sp
)

# collect the six functional‐network connectance values
conn_tr <- c(
  obs_conn_18_tr,
  obs_conn_19_tr,
  obs_conn_21_tr,
  obs_conn_22_tr,
  obs_conn_23_tr,
  obs_conn_24_tr
)

# compute means and standard deviations
mean_conn_sp <- mean(conn_sp)
sd_conn_sp   <- sd(conn_sp)

mean_conn_tr <- mean(conn_tr)
sd_conn_tr   <- sd(conn_tr)

# print to console
cat("Species‐based networks weighted connectance:\n",
    "  Years:", paste(years, collapse = ", "), "\n",
    "  Mean =", round(mean_conn_sp, 3),
    " (SD =", round(sd_conn_sp, 3), ")\n\n")

cat("Functional‐based networks weighted connectance:\n",
    "  Years:", paste(years, collapse = ", "), "\n",
    "  Mean =", round(mean_conn_tr, 3),
    " (SD =", round(sd_conn_tr, 3), ")\n")
