##Stepwise Extinction model##

#Run Data filtering script first#

###Building the model###

#packages 
library(dplyr)
library(nlme)
library(ggplot2)
library(pracma)
library(bipartite)

#bind datasets
bee_datasets <- list(
  `2018` = bee_pca_2018,
  `2019` = bee_pca_2019,
  `2021` = bee_pca_2021,
  `2022` = bee_pca_2022,
  `2023` = bee_pca_2023,
  `2024` = bee_pca_2024
)

network_types <- c("species","functional")
Times_run     <- 100
OutPath       <- "C:/Users/User/Downloads/Revised Extinction Analysis 2"
if (!dir.exists(OutPath)) dir.create(OutPath, recursive = TRUE)

# the two metrics 
metrics_to_plot <- c("Bombus_Species_remaining","Flower_Species_remaining")

for(net_type in network_types){
  for(year in names(bee_datasets)){
    Int_net <- bee_datasets[[year]]
    
    
    if(net_type=="species"){
      Base <- "FG_removal"
      Tm <- data.frame(
        Sample_No      = Int_net$Sample_No,
        Flower_Species = Int_net$Flower_Species,
        Bombus_Species = Int_net$Bombus_Species,
        FG             = Int_net$PC1_cluster,
        ID             = Int_net$Sample_No
      )
    } else {
      Base <- "Species_removal"
      Tm <- data.frame(
        Sample_No      = Int_net$Sample_No,
        Flower_Species = Int_net$Flower_Species,
        Bombus_Species = Int_net$PC1_cluster,
        FG             = Int_net$Bombus_Species,
        ID             = Int_net$Sample_No
      )
    }
    
   
    mat0 <- as.matrix(table(Tm$Bombus_Species, Tm$Flower_Species))
    intact <- networklevel(mat0,
                           index = c("connectance","weighted connectance")) %>%
      t() %>% data.frame(stringsAsFactors=FALSE)
    
    intact$Bombus_Species_affected  <- NA
    intact$FG_removed               <- NA
    intact$Position_removed         <- 0
    intact$Times_run                <- 0
    intact$Flower_Species_remaining <- length(unique(Tm$Flower_Species))
    intact$Bombus_Species_remaining <- length(unique(Tm$Bombus_Species))
    intact$FG_remaining             <- length(unique(Tm$FG))
    intact$Interactions_remaining   <- sum(mat0)
    
    network_info <- intact
    
    #randomized removals
    for(j in seq_len(Times_run)){
      Tm2 <- Tm
      Runs <- Tm %>%
        dplyr::group_by(Bombus_Species, FG) %>%
        dplyr::summarise(n(), .groups="drop") %>%
        nrow()
      
      for(i in seq_len(Runs)){
        # pick one “Bombus_Species” and remove an FG
        spat    <- sample(unique(Tm2$Bombus_Species),1)
        sub     <- dplyr::filter(Tm2, Bombus_Species==spat)
        to_drop <- sample(unique(sub$FG),1)
        Tm2     <- dplyr::filter(Tm2, !(ID %in% subset(sub,FG==to_drop)$ID))
        
       
        if(length(unique(Tm2$Bombus_Species))==1 ||
           length(unique(Tm2$Flower_Species)) ==1) break
        
        
        mat_tmp <- as.matrix(table(Tm2$Bombus_Species, Tm2$Flower_Species))
        ni      <- networklevel(mat_tmp,
                                index = c("connectance","weighted connectance")) %>%
          t() %>% data.frame(stringsAsFactors=FALSE)
        
       
        ni$Bombus_Species_affected  <- spat
        ni$FG_removed               <- to_drop
        ni$Position_removed         <- i
        ni$Times_run                <- j
        ni$Flower_Species_remaining <- length(unique(Tm2$Flower_Species))
        ni$Bombus_Species_remaining <- length(unique(Tm2$Bombus_Species))
        ni$FG_remaining             <- length(unique(Tm2$FG))
        ni$Interactions_remaining   <- sum(mat_tmp)
        
        network_info <- dplyr::bind_rows(network_info, ni)
      }
    }
    
    #a copy for export
    network_info2 <- network_info
    
    # model + AUC + plot
    AUC_info <- data.frame(
      Variable=character(), AUC=double(), AUC_stan=double(), Model_form=character(),
      Intercept=double(), Intercept_se=double(), Intercept_t=double(), Intercept_p=double(),
      Beta=double(), Beta_se=double(), Beta_t=double(), Beta_p=double(),
      Beta2=double(), Beta2_se=double(), Beta2_t=double(), Beta2_p=double(),
      Variance_structure=character(),
      stringsAsFactors = FALSE
    )
    
    for(metric in metrics_to_plot){
      #quadratic term convenience column
      network_info$Position_removed2 <- network_info$Position_removed^2
      
      fit_lin  <- lm(network_info[[metric]] ~ Position_removed,                         data=network_info)
      fit_quad <- lm(network_info[[metric]] ~ Position_removed + Position_removed2,     data=network_info)
      fit_exp  <- lm(network_info[[metric]] ~ exp(Position_removed),                    data=network_info)
      
      R2s  <- c(L=summary(fit_lin)$adj.r.squared,
                Q=summary(fit_quad)$adj.r.squared,
                E=summary(fit_exp)$adj.r.squared)
      best <- names(which.max(R2s))  # "L","Q","E"
      
      if(best=="L"){
        form     <- as.formula(paste(metric,"~Position_removed"))
        base_mod <- gls(form, data=network_info)
      } else if(best=="Q"){
        form     <- as.formula(paste(metric,"~Position_removed+Position_removed2"))
        base_mod <- gls(form, data=network_info)
      } else {
        form     <- as.formula(paste(metric,"~exp(Position_removed)"))
        base_mod <- gls(form, data=network_info)
      }
      var_mod <- try(gls(form,
                         weights=varExp(~Position_removed),
                         data=network_info),
                     silent=TRUE)
      if(inherits(var_mod,"try-error")){
        mod <- base_mod; vs <- "none"
      } else {
        if(AIC(var_mod) < AIC(base_mod)) { mod <- var_mod; vs <- "varExp" } else { mod <- base_mod; vs <- "none" }
      }
      
      sm <- summary(mod)$tTable
      
      
      x_max  <- max(network_info$Position_removed, na.rm = TRUE)
      x_grid <- seq(0, x_max, by = 1)                           
      
      newdata <- data.frame(Position_removed = x_grid)
      if (best %in% c("Q","Quadratic")) newdata$Position_removed2 <- x_grid^2
      
      preds_grid <- as.numeric(predict(mod, newdata = newdata))
      preds_grid <- pmax(preds_grid, 0)                         
      
      # raw AUC in original units 
      AUC_raw <- pracma::trapz(x_grid, preds_grid)
      
      #standardised AUC on [0,1]: x as fraction removed, y as fraction of initial richness
      y0     <- preds_grid[1]
      if (!is.finite(y0) || y0 <= 0) y0 <- max(preds_grid, na.rm = TRUE)
      x_prop <- if (x_max > 0) x_grid / x_max else x_grid
      y_prop <- pmin(pmax(preds_grid / y0, 0), 1)               # bound 0..1
      
      AUC_std <- pracma::trapz(x_prop, y_prop)
      # -----------------------------------------------------------------
      
      model_label <- switch(best, L="Linear", Q="Quadratic", E="Exponential")
      
      #extract coefficients 
      Intercept    <- sm[1,"Value"];     Intercept_se <- sm[1,"Std.Error"]
      Intercept_t  <- sm[1,"t-value"];   Intercept_p  <- sm[1,"p-value"]
      
      slope_name <- switch(best,
                           L = "Position_removed",
                           Q = "Position_removed",
                           E = "exp(Position_removed)")
      if (!slope_name %in% rownames(sm)) slope_name <- rownames(sm)[2]
      Slope    <- sm[slope_name,"Value"]
      Slope_se <- sm[slope_name,"Std.Error"]
      Slope_t  <- sm[slope_name,"t-value"]
      Slope_p  <- sm[slope_name,"p-value"]
      
      Beta2 <- Beta2_se <- Beta2_t <- Beta2_p <- NA_real_
      if(best=="Q"){
        if("Position_removed2" %in% rownames(sm)){
          Beta2    <- sm["Position_removed2","Value"]
          Beta2_se <- sm["Position_removed2","Std.Error"]
          Beta2_t  <- sm["Position_removed2","t-value"]
          Beta2_p  <- sm["Position_removed2","p-value"]
        } else if (nrow(sm) >= 3){
          Beta2    <- sm[3,"Value"];  Beta2_se <- sm[3,"Std.Error"]
          Beta2_t  <- sm[3,"t-value"];Beta2_p  <- sm[3,"p-value"]
        }
      }
      
      
      out <- data.frame(
        Variable           = metric,
        AUC                = AUC_raw,
        AUC_stan           = AUC_std,          
        Model_form         = model_label,
        Intercept          = Intercept,
        Intercept_se       = Intercept_se,
        Intercept_t        = Intercept_t,
        Intercept_p        = Intercept_p,
        Beta               = Slope,
        Beta_se            = Slope_se,
        Beta_t             = Slope_t,
        Beta_p             = Slope_p,
        Beta2              = Beta2,
        Beta2_se           = Beta2_se,
        Beta2_t            = Beta2_t,
        Beta2_p            = Beta2_p,
        Variance_structure = vs,
        stringsAsFactors   = FALSE
      )
      AUC_info <- dplyr::bind_rows(AUC_info, out)
      
      
      plot_df <- dplyr::filter(network_info, Position_removed > 0)
      p <- ggplot(plot_df,
                  aes(x = Position_removed,
                      y = .data[[metric]],
                      color = FG_removed)) +
        geom_jitter(width=0.3, height=0.2, size=1.5, alpha=0.7) +
        theme_classic() +
        labs(
          x = "Removal step",
          y = metric,
          subtitle = paste0("Raw AUC=", round(AUC_raw,2),
                            "; Std AUC=", round(AUC_std,3))
        ) +
        guides(color = guide_legend(title = "Groups removed")) +
        theme(legend.position = "right")
      
      fn <- paste0("ExtPlot_", Base, "_", year, "_", metric, ".png")
      png(file.path(OutPath, fn), width=1000, height=800)
      print(p)
      dev.off()
    } # end metric loop
    
    #write tables
    write.csv(network_info2,
              file=file.path(OutPath, paste0("ExtMetrics_",Base,"_",year,".csv")),
              row.names=FALSE)
    write.csv(AUC_info,
              file=file.path(OutPath, paste0("AUC_",Base,"_",year,".csv")),
              row.names=FALSE)
    
  } 
}   

###Visualising the model###

library(cowplot)

# where CSVs will live
OutPath <- "C:/Users/User/Downloads/Revised Extinction Analysis 2"
if(!dir.exists(OutPath)) dir.create(OutPath, recursive=TRUE)

years   <- c(2018, 2019, 2021, 2022, 2023, 2024)
bases   <- c("FG_removal", "Species_removal")
metrics <- c("Bombus_Species_remaining", "Flower_Species_remaining")

base_labels <- c(
  FG_removal      = "Species‐based network\n(remove FG)",
  Species_removal = "Trait‐based network\n(remove species)"
)

# -------------------------------------------------------------------
# A) Read ExtMetrics and stack
# -------------------------------------------------------------------
ext_list <- list()
for (base in bases) {
  for (yr in years) {
    fn <- file.path(OutPath, paste0("ExtMetrics_", base, "_", yr, ".csv"))
    df <- read.csv(fn, stringsAsFactors = FALSE)
    
    df$Bombus_Species_affected <- as.character(df$Bombus_Species_affected)
    df$FG_removed              <- as.character(df$FG_removed)
    
    df$Year         <- yr
    df$Removal_Type <- base_labels[base]
    ext_list[[paste(base, yr, sep = "_")]] <- df
  }
}
ext_df <- bind_rows(ext_list)

# -------------------------------------------------------------------
# B) Read AUC tables (for facet annotations)
# -------------------------------------------------------------------
auc_list <- list()
for (base in bases) {
  for (yr in years) {
    fn  <- file.path(OutPath, paste0("AUC_", base, "_", yr, ".csv"))
    aua <- read.csv(fn, stringsAsFactors = FALSE)
    aua$Year         <- yr
    aua$Removal_Type <- base_labels[base]
    auc_list[[paste(base, yr, sep = "_")]] <- aua
  }
}
auc_df <- bind_rows(auc_list)

lvl_order <- c(base_labels["FG_removal"], base_labels["Species_removal"])
ext_df <- ext_df %>%
  mutate(Removal_Type = factor(Removal_Type, levels = lvl_order))
auc_df <- auc_df %>%
  mutate(Removal_Type = factor(Removal_Type, levels = lvl_order))

# Build small annotation tables (one row per Year for each metric)
ann_bees <- auc_df %>%
  filter(Variable == "Bombus_Species_remaining") %>%
  group_by(Year) %>%
  summarise(
    SpeciesAUC = dplyr::first(AUC_stan[Removal_Type == base_labels["FG_removal"]]),
    TraitAUC   = dplyr::first(AUC_stan[Removal_Type == base_labels["Species_removal"]]),
    lab = paste0(
      "Species AUC: ", round(SpeciesAUC, 3), "\n",
      "Trait AUC:   ", round(TraitAUC,   3)
    ),
    .groups = "drop"
  )

ann_flowers <- auc_df %>%
  filter(Variable == "Flower_Species_remaining") %>%
  group_by(Year) %>%
  summarise(
    SpeciesAUC = dplyr::first(AUC_stan[Removal_Type == base_labels["FG_removal"]]),
    TraitAUC   = dplyr::first(AUC_stan[Removal_Type == base_labels["Species_removal"]]),
    lab = paste0(
      "Species AUC: ", round(SpeciesAUC, 3), "\n",
      "Trait AUC:   ", round(TraitAUC,   3)
    ),
    .groups = "drop"
  )

# -------------------------------------------------------------------
# C) Summaries for plotting — IMPORTANT: drop Position_removed == 0
# -------------------------------------------------------------------
summary_df <- ext_df %>%
  filter(Position_removed > 0) %>%                 # ← change A
  select(Year, Removal_Type, Position_removed, all_of(metrics)) %>%
  pivot_longer(
    cols      = all_of(metrics),
    names_to  = "Metric",
    values_to = "Value"
  ) %>%
  group_by(Year, Removal_Type, Metric, Position_removed) %>%
  summarise(mean = mean(Value, na.rm = TRUE), .groups = "drop")

# colours & linetypes 
my_cols <- c(
  `Species‐based network\n(remove FG)`      = "#E67E22",
  `Trait‐based network\n(remove species)`   = "#1ABC9C"
)
my_lts <- c(
  `Species‐based network\n(remove FG)`      = "solid",
  `Trait‐based network\n(remove species)`   = "longdash"
)

# -------------------------------------------------------------------
# D) Build the two panels 
# -------------------------------------------------------------------
p_bees <- summary_df %>%
  filter(Metric == "Bombus_Species_remaining") %>%
  ggplot(aes(x = Position_removed, y = mean,
             color = Removal_Type, linetype = Removal_Type)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.6, size = 2) +
  facet_wrap(~ Year, ncol = 3, scales = "fixed") +
  scale_color_manual(values = my_cols, breaks = lvl_order, name = NULL) +  # ← change B (breaks)
  scale_linetype_manual(values = my_lts, breaks = lvl_order, name = NULL) +# ← change B (breaks)
  theme_classic() +
  labs(
    title = "A) Bumblebee species remaining",
    x     = "Removal step",
    y     = "Bumblebee species remaining"
  ) +
  geom_text(
    data    = ann_bees %>% filter(Year != 2024),
    aes(x = Inf, y = Inf, label = lab),
    hjust   = 1.05, vjust = 1.2,
    size    = 4, color = "black",
    inherit.aes = FALSE
  ) +
  # special case for 2024 → bottom‐left (avoids overlap)
  geom_text(
    data    = ann_bees %>% filter(Year == 2024),
    aes(x = -Inf, y = -Inf, label = lab),
    hjust   = -0.05, vjust = -0.5,
    size    = 4, color = "black",
    inherit.aes = FALSE
  )

p_flowers <- summary_df %>%
  filter(Metric == "Flower_Species_remaining") %>%
  ggplot(aes(x = Position_removed, y = mean,
             color = Removal_Type, linetype = Removal_Type)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.6, size = 2) +
  facet_wrap(~ Year, ncol = 3, scales = "fixed") +
  scale_color_manual(values = my_cols, breaks = lvl_order, name = NULL) +
  scale_linetype_manual(values = my_lts, breaks = lvl_order, name = NULL) +
  theme_classic() +
  labs(
    title = "B) Flower species remaining",
    x     = "Removal step",
    y     = "Flower species remaining"
  ) +
  geom_text(
    data    = ann_flowers %>% filter(Year != 2021),
    aes(x = Inf, y = Inf, label = lab),
    hjust   = 1.05, vjust = 1.2,
    size    = 4, color = "black",
    inherit.aes = FALSE
  ) +
  # special case for 2021 → bottom‐left (avoids overlap)
  geom_text(
    data    = ann_flowers %>% filter(Year == 2021),
    aes(x = -Inf, y = -Inf, label = lab),
    hjust   = -0.05, vjust = -0.5,
    size    = 4, color = "black",
    inherit.aes = FALSE
  )

# -------------------------------------------------------------------
# E) Combine and save
# -------------------------------------------------------------------
final_plot <- plot_grid(
  p_bees, p_flowers,
  ncol   = 1,
  labels = c("", ""),   
  align  = "v"
)

ggsave(
  file.path(OutPath, "Annual_Extinction_Mean_Curves_Revised_2.png"),
  plot   = final_plot,
  width  = 10,
  height = 12,
  dpi    = 300
)
message("→ written to ", file.path(OutPath, "Annual_Extinction_Mean_Curves_Revised_2.png"))
