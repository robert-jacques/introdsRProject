install.packages(c("tidyverse", "scales", "reshape2", "RColorBrewer",               # Install packages.
                   "rgl", "patchwork", "car", "factoextra"))


library(tidyverse)                                                                  # Load packages.
library(scales)
library(reshape2)
library(RColorBrewer)
library(rgl)
library(patchwork)
library(car)
library(factoextra)


spotify <- read.csv("https://raw.githubusercontent.com/robert-jacques/introdsRProject/refs/heads/main/data/data.csv")
                                                                                    
                                                                                    # Load dataset.


###############
# DATA CLEANING
###############


head(spotify)                                                                       # Check structure of dataset.

tail(spotify)

str(spotify)


summary(spotify)                                                                    # Check summary statistics for each variable.


sum(duplicated(spotify))                                                            # Check for duplicate rows in dataset.


sum(is.na(spotify))                                                                 # Check for missing values (NA) (No NA values
                                                                                    # in dataset so no further action required).


spotify %>%                                                                         # Check for zero values.
  summarise(across(c(popularity, duration_ms, danceability, energy,   
                     loudness, speechiness, acousticness,
                     instrumentalness, liveness, valence,
                     tempo, time_signature),
                   ~ sum(. == 0)))


spotify <- spotify %>%                                                              # Replace zero values in each column with
  mutate(across(c(popularity, duration_ms, danceability, energy,                    # mean value for that column once zero
                  loudness, speechiness, acousticness,                              # values removed, other than Mode which
                  instrumentalness, liveness, valence,                              # is binary and Key and Time Signature
                  tempo),                                                           # which are categorical.
                ~ ifelse(. == 0, mean(.[. != 0]), .)))


spotify <- spotify %>%                                                              # Replace zero values in Time Signature
  mutate(time_signature = ifelse(time_signature == 0,                               # with the mode for that column as
                                 as.numeric(names(sort(table(time_signature),       # categorical (zero values in Key and
                                                       decreasing = TRUE))[1]),     # Mode are not missing values).
                                 time_signature))


spotify %>%                                                                         # Check that zero values now replaced.
  summarise(across(c(popularity, duration_ms, danceability, energy,
                     loudness, speechiness, acousticness,
                     instrumentalness, liveness, valence,
                     tempo, time_signature),
                   ~ sum(. == 0)))


summary(spotify)



#####################
# DATA PRE-PROCESSING
#####################


spotify_audio_features <- spotify %>%                                               # Select variables of interest (Valence and
  select(danceability, energy, key, loudness, mode,                                 # Audio Features).
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo, time_signature)
  

spotify_audio_features <- spotify_audio_features %>%                                # Normalise all numeric variables to 0-1 scale,
  mutate(                                                                           # other than Mode which is binary and Key and
    across(where(is.numeric) & !any_of(c("key", "mode", "time_signature")),         # Time Signature which are categorical.
           ~ rescale(.)))                                                      


summary(spotify_audio_features)                                                     # Summarise pre-processed data as check.



###########################
# EXPLORATORY DATA ANALYSIS
###########################

############
# Histograms
############


audio_features_hist_01 <- ggplot(spotify_audio_features, aes(x = danceability)) +   # Plot histograms to visualise distribution
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",              # of the selected Audio Features.
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 12500)) +
  labs(x = "Danceability", y = "Frequency") +
  theme_minimal()


audio_features_hist_02 <- ggplot(spotify_audio_features, aes(x = energy)) +
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 12500)) +
    labs(x = "Energy", y = "Frequency") +
    theme_minimal()

  
audio_features_hist_03 <- ggplot(spotify_audio_features, aes(x = factor(key))) +
  geom_bar(stat = "count", fill = "darkblue", colour = "black",
           linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 15000)) +
  labs(x = "Key", y = "Frequency") +
  theme_minimal()

  
audio_features_hist_04 <- ggplot(spotify_audio_features, aes(x = loudness)) +
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 40000)) +
  labs(x = "Loudness", y = "Frequency") +
  theme_minimal()
  

audio_features_hist_05 <- ggplot(spotify_audio_features, aes(x = factor(mode))) +
  geom_bar(stat = "count", fill = "darkblue", colour = "black",
           linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 80000)) +
  labs(x = "Mode", y = "Frequency") +
  theme_minimal()


audio_features_hist_06 <- ggplot(spotify_audio_features, aes(x = speechiness)) +
  geom_histogram(binwidth = 0.04, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 50000)) +
  labs(x = "Speechiness", y = "Frequency") +
  theme_minimal()
  

audio_features_hist_07 <- ggplot(spotify_audio_features, aes(x = acousticness)) +
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 40000)) +
  labs(x = "Acousticness", y = "Frequency") +
  theme_minimal()


audio_features_hist_08 <- ggplot(spotify_audio_features, aes(x = instrumentalness)) +
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 50000)) +
  labs(x = "Instrumentalness", y = "Frequency") +
  theme_minimal()
  

audio_features_hist_09 <- ggplot(spotify_audio_features, aes(x = liveness)) +
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 50000)) +
  labs(x = "Liveness", y = "Frequency") +
  theme_minimal()


audio_features_hist_10 <- ggplot(spotify_audio_features, aes(x = valence)) +
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 8000)) +
  labs(x = "Valence", y = "Frequency") +
  theme_minimal()


audio_features_hist_11 <- ggplot(spotify_audio_features, aes(x = tempo)) +
  geom_histogram(binwidth = 0.05, fill = "darkblue", colour = "black",
                 linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 20000)) +
  labs(x = "Tempo", y = "Frequency") +
  theme_minimal()
 

audio_features_hist_12 <- ggplot(spotify_audio_features, aes(x = factor(time_signature))) +
  geom_bar(stat = "count", fill = "darkblue", colour = "black",
           linewidth = 0.2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 125000)) +
  labs(x = "Time Signature", y = "Frequency") +
  theme_minimal()


audio_features_hist_sum <- (audio_features_hist_01 | audio_features_hist_02 |       # Faceted plot of histograms 1-12 for visual
                              audio_features_hist_03 | audio_features_hist_04) /    # summary.
  (audio_features_hist_05 | audio_features_hist_06 |
     audio_features_hist_07 | audio_features_hist_08) /
  (audio_features_hist_09 | audio_features_hist_10 |
     audio_features_hist_11 | audio_features_hist_12) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Frequency Distributions",
                  subtitle = "The distribution of each audio feature",
                  caption = "Source: Spotify Tracks Dataset") &
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -1.5),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        plot.margin = margin(10, 10, 10, 10))

audio_features_hist_sum


##########
# Boxplots
##########


spotify_audio_features_long <- spotify_audio_features %>%                           # Reshape data into long format for plotting
  pivot_longer(cols = c(danceability, energy, loudness, speechiness,                # of boxplots.
                        acousticness, instrumentalness, liveness,
                        valence, tempo),
               names_to = "attribute",
               values_to = "value")


audio_features_box_01 <- ggplot(spotify_audio_features_long,                        # Plot boxplots to further visualise
                             aes(x = attribute, y = value)) +                       # distribution of Audio Features.          
  geom_boxplot(fill = brewer.pal(8, "Set2")[1],                                     
               colour = "black",
               alpha = 0.6,
               notch = FALSE,
               width = 0.5,
               size = 0.3,
               outlier.colour = "darkorange",
               outlier.size = 2,         
               outlier.shape = 1,
               outlier.alpha = 0.5) +
  labs(title = "Boxplots",
       subtitle = "The distribution of each audio feature",
       caption = "Source: Spotify Tracks Dataset",
       x = "Audio Feature", y = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -1.5),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10))

audio_features_box_01


##############
# Scatterplots
##############


audio_features_scat_01 <- ggplot(spotify_audio_features,                            # Plot scatterplots of Valence vs. other
                                 aes(x = valence, y = danceability)) +              # Audio Features to visualise            
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +                      # relationships.
  labs(x = "Valence", y = "Danceability") +
  theme_minimal()


audio_features_scat_02 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = energy)) +
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Energy") +
  theme_minimal()


audio_features_scat_03 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = key)) +
  geom_jitter(width = 0, height = 0.01, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Key") +
  theme_minimal()


audio_features_scat_04 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = loudness)) +
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Loudness") +
  theme_minimal()


audio_features_scat_05 <- ggplot(spotify_audio_features,
                                  aes(x = valence, y = mode)) +
  geom_jitter(width = 0, height = 0.01, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Mode") +
  theme_minimal()


audio_features_scat_06 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = speechiness)) +
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Speechiness") +
  theme_minimal()


audio_features_scat_07 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = acousticness)) +
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Acoustiness") +
  theme_minimal()


audio_features_scat_08 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = instrumentalness)) +
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Instrumentalness") +
  theme_minimal()


audio_features_scat_09 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = liveness)) +
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Liveness") +
  theme_minimal()


audio_features_scat_10 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = tempo)) +
  geom_point(size = 0.1, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Tempo") +
  theme_minimal()


audio_features_scat_11 <- ggplot(spotify_audio_features,
                                 aes(x = valence, y = time_signature)) +
  geom_jitter(width = 0, height = 0.01, colour = "darkorange", alpha = 0.5) +
  labs(x = "Valence", y = "Time Signature") +
  theme_minimal()


audio_features_scat_sum <- (audio_features_scat_01 | audio_features_scat_02 |       # Faceted plot of scatterplots 1-11 for
                              audio_features_scat_03 | audio_features_scat_04) /    # visual summary.
  (audio_features_scat_05 | audio_features_scat_06 |
     audio_features_scat_07 | audio_features_scat_08) /                         
  (audio_features_scat_09 | audio_features_scat_10 |
     audio_features_scat_11) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Distributions",
                  subtitle = "The relationship between Valence (Mood) and each other audio feature",
                  caption = "Source: Spotify Tracks Dataset") &
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -1.5),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        plot.margin = margin(10, 10, 10, 10))

audio_features_scat_sum


#############
# Correlation
#############


spotify_audio_features_cor_matrix <- cor(spotify_audio_features)                    # Correlation matrix of Audio Features.
                                                                                    
spotify_audio_features_cor_matrix                                                   # Calculate numeric correlations between
                                                                                    # Audio Features.

spotify_audio_features_cor_matrix_melted <- melt(spotify_audio_features_cor_matrix)


audio_features_cor_heatmap_01 <- ggplot(spotify_audio_features_cor_matrix_melted,   # Plot correlation heatmap of Audio Features
                                        aes(x = Var1, y = Var2, fill = value)) +    # to visualise relationships.
  geom_tile() +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027",
                       midpoint = 0) +
  geom_text(aes(label = round(value, 2)),                                           # Annotate heatmap with rounded correlation
            colour = "black", size = 3) +                                           # values.
  labs(title = "Correlation Heatmap",
       subtitle = "Representation of how audio features correlate with one another",
       caption = "Source: Spotify Tracks Dataset",
       x = "Audio Features", y = "Audio Features",
       fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -1.5),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = margin(10, 10, 10, 10))
 
audio_features_cor_heatmap_01       


spotify_audio_features_valence_cor_matrix <-                                        
  spotify_audio_features_cor_matrix["valence", ]                                                  
                                                                                 
                                                                                    
spotify_audio_features_valence_cor_matrix                                           # Calculate numeric correlations between
                                                                                    # Valence and other Audio Features.

spotify_audio_features_cor_matrix_melted %>%                                        # Check for multicollinearity (No values
  filter(abs(value) > 0.8 & Var1 != Var2)                                           # > 0.8 so no further action required).
  


###################################
# FURTHER DATA PRE-PROCESSING & EDA
###################################


spotify_audio_features <- spotify %>%                                               # Re-select variables of interest (Valence
  select(danceability, energy, key, loudness, mode,                                 # and Audio Features).
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo, time_signature)


filter_outliers_using_iqr <- function(df, columns_to_clean) {
  df %>%
    mutate(across(all_of(columns_to_clean), ~ {
      Q1 <- quantile(., 0.25, na.rm = TRUE)                                         # Calculate Q1, Q3, and IQR for each
      Q3 <- quantile(., 0.75, na.rm = TRUE)                                         # specified column.
      IQR_value <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR_value                                           # Define lower and upper bounds for outliers.
      upper_bound <- Q3 + 1.5 * IQR_value
      
      ifelse(. >= lower_bound & . <= upper_bound, ., NA)
    }))
}

audio_features_with_outliers <- c("danceability", "energy", "loudness",             # Define audio features with outliers.
                                  "speechiness", "acousticness",
                                  "instrumentalness", "liveness",
                                  "valence", "tempo")


spotify_audio_features <- spotify_audio_features %>%                                # Replace outliers with NA values.
  filter_outliers_using_iqr(audio_features_with_outliers) %>%
  drop_na()                                                                         # Remove rows with NA values after outlier removal.


spotify_audio_features <- spotify_audio_features %>%                                # Transform audio features based on
  mutate(                                                                           # histogram and boxplot distributions
    energy = sqrt((max(energy) - energy) + 1e-6),                                   # (to reduce skew and improve
    speechiness = 1 / (speechiness + 1e-6),                                         # normality of distribution).
    acousticness = sqrt(acousticness + 1e-6),
    instrumentalness = sqrt(instrumentalness + 1e-6), 
    liveness = sqrt(liveness + 1e-6))



spotify_audio_features <- spotify_audio_features %>%                                # Normalise all numeric variables to 0-1 scale,
  mutate(                                                                           # other than Mode which is binary and Key and
    across(where(is.numeric) & !any_of(c("key", "mode", "time_signature")),         # Time Signature which are categorical.
           ~ rescale(.)))


summary(spotify_audio_features)                                                     # Summarise pre-processed again data as check.



############################
# MULTIPLE LINEAR REGRESSION
# Baseline model (1).
############################


set.seed(17)                                                                        # Set seed to reproduce output.

spotify_audio_features_data <- spotify_audio_features[sample(1:nrow(spotify_audio_features)),]    
                                                                                    # Shuffle rows of spotify_audio_features dataframe into random order.


train_size = 0.8                                                                    # Set train / test split (80 / 20).

spotify_audio_features_data.train <- spotify_audio_features_data[1:(train_size *
                                                nrow(spotify_audio_features_data)), ]          
                                                                                    # Randomly separate dataframe into train and test data.

spotify_audio_features_data.test <- spotify_audio_features_data[(floor(train_size *
                                                   nrow(spotify_audio_features_data)) + 1):nrow(spotify_audio_features_data), ]


spotify_audio_features_model <- lm(valence ~ danceability + energy + key            # Baseline multiple linear regression model.
                                   + loudness + mode + speechiness
                                   + acousticness + instrumentalness
                                   + liveness + tempo + time_signature,
                                   data = spotify_audio_features_data.train)


spotify_audio_features_model                                                        # Run MLR model.


predict(spotify_audio_features_model,                                               # Generate predictions from fitted model
        newdata = spotify_audio_features_data.test,                                 # using test data and calculate
        interval = "confidence")                                                    # confidence intervals.


spotify_audio_features_data.test_copy <- spotify_audio_features_data.test           # Create copy of test data.

spotify_audio_features_data.test_copy$predicted <- predict(spotify_audio_features_model,
                                                           newdata = spotify_audio_features_data.test_copy)
                                                                                    # Generate predictions from fitted model
                                                                                    # using test data and store in copy of
                                                                                    # test data.

spotify_audio_features_data.test_copy$residuals <- spotify_audio_features_data.test_copy$predicted - spotify_audio_features_data.test_copy$valence
                                                                                    # Calculate residuals and store in copy
                                                                                    # of test data.
spotify_audio_features_data.test_copy


summary(spotify_audio_features_model)                                               # Summarise MLR model results.


sse_audio_features_model <- sum(spotify_audio_features_data.test_copy$residuals ^ 2)
                                                                                    # Calculate Sum of Squared Errors.
print(paste("Sum of Squared Errors: ", sse_audio_features_model))


rmse_audio_features_model <- sqrt(mean(spotify_audio_features_data.test_copy$residuals ^ 2))
                                                                                    # Calculate Root Mean Squared Error.
print(paste("Root Mean Squared Error: ", rmse_audio_features_model))


vif(spotify_audio_features_model)                                                   # Calculate Variance Inflation Factor to
                                                                                    # check for model multicollinearity.

plot(spotify_audio_features_model, which = 1)                                       # Plot residuals vs. fitted values to
                                                                                    # check for patterns in residuals.

plot(spotify_audio_features_model, which = 2)                                       # Plot normal Q-Q residuals to check
                                                                                    # whether residuals normally
                                                                                    # distributed.

##############################
# PRINCIPAL COMPONENT ANALYSIS
##############################


spotify_pca_data <- prcomp(spotify_audio_features[, c(1:9, 11, 12)], scale. = TRUE)
                                                                                    # Perform Principal Component Analysis
                                                                                    # (PCA) on Audio Features excluding
                                                                                    # Valence.

summary(spotify_pca_data)                                                           # Summarise PCA results (proportion
                                                                                    # of variance explained by each PC).

 
fviz_eig(spotify_pca_data)                                                          # Plot scree plot to visualise
                                                                                    # proportion of variance explained
                                                                                    # by each PC.


spotify_pca_loading <- data.frame(dimensions = colnames(spotify_audio_features[c(1:9, 11, 12)]),
                                  PC1 = spotify_pca_data$rotation[, 1],
                                  PC2 = spotify_pca_data$rotation[, 2])

spotify_pca_loading_plot_01 <- ggplot(spotify_pca_loading,                          # Plot PCA loading plot to visualise
                                      aes(x = PC1, y = PC2, label = dimensions)) +  # component loadings for PC1 and PC2.            
  geom_text(size = 4, colour = brewer.pal(8, "Set2")[2], fontface = "bold") +      
  labs(title = "Loading plot of audio features",
       subtitle = "PCA component loadings, PC1 & PC2",
       x = "Principal Component 1", y = "Principal Component 2",
       caption = "Source: Spotify Tracks Dataset") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -1.5),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        plot.margin = margin(10, 10, 10, 10))

spotify_pca_loading_plot_01


spotify_pca_data <- data.frame(spotify_pca_data$x[, 1:11],                          # Create new dataframe of PC1-11
                               valence = spotify_audio_features$valence)            # and Valence for PCA-based MLR
                                                                                    # model.


############################
# MULTIPLE LINEAR REGRESSION
# PCA-based model (2).
############################


set.seed(17)                                                                        # Set seed to reproduce output.

spotify_pca_data <- spotify_pca_data[sample(1:nrow(spotify_pca_data)),]             # Shuffle rows of spotify_pca_data dataframe into random order.


train_size = 0.8                                                                    # Set train / test split (80 / 20).

spotify_pca_data.train <- spotify_pca_data[1:(train_size *
                                                nrow(spotify_pca_data)), ]          # Randomly separate dataframe into train and test data.

spotify_pca_data.test <- spotify_pca_data[(floor(train_size *
                                                   nrow(spotify_pca_data)) + 1):nrow(spotify_pca_data), ]


spotify_pca_model <- lm(valence ~ PC1 + PC2 + PC3 + PC6 + PC7 + PC8 + PC9 + PC10,   # PCA-based multiple linear regression model.
                        data = spotify_pca_data.train)


spotify_pca_model                                                                   # Run MLR model.


predict(spotify_pca_model,                                                          # Generate predictions from fitted model
        newdata = spotify_pca_data.test,                                            # using test data and calculate
        interval = "confidence")                                                    # confidence intervals.
        

spotify_pca_data.test_copy <- spotify_pca_data.test                                 # Create copy of test data.

spotify_pca_data.test_copy$predicted <- predict(spotify_pca_model,
                                               newdata = spotify_pca_data.test_copy)
                                                                                    # Generate predictions from fitted model
                                                                                    # using test data and store in copy of
                                                                                    # test data dataset.

spotify_pca_data.test_copy$residuals <- spotify_pca_data.test_copy$predicted - spotify_pca_data.test_copy$valence
                                                                                    # Calculate residuals and store in copy
                                                                                    # of test data dataset.
spotify_pca_data.test_copy


summary(spotify_pca_model)                                                          # Summarise MLR model results.


sse_pca_model <- sum(spotify_pca_data.test_copy$residuals ^ 2)                      # Calculate Sum of Squared Errors.

print(paste("Sum of Squared Errors: ", sse_pca_model))


rmse_pca_model <- sqrt(mean(spotify_pca_data.test_copy$residuals ^ 2))              # Calculate Root Mean Squared Error.

print(paste("Root Mean Squared Error: ", rmse_pca_model))


vif(spotify_pca_model)                                                              # Calculate Variance Inflation Factor to
                                                                                    # check for model multicollinearity.

plot(spotify_pca_model, which = 1)                                                  # Plot residuals vs. fitted values to
                                                                                    # check for patterns in residuals.

plot(spotify_pca_model, which = 2)                                                  # Plot normal Q-Q residuals to check
                                                                                    # whether residuals normally
                                                                                    # distributed.
#############################
# Predicted vs Actual Valence
# Scatterplots
#############################


spotify_audio_features_model_scat_01 <- ggplot(spotify_audio_features_data.test_copy,
                                               aes(x = valence, y = predicted)) +
  geom_point(size = 0.2, colour = brewer.pal(3, "Set2")[1], alpha = 0.7, ) +
  geom_smooth(method = "lm", colour = brewer.pal(3, "Set2")[2], se = FALSE) +
  labs(title = "Baseline model (1)",
       x = "Actual Valence", y = "Predicted Valence") +
  scale_y_continuous(limits = c(-0.05, 1)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))


spotify_pca_model_scat_01 <- ggplot(spotify_pca_data.test_copy,
                                    aes(x = valence, y = predicted)) +
  geom_point(size = 0.2, colour = brewer.pal(3, "Set2")[3], alpha = 0.7) +
  geom_smooth(method = "lm", colour = brewer.pal(3, "Set2")[2], se = FALSE) +
  labs(title = "PCA-based model (2)",
       x = "Actual Valence", y = "Predicted Valence") +
  scale_y_continuous(limits = c(-0.05, 1)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))


spotify_audio_features_model_scat_sum <- (spotify_audio_features_model_scat_01 | spotify_pca_model_scat_01) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Predicted vs Actual Valence",
                  subtitle = "Baseline and PCA-based multiple linear regression models",
                  caption = "Source: Spotify Tracks Dataset") &
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -1.5),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        plot.margin = margin(10, 10, 10, 10))

spotify_audio_features_model_scat_sum