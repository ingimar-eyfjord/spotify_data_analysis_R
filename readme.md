# The melodies of music

> A data-driven review of Spotify's audio features and their use case for genre classification and recommender systems.

This repo is the code for an exam paper I wrote during my Master's Degree in Information Science. The paper is available on Medium [here]()

## Code documentation

### Importing libraries

```r
library(tidyverse)
library(ggplot2)
library(corrplot)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(hrbrthemes)
library(patchwork)
library(ggrepel)
library(ggtext)
library(showtext)
library(gganimate)
library(rcartocolor)
library(transformr)
library(tweenr)
library(tidytext)
library(colorRamps)
library(ggcorrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(lsa)
library(plotly)
library(scales)
library(httr)
library(jsonlite)
library(dotenv)
library(spotifyr)
library(knitr)
library(caret)
library(tm)
library(kableExtra)
library(dbscan)
library(FNN)
library(splitstackshape)
library(wordcloud)
```

## Theming

The following code is used to theme the plots in the paper. Where the goal was to replicate Spotify's color scheme and font.

```r
# Adding Montserrat font to the plot (similar to Spotify's font)
font_add_google("Montserrat")
showtext_auto()
# Applying fonts to the theme ipsum
theme_set(theme_ipsum(base_family = "Montserrat"))
# Applying the custom theme
theme_update(
    # Remove title for both x and y axes
    axis.title = element_blank(),
    # Axes labels are grey
    axis.text = element_text(color = "black"),
    # The size of the axes labels are different for x and y.
    axis.text.x = element_text(size = 20, margin = ggplot2::margin(t = 5)),
    axis.text.y = element_text(size = 17, margin = ggplot2::margin(r = 5)),
    # Also, the ticks have a very light grey color
    axis.ticks = element_line(color = "#1DB954", size = .5),
    # The length of the axis ticks is increased.
    axis.ticks.length.x = unit(1.3, "lines"),
    axis.ticks.length.y = unit(.7, "lines"),
    # Remove the grid lines that come with ggplot2 plots by default
    # panel.grid = element_blank(),
    # Customize margin values (top, right, bottom, left)
    plot.margin = ggplot2::margin(20, 20, 20, 20),
    # Use a light grey color for the background of both the plot and the panel
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    # Customize title appearence
    plot.title = element_text(
        color = "#1DB954",
        size = 40,
        face = "bold",
        margin = ggplot2::margin(t = 15)
    ),
    # Customize subtitle appearence
    plot.subtitle = element_markdown(
        color = "darkorchid4",
        size = 20,
        lineheight = 1.35,
        margin = ggplot2::margin(t = 15, b = 20)
    ),
    # Title and caption are going to be aligned
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_markdown(
        color = "darkorchid4",
        size = 20,
        lineheight = 1.2,
        hjust = 0,
        margin = ggplot2::margin(t = 10) # Large margin on the top of the caption.
    ),
    # Remove legend if you don't want any.
    # legend.position = "none"
)
# A function to create a custom color gradient
custom_color_gradient <- colorRampPalette(colors = c("darkorchid4", "white", "#1DB954"))

# A function to retrieve predefined colors
kp_cols <- function(...) {
    kp_colors <- c(
        purple = "#490B32",
        red = "#9A031E",
        orange = "#FB8B24",
        dark_orange = "#E36414",
        dark_blue = "#0F4C5C",
        grey = "#66717E",
        light_green = "#B3CBB9",
        blue = "#5DA9E9",
        purple = "darkorchid4",
        green = "1DB954"
    )

    cols <- c(...)

    if (is.null(cols)) {
        return(kp_colors)
    }

    kp_colors[cols]
}
```

## Using Spotify's API

The following code is used to retrieve data from Spotify's API. The code uses the `dotenv` package to load the environment variables from the `.env` file. The `.env` file is not included in this repo for security reasons. The `.env` file should look like this:

```bash
SPOTIFY_CLIENT_ID=your_client_id
SPOTIFY_CLIENT_SECRET=your_client_secret
SPOTIFY_REDIRECT_URI=your_redirect_uri
SPOTIFY_USER=your_user_id
```

These variables can be retrieved from the [Spotify Developer Dashboard](https://developer.spotify.com/dashboard/).

> Keep in mind, you need to create an account and a project in the Spotify Developer Dashboard to get the credentials (which is free).

**Load the .env file**

```r
load_dot_env(".env")
```

**Access the environment variablesI**

This accesses the environment variables from the `.env` file and sets them as variables in the R environment.

```r
client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
redirect_uri <- Sys.getenv("SPOTIFY_REDIRECT_URI")
user_id <- Sys.getenv("SPOTIFY_USER")
```

**Authenticating in and retrieve authentication token**

The following code uses oauth to open a popup to the browser to authenticate a user and retrieve a Bearer token from your Spotify app. The authentication token is used to authenticate the user for subsequent API calls.

```r
# Spotify API endpoint
spotify_endpoint <- oauth_endpoint(
    authorize = "<https://accounts.spotify.com/authorize>",
    access = "<https://accounts.spotify.com/api/token>"
)
# Client ID and secret are based on your Spotify app
spotify_app <- oauth_app(
    "spotify",
    key = client_id,
    secret = client_secret,
    redirect_uri = "<http://localhost:1410/>"
)
# Scopes are based on the permissions you want to give your app
# I did not use all of these scopes, but I left some in here as an example
# You need to set the scopes in the Spotify Developer Dashboard as well
spotify_scopes <- c(
    "user-read-email",
    "user-read-private",
    "playlist-read-private",
    "playlist-modify-public",
    "playlist-modify-private",
    "user-top-read"
)
# This opens a popup in the browser to authenticate the user
token <- oauth2.0_token(
    endpoint = spotify_endpoint,
    app = spotify_app,
    scope = spotify_scopes,
    type = "code",
    cache = FALSE,
)

# This converts the token$credentials to raw
credentials_raw <- token$credentials
# This converts the credentials to a character
credentials_char <- rawToChar(credentials_raw)
# This converts the credentials to a list
credentials_list <- fromJSON(credentials_char)

# This is the access token that is used for subsequent API calls
access_token <- credentials_list$access_token
# This is the authorization header that is used for subsequent API calls
auth_header <- paste("Bearer", access_token)
# This is the API call to retrieve the user's profile

```

**Get the user's profile**

This retrieves the user's profile from the API. The user's profile is used to retrieve the user's display name, followers, and user id ect.

```r
# This is the API call to retrieve the user's profile
profile_url <- "<https://api.spotify.com/v1/me>"
response <- GET(profile_url, add_headers(Authorization = auth_header))

# Parse the JSON response and convert it to a data frame
profile_list <- fromJSON(rawToChar(response$content))
profile_df <- list(
    display_name = profile_list$display_name,
    followers_total = profile_list$followers$total,
    id = profile_list$id
)
```

**Get the user's top items**

This retrieves the user's top 50 items from the API. The function uses the `get_top_items` function to retrieve the user's top 50 artists and tracks. The function can be modified to retrieve the user's top 50 albums as well. The function takes the access token, type, time range, limit, and offset as arguments. The type can be either `artists` or `tracks`. The time range can be either `short_term`, `medium_term`, or `long_term`. The limit can be any number between 1 and 50. The offset can be any number between 0 and 49. The function returns a list of the user's top 50 items.

```r

get_top_items <- function(access_token, type, time_range = "medium_term", limit = 100, offset = 0) {
    base_url <- "<https://api.spotify.com/v1/me/top>"
    url <- paste0(base_url, "/", type)

    auth_header <- paste("Bearer", access_token)

    response <- GET(url,
        add_headers(Authorization = auth_header),
        query = list(
            time_range = time_range,
            limit = limit,
            offset = offset
        )
    )

    if (status_code(response) != 200) {
        print("Error in API request:")
        print(fromJSON(rawToChar(response$content)))
        return(NULL)
    }

    top_items <- fromJSON(rawToChar(response$content))
    return(top_items)
}
```

**Get the user's top artists**

In the report I did not need to use my top artists so it's commented out, but the following code can be used to retrieve the user's top artists.

```r
top_artists <- get_top_items(access_token, "artists", time_range = "medium_term", limit = 10, offset = 0)
```

**Get the user's top songs**

The following function retrieves your top 50 songs.

```r
top_tracks <- get_top_items(access_token, "tracks", time_range = "medium_term", limit = 50, offset = 0)
View(top_tracks$items)

items_list <- top_tracks$items

# Transform the list into a data frame
tracks_df <- items_list %>%
    select(-c(album, available_markets, disc_number, explicit, external_ids, external_urls, href, is_local, preview_url, track_number, type, uri))
```

**Get the audio features for the user's top songs**

The following function retrieves the audio features for a song. The function takes the access token and track id as arguments. The function returns a list of the audio features for the track. Calling it with `sapply` returns a list of lists. Calling `t` on the list of lists returns a data frame, finally merging the tracks and audio features data frames returns a data frame with the user's top songs and their audio features.

```r
get_audio_features <- function(access_token, track_id) {
    base_url <- "<https://api.spotify.com/v1/audio-features>"
    url <- paste0(base_url, "/", track_id)

    auth_header <- paste("Bearer", access_token)

    response <- GET(url, add_headers(Authorization = auth_header))

    if (response$status_code != 200) {
        warning("Error in API request:")
        print(response)
        return(NULL)
    }

    audio_features <- fromJSON(rawToChar(response$content))
    return(audio_features)
}

# For each track id, get the audio features
audio_features_list <- sapply(tracks_df$id, function(track_id) get_audio_features(access_token, track_id))
# Transform the list into a data frame
audio_features_df <- t(audio_features_list)

# Merge the tracks and audio features data frames
merged_df <- merge(tracks_df, audio_features_df, by = "id")
```

**Data preporcessing**

To prepare the data to fit with data from the `tidytuesdayR` package, the following code renames the columns and removes the columns that are not needed. The code also converts the data types of the columns to numeric.

```r
# Rename the columns
merged_df <- merged_df %>% rename(track_popularity = popularity)
merged_df <- merged_df %>% rename(duration_ms = duration_ms.y)
merged_df <- merged_df %>% rename(track_id = id)
merged_df <- merged_df %>% rename(track_name = name)
merged_df <- merged_df %>% rename(track_artist = artists)

# Remove the columns that are not needed
column_index <- which(names(merged_df) == "duration_ms.x")
merged_df <- merged_df[, -column_index]

# Convert the data types of the columns to numeric
merged_df$duration_ms <- as.numeric(merged_df$duration_ms)
merged_df$speechiness <- as.numeric(merged_df$speechiness)
merged_df$acousticness <- as.numeric(merged_df$acousticness)
merged_df$instrumentalness <- as.numeric(merged_df$instrumentalness)
merged_df$liveness <- as.numeric(merged_df$liveness)
merged_df$valence <- as.numeric(merged_df$valence)
merged_df$tempo <- as.numeric(merged_df$tempo)
merged_df$energy <- as.numeric(merged_df$energy)
merged_df$danceability <- as.numeric(merged_df$danceability)
merged_df$key <- as.numeric(merged_df$key)
merged_df$loudness <- as.numeric(merged_df$loudness)
merged_df$mode <- as.numeric(merged_df$mode)

# rename the data frame to users top 50
users_top_50 <- merged_df
```

**Get the data from the `tidytuesdayR` package**

The following code gets the data from the `tidytuesdayR` package. The data is from the `spotify_songs` data set. The data is from the `tidytuesdayR` package is from 2020-01-21.

```r
get_tidytuesday_songs <- function() {
    # Get the Data
    spotify_songs <- readr::read_csv("<https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv>")

    # Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
    # PLEASE NOTE TO USE 2020 DATA YOU NEED TO UPDATE tidytuesdayR from GitHub

    # Either ISO-8601 date or year/week works!

    # Install via devtools::install_github("thebioengineer/tidytuesdayR")

    tuesdata <- tidytuesdayR::tt_load("2020-01-21")
    tuesdata <- tidytuesdayR::tt_load(2020, week = 4)

    ## Define the data as spotify_songs

    return(tuesdata$spotify_songs)
}
spotify_songs <- get_tidytuesday_songs()
```

**Define feature names**

This defines the feature names for the audio features. This is used later in multiple places.

```r
feature_names <- c(
    "danceability", "energy", "key", "loudness", "mode", "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo",
    "duration_ms", "duration_sec"
)
```

### Exploratory Data Analysis

The following function is defined to perform the exploratory data analysis, it accepts the song datasets as paraemters and therefore we can call it twice, once for the TidyTuesday dataset and once for the user's top songs dataset.

```r
# Function: eda_function
# Description: Performs exploratory data analysis (EDA) on the Spotify songs dataset.
# Parameters:
#   - spotify_songs: Data frame containing the Spotify songs dataset.
# Returns: None

eda_function <- function(spotify_songs) {
  
  # Check if playlist_genre column exists in the dataset (Dataset A)
  # If not, add a new column called playlist_genre with "-" values for all rows (Dataset B)
  if (!"playlist_genre" %in% names(spotify_songs)) {
    spotify_songs <- spotify_songs %>%
      mutate(playlist_genre = "-")
  }
  
  # Plot the distribution of duration with outliers
  with_outliers <- spotify_songs %>%
    ggplot(aes(y = duration_ms)) +
    geom_boxplot(color = kp_cols("red"), coef = 4) +
    coord_flip() +
    labs(title = "Duration")
  
  # Find and remove the outliers
  duration_outliers <- boxplot(spotify_songs$duration_ms, plot = FALSE, range = 4)$out
  spotify_songs_no_outliers <- spotify_songs %>%
    filter(!duration_ms %in% duration_outliers)
  
  # Plot the distribution of duration without outliers
  without_outliers <- spotify_songs_no_outliers %>%
    ggplot(aes(y = duration_ms)) +
    geom_boxplot(color = kp_cols("red"), coef = 4) +
    coord_flip() +
    labs(title = "Duration, outliers removed")
  
  # Plot the two graphs side by side
  gridExtra::grid.arrange(with_outliers, without_outliers, ncol = 1)
  
  # Print the number of songs in the dataset
  nrow(spotify_songs)
  
  # View the number of songs per genre
  spotify_songs %>%
    group_by(playlist_genre) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  # Define duration_sec and duration_min columns
  spotify_songs <- spotify_songs %>%
    mutate(
      duration_sec = duration_ms / 1000,
      duration_min = duration_sec / 60
    )
  
  # Plot the density of audio features
  audio_feature_density <- spotify_songs %>%
    select(c("playlist_genre", feature_names)) %>%
    pivot_longer(cols = feature_names) %>%
    filter(name != "duration_ms") %>%
    ggplot(aes(x = value)) +
    geom_density(aes(color = playlist_genre), alpha = 0.5) +
    facet_wrap(~name, ncol = 3, scales = "free") +
    labs(title = "", x = "", y = "density") +
    theme(axis.text.y = element_blank())
  
  # Print the audio feature density plot
  print(audio_feature_density)
  
  # Check the structure of the data
  str(spotify_songs)
  
  # Get data statistics
  
  # Summary statistics for numeric variables
  summary_list <- summary(spotify_songs[, c("track_popularity", "danceability", "energy", "key", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")])
  View(summary_list)
  
  # Frequencies for categorical variables
  frequency_of_playlist_genre <- table(spotify_songs$playlist_genre)
  View(frequency_of_playlist_genre)
  

    frequency_of_playlist_subgenre <- table(spotify_songs$playlist_subgenre)
    View(frequency_of_playlist_subgenre)

    # Missing values
    missing_values <- sum(is.na(spotify_songs))


    distribution_of_numerics <- summary(spotify_songs[, c("track_popularity", "danceability", "energy", "key", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")])

    # Visualize the distribution of continuous variables: You can use histograms, density plots, or box plots to visualize the distribution of continuous variables like danceability, energy, loudness, and tempo. For example, to create a histogram for danceability:
    ## Distribution of Dancaibility -
    histogram <- ggplot(spotify_songs, aes(x = danceability)) +
        geom_histogram(binwidth = 0.05, fill = "#1DB954", alpha = 0.7, color = "white") +
        geom_vline(aes(xintercept = mean(danceability)),
            color = "darkorchid4", linetype = "dashed", size = 1
        ) +
        labs(x = "", y = "")
   
    print(histogram)

    # Visualize relationships between continuous variables: Use scatter plots or pairs plots to visualize relationships between continuous variables. For example, to create a scatter plot of energy vs. loudness:
    continous <- ggplot(spotify_songs, aes(x = energy, y = loudness, color = "#1DB954")) +
        geom_point(alpha = 0.5, color = "#1DB954") +
        labs(title = "Energy vs. Loudness", subtitle = "Scatter Plot of Energy vs. Loudness", x = "Energy", y = "Loudness", caption = caption)
    print(continous)

    # Correlation Matrix of Numerics
    # Visualize relationships between categorical and continuous variables: Use box plots or violin plots to visualize relationships between categorical variables like playlist_genre and continuous variables like danceability. For example, to create a box plot of playlist_genre vs. danceability:
    playlist_genre <- ggplot(spotify_songs, aes(x = playlist_genre, y = danceability)) +
        geom_boxplot(alpha = 0.7, fill = "#1DB954") +
        labs(title = "", x = "", y = "")
    print(playlist_genre)

    # Examine correlations between continuous variables: Calculate the correlation matrix and visualize it using a correlation plot. For example, for variables like danceability, energy, loudness, and valence:
    spotify_songs_continuous <- spotify_songs[, c("danceability", "energy", "key", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "loudness")]
    correlation_matrix <- cor(spotify_songs_continuous)
    corrolation_plot <- corrplot(correlation_matrix, method = "circle", col = custom_color_gradient(100))

    # Create the ggplot2-based correlation plot
    ggcorrplot_object <- ggcorrplot(
        correlation_matrix,
        hc.order = TRUE,
        hc.method = "complete",
        lab = TRUE,
        outline.color = "darkorchid4",
        type = "lower",
        pch.col = "black",
        pch.cex = 5,
        tl.cex = 12,
        tl.srt = 45,
    ) +
        scale_fill_gradient2(low = "darkorchid4", mid = "white", high = "#1DB954", midpoint = 0) +
        theme(
            plot.title = element_text(
                color = "#1DB954",
                size = 40,
                face = "bold",
                margin = ggplot2::margin(t = 15)
            ),
            plot.subtitle = element_text(
                color = "darkorchid4",
                size = 20,
                margin = ggplot2::margin(t = 15, b = 20)
            ),
            legend.text = element_text(face = "bold", color = "darkorchid4"),
        )

    print(ggcorrplot_object)

    # Create a different looking correlation plot
    corr_matrix <- cor(spotify_songs[, c("danceability", "energy", "key", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms", "loudness")])
    print(corr_matrix)

    correlation_plot_2 <- corrplot(
        corr_matrix,
        type = "lower",
        mar = c(2, 2, 2, 2),
        order = "hclust",
        number.cex = 0.9,
        addCoef.col = "grey30",
        method = "color",
        tl.cex = 1.2,
        tl.col = "darkorchid4",
        tl.srt = 45,
        # cl.pos = "n",
        cl.ratio = 0.2,
        insig = "blank",
        diag = FALSE,
        col = custom_color_gradient(100)
    )
    # Print the correlation plot
    print(correlation_plot_2)
}
```

**Call the function**

```r
eda_function(spotify_songs)
eda_function(users_top_50)
```

## Classification

The following function will be used to train a decision tree model to predict the playlist genre of a song.

If starts by

```r
# Function: classification_function_decision_tree
# Description: Performs classification using a decision tree model on Spotify songs dataset.
# Parameters:
#   - spotify_songs: Data frame containing the Spotify songs dataset.
# Returns:
#   - results: List of classification results including accuracy and evaluation metrics.

classification_function_decision_tree <- function(spotify_songs) {
  
  # Define the feature names used for classification
  feature_names <- c(
    "danceability", "energy", "key", "mode", "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo",
    "duration_ms"
  )
  
  # Scale the numeric features in the dataset
  playlist_songs_scaled <- spotify_songs %>%
    mutate_if(is.numeric, scale)
  
  # Split the dataset into training and testing sets
  set.seed(19234)
  train_index <- createDataPartition(y = spotify_songs$playlist_genre, p = 0.8, list = FALSE)
  train_set <- playlist_songs_scaled[train_index, c("playlist_genre", feature_names)]
  test_set <- playlist_songs_scaled[-train_index, c("playlist_genre", feature_names)]
  
  # Train a decision tree model using the training set
  set.seed(1111)
  model_dt <- rpart(playlist_genre ~ ., data = train_set)
  
  # Plot the decision tree model
  rpart.plot(model_dt,
    type = 5,
    extra = 104,
    box.palette = list(
      orange = "#FB8B24",
      green = "#1DB954",
      skyblue = "#1DB9E2",
      lightpink = "#F8BBD0",
      lightgray = "#E0E0E0",
      lightpurple = "#D1C4E9"
    ),
    leaf.round = 0,
    fallen.leaves = FALSE,
    branch = 1,
    under = TRUE,
    family = "Montserrat",
    tweak = 1.2
  )
  
  # Perform predictions on the testing set using the trained model
  predict_dt <- predict(object = model_dt, newdata = test_set, type = "class")
  
  # Compare the true and predicted values
  compare_dt <- data.frame(
    true_value = test_set$playlist_genre,
    predicted_value = predict_dt,
    stringsAsFactors = FALSE
  )
  
  # Create a confusion matrix
  cm_dt <- table(compare_dt$true_value, compare_dt$predicted_value)
  
  # Calculate accuracy
  accuracy_dt <- sum(diag(cm_dt)) / sum(cm_dt)
  
  # Calculate precision, recall, and F1-score for each class
  precision_dt <- diag(cm_dt) / colSums(cm_dt)
  recall_dt <- diag(cm_dt) / rowSums(cm_dt)
  f1_score_dt <- 2 * ((precision_dt * recall_dt) / (precision_dt + recall_dt))
  
  # Create a data frame to store evaluation metrics
  metrics_dt <- data.frame(precision = precision_dt, recall = recall_dt, f1_score = f1_score_dt)
  row.names(metrics_dt) <- levels(as.factor(test_set$playlist_genre))
  
  # Store the results in a list
  results <- list(accuracy = accuracy_dt, metrics = metrics_dt)
  
  # Return the results
  return(results)
}
```

**Call the function**

Now call the function to train a decision tree model on the Spotify songs dataset. The function will return the accuracy and evaluation metrics.

```r
# Train a decision tree model on the Spotify songs dataset
results_dt <- classification_function_decision_tree(spotify_songs)
# Print the results
print(paste("Decision Tree Accuracy:", results_dt$accuracy))
print("Metrics:")
print(results_dt$metrics)
```

## Clustering

The following function will be used to perform clustering on the Spotify songs dataset. It will use the k-means algorithm to cluster the songs into different groups based on their features. And is called in the reccomend function.

```r
# Function: clustering_function
# Description: Performs clustering analysis on the Spotify songs dataset using k-means algorithm.
# Parameters:
#   - spotify_songs: Data frame containing the Spotify songs dataset.
#   - feature_names: Vector of feature names to be used for clustering.
# Returns:
#   - results: List of clustering results including optimal number of clusters, scaled data, cluster assignments, optimal features, and feature names.

clustering_function <- function(spotify_songs, feature_names) {

    # Subset the data to keep only the features of interest
    spotify_cluster <- spotify_songs[, feature_names]

    # Scaling the data
    spotify_cluster_scaled <- scale(spotify_cluster)

    # Feature selection using PCA
    pca_results <- prcomp(spotify_cluster_scaled, scale. = TRUE)
    plot(pca_results, type = "l")

    optimal_features <- feature_names[abs(pca_results$rotation[, 1]) > 0.2]
    optimal_features_scaled <- spotify_cluster_scaled[, optimal_features]
    optimal_features_scaled <- as.data.frame(optimal_features_scaled)

    optimal_features_scaled$PC1 <- pca_results$x[, 1]
    optimal_features_scaled$PC2 <- pca_results$x[, 2]


    # Extract loadings for the first two principal components
    loadings <- pca_results$rotation[, 1:2]

    # Display the loadings
    print(loadings)

    # Find the indices of the features with the highest absolute loadings
    max_loading_index_PC1 <- which.max(abs(loadings[, 1]))
    max_loading_index_PC2 <- which.max(abs(loadings[, 2]))

    # Get the corresponding feature names
    max_loading_feature_PC1 <- optimal_features[max_loading_index_PC1]
    max_loading_feature_PC2 <- optimal_features[max_loading_index_PC2]

    # Create a string with the feature names and their loadings
    loadings_text <- paste(
        "PC1 (", max_loading_feature_PC1, "): ", round(loadings[max_loading_index_PC1, 1], 3),
        ", PC2 (", max_loading_feature_PC2, "): ", round(loadings[max_loading_index_PC2, 2], 3),
        sep = ""
    )
    # Elbow method
    wss <- (nrow(optimal_features_scaled) - 1) * sum(apply(optimal_features_scaled, 2, var))
    for (i in 2:10) wss[i] <- sum(kmeans(optimal_features_scaled, centers = i)$tot.withinss)
    plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

    # Silhouette method
    sil_width <- c(NA)
    for (i in 2:10) {
        kmeans_fit <- kmeans(optimal_features_scaled, centers = i, nstart = 25)
        ss <- silhouette(kmeans_fit$cluster, dist(optimal_features_scaled))
        sil_width[i] <- mean(ss[, 3])
    }
    optimal_k <- which.max(sil_width)
    print(paste0("Optimal number of clusters based on silhouette method: ", optimal_k))

    # fviz_nbclust with the optimal number of clusters and the silhouette method
    silhouette_method <- fviz_nbclust(optimal_features_scaled, kmeans, method = "silhouette", nstart = optimal_k)
    print(silhouette_method)

    # Cluster data using optimal number of clusters
    set.seed(123)
    kmeans_model <- kmeans(optimal_features_scaled, centers = optimal_k, nstart = 25, iter.max = 50)

    # Print the cluster assignments
    print(kmeans_model)

    # Define the first two principal components
    PC1 <- pca_results$x[, 1]
    PC2 <- pca_results$x[, 2]

    # Create a data frame with the first two principal components and cluster assignments
    pca_data <- optimal_features_scaled %>% data.frame(PC1 = PC1, PC2 = PC2, cluster = as.factor(kmeans_model$cluster))

    # Create a scatter plot of the first two principal components
    pca_scatter_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(alpha = 0.6) +
        labs(
            x = "Principal Component 1",
            y = "Principal Component 2",
            color = "Cluster",
            title = "Scatter Plot of the First Two Principal Components"
        ) +
        theme(legend.position = "bottom")

    # Print the scatter plot
    print(pca_scatter_plot)

    # Convert the scaled matrix to a data frame
    optimal_features_scaled <- as.data.frame(optimal_features_scaled)

    # Add the cluster assignments to the data frame
    optimal_features_scaled$cluster <- as.factor(kmeans_model$cluster)

    # Create a scatter plot with clusters and density
    scatter_density <- ggplot(optimal_features_scaled, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(alpha = 0.6) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon") +
        theme_minimal() +
        labs(
            x = "Principal Component 1", y = "Principal Component 2", color = "Cluster", fill = "Density",
            title = paste("Scatter Plot with Clusters and Density (", loadings_text, ")", sep = "")
        ) +
        theme(legend.position = "bottom") 

    # Print the scatter plot with clusters and density
    print(scatter_density)

  
    # Print silhouette plot without title
    sil <- silhouette(kmeans_model$cluster, dist(optimal_features_scaled))

    # Create a silhouette plot
    sil_plot_X <- fviz_silhouette(sil) +
        ggtitle(NULL) +
        theme_bw(base_family = "Montserrat") +
        theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        ) +
        labs(y = "Cluster") +
        coord_flip()
        print(sil_plot_X)

    # Return the optimal number of clusters and the scaled data
    return(list(optimal_k = optimal_k, scaled_data = optimal_features_scaled, cluster = kmeans_model$cluster, optimal_features = optimal_features, feature_names = feature_names))
}
```



### Recommend Songs

**Model Evaluation function**

The following function is used later to evaluates the models using cross-validation on the provided dataset and optimal features.

```r
# Function: evaluate_models
# Description: Evaluates models using cross-validation on the provided dataset and optimal features.
# Parameters:
#   - data: Data frame containing the dataset.
#   - optimal_features: Vector of optimal feature names to be used for model evaluation.
# Returns:
#   - results: Data frame of model evaluation results.

evaluate_models <- function(data, optimal_features) {
  
  # Convert the playlist_genre column to a factor
  data$playlist_genre <- as.factor(data$playlist_genre)
  
  # Define the train control with 10-fold cross-validation and save predictions
  control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
  
  # Train the random forest model using cross-validation
  model_cv <- train(
    playlist_genre ~ .,
    data = data[, c(optimal_features, "playlist_genre")],
    method = "rf",
    trControl = control,
    tuneGrid = data.frame(mtry = floor(sqrt(length(optimal_features))))
  )
  
  # Return the model evaluation results
  return(model_cv$results)
}
```

**Recommend Songs function**

The follwing function recommends songs from dataset A based on the clustering results and dataset B. It performs the following steps:

```r
# Function: recommend_songs_cluster
# Description: Recommends songs from dataset A based on the clustering results and dataset B.
# Parameters:
#   - data_a: Data frame containing dataset A.
#   - data_b: Data frame containing dataset B.
#   - feature_names: Vector of feature names to be used for clustering and recommendation.
# Returns:
#   - recommended_songs: List of recommended songs from dataset A.
recommend_songs_cluster <- function(data_a, data_b, feature_names) {
    set.seed(123)
    data_a <- data_a %>%
        sample_n(5000)

    # Step 1: Cluster the songs in dataset A using the clustering_function()
    clustering_results <- clustering_function(data_a, feature_names)
    print("clustering worked")
    data_a_scaled <- clustering_results$scaled_data
    data_a$cluster <- clustering_results$cluster


    ## Run this only if you want to see the performance the clustering function
    # # Evaluate the performance of the random forest models using k-fold cross-validation
    # model_performance <- evaluate_models(data_a, clustering_results$optimal_features)
    # cat("Model performance on dataset A:\n")
    # print(model_performance)

    # Step 2: For each cluster, train a random forest model using the songs in that cluster
    cluster_models <- lapply(unique(data_a$cluster), function(cluster) {
        train_data_a <- data_a_scaled[data_a$cluster == cluster, clustering_results$optimal_features] 
        train_labels_a <- as.factor(data_a$playlist_genre[data_a$cluster == cluster])

        # Train the random forest model with the optimal number of trees
        randomForest(train_data_a, train_labels_a, importance = TRUE, ntree = 500)
    })

    # Scale data_b using the common feature names
    data_b_scaled <- scale(data_b[, feature_names])
    # Subset data_b_scaled to only include the optimal features
    data_b_scaled_optimal <- data_b_scaled[, clustering_results$optimal_features]

    # Print the feature names of the datasets
    cat("Features in the model:\n")
    print(clustering_results$optimal_features)
    cat("Features in data_b_scaled_optimal:\n")
    print(colnames(data_b_scaled_optimal))

    # Step 3: Predict the playlist genre for dataset B using the random forest models
    predicted_labels_b <- sapply(cluster_models, function(model) {
        predict(model, data_b_scaled_optimal)
    })
    # Get the predicted genres for dataset B
    predicted_genres_b <- do.call(rbind, lapply(predicted_labels_b, function(x) {
        data.frame(predicted_genre = x, stringsAsFactors = FALSE)
    }))

    # Add the predicted genres to dataset B
    users_top_50_with_predicted_genres <- cbind(users_top_50, predicted_genres_b)

    # Print dataset B with predicted genres
    View(users_top_50_with_predicted_genres)
    summary_table <- users_top_50_with_predicted_genres %>%
        group_by(predicted_genre) %>%
        summarize(n = n()) %>%
        arrange(desc(n))
    View(summary_table)
    print(summary_table)
    print("step 3 worked")
    # Step 4: For each song in dataset B, find the closest song in the corresponding cluster in dataset A
    recommended_songs <- lapply(seq_len(nrow(data_b)), function(i) {
        cluster_predictions <- predicted_labels_b[i, ] # Use the correct index order
        closest_cluster <- unique(data_a$cluster[data_a$playlist_genre %in% cluster_predictions])

        if (length(closest_cluster) > 1) {
            # If a song is predicted to belong to multiple clusters, choose the first one
            closest_cluster <- closest_cluster[1]
        }

        cluster_songs <- data_a[data_a$cluster == closest_cluster, ]
        cluster_songs_scaled <- data_a_scaled[data_a$cluster == closest_cluster, clustering_results$optimal_features] # Exclude PC1 and PC2

        distances <- apply(cluster_songs_scaled, 1, function(x) {
            sqrt(sum((x - data_b_scaled_optimal[i, ])^2))
        })

        closest_song <- cluster_songs[which.min(distances), ]
        closest_song
    })


    # Return the list of recommended songs
    return(recommended_songs)
}
``` 

**Run the recommendation function**

The following code runs the recommendation function and prints the recommended songs.

```r
feature_names <- c(
    "danceability", "energy", "key",  "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo", "mode"
)

# Run the recommendation function
recommended_songs_cluster <- recommend_songs_cluster(spotify_songs, users_top_50, feature_names)

# Convert the list of recommended songs to a data frame
top_song_recomendation <- do.call(rbind, lapply(recommended_songs_cluster, function(x) data.frame(t(sapply(x, unlist)), stringsAsFactors = FALSE)))

# select the first 10 songs
top_10_song_recomendation <- top_song_recomendation[1:10, ]

# print track_name and track_artist and track genre
print(top_10_song_recomendation[, c("track_name", "track_artist", "playlist_genre", "cluster")])

```