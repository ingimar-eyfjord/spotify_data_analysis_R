library(tidyverse) # for data manipulation
library(ggplot2) # for data visualization
library(corrplot) # for correlation visualization
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


## Theming
font_add_google("Montserrat")
showtext_auto()
theme_set(theme_ipsum(base_family = "Montserrat"))
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
    # Remove legend
    # legend.position = "none"
)
custom_color_gradient <- colorRampPalette(colors = c("darkorchid4", "white", "#1DB954"))

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

## Theming done

## Log user in

# # Load the .env file
load_dot_env(".env")

# # Access the environment variables
client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
redirect_uri <- Sys.getenv("SPOTIFY_REDIRECT_URI")
user_id <- Sys.getenv("SPOTIFY_USER")

access_token <- get_spotify_access_token()

# Replace this with the name of the artist you are looking for!


headers <- add_headers(Authorization = paste("Bearer", access_token))

user_profile_url <- paste0("https://api.spotify.com/v1/users/", user_id)
user_profile_response <- GET(user_profile_url, headers)

# Convert JSON response to a data frame
# user_profile_data <- fromJSON(content(user_profile_response, as = "text", encoding = "UTF-8"))
user_profile_data <- fromJSON(rawToChar(user_profile_response$content))
user_data <- list(
    display_name = user_profile_data$display_name,
    followers_total = user_profile_data$followers$total,
    id = user_profile_data$id
)

# Convert the list to a data frame
user_profile_df <- as.data.frame(user_data)

# Assuming 'response' is the response object you received


spotify_endpoint <- oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access = "https://accounts.spotify.com/api/token"
)

spotify_app <- oauth_app(
    "spotify",
    key = client_id,
    secret = client_secret,
    redirect_uri = "http://localhost:1410/"
)

spotify_scopes <- c(
    "user-read-email",
    "user-read-private",
    "playlist-read-private",
    "playlist-modify-public",
    "playlist-modify-private",
    "user-top-read"
)

token <- oauth2.0_token(
    endpoint = spotify_endpoint,
    app = spotify_app,
    scope = spotify_scopes,
    type = "code",
    cache = FALSE,
)

profile_url <- "https://api.spotify.com/v1/me"
# access_token <- token$credentials
credentials_raw <- token$credentials
credentials_char <- rawToChar(credentials_raw)
credentials_list <- fromJSON(credentials_char)


access_token <- credentials_list$access_token
auth_header <- paste("Bearer", access_token)

response <- GET(profile_url, add_headers(Authorization = auth_header))
# Parse the JSON response and convert it to a data frame


profile_list <- fromJSON(rawToChar(response$content))
profile_df <- list(
    display_name = profile_list$display_name,
    followers_total = profile_list$followers$total,
    id = profile_list$id
)


get_top_items <- function(access_token, type, time_range = "medium_term", limit = 100, offset = 0) {
    base_url <- "https://api.spotify.com/v1/me/top"
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

# top_artists <- get_top_items(access_token, "artists", time_range = "medium_term", limit = 10, offset = 0)

# artists_df <- data.frame(id = sapply(top_artists$items, function(x) x$id),
#                          name = sapply(top_artists$items, function(x) x$name),
#                          popularity = sapply(top_artists$items, function(x) x$popularity),
#                          followers = sapply(top_artists$items, function(x) x$followers$total),
#                          genres = I(sapply(top_artists$items, function(x) paste(x$genres, collapse = ", "))),
#                          stringsAsFactors = FALSE)

top_tracks <- get_top_items(access_token, "tracks", time_range = "medium_term", limit = 100, offset = 0)
View(top_tracks$items)


items_list <- top_tracks$items

tracks_df <- items_list %>%
    select(-c(album, available_markets, disc_number, explicit, external_ids, external_urls, href, is_local, preview_url, track_number, type, uri))





get_audio_features <- function(access_token, track_id) {
    base_url <- "https://api.spotify.com/v1/audio-features"
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
# audio_features_list <- sapply(tracks_df$id, get_audio_features, access_token)
audio_features_list <- sapply(tracks_df$id, function(track_id) get_audio_features(access_token, track_id))
audio_features_df <- t(audio_features_list)

# delete first row of audio_features_df

merged_df <- merge(tracks_df, audio_features_df, by = "id")


# # Function to fetch genres for a vector of artist IDs
# get_genres_for_artists <- function(access_token, artist_ids) {
#     base_url <- "https://api.spotify.com/v1/artists?ids="
#     url <- paste0(base_url, paste(artist_ids, collapse = ","))

#     auth_header <- paste("Bearer", access_token)

#     response <- GET(url, add_headers(Authorization = auth_header))

#     if (response$status_code != 200) {
#         warning("Error in API request:")
#         print(response)
#         return(NULL)
#     }

#     # artist_info <- content(response, as = "parsed", encoding = "UTF-8")
#     artist_info <- fromJSON(rawToChar(response$content))

#     artist_genres <- lapply(artist_info$artists, function(x) x$genres)
#     unique_genres <- unique(unlist(artist_genres))
#     return(unique_genres)
# }

# # Split the artist_ids column into a list of vectors
# artist_id_list <- strsplit(merged_df$artist_ids, ", ")

# # Fetch genres for each track
# track_genres <- lapply(artist_id_list, function(ids) get_genres_for_artists(access_token, ids))

# # Add genres to the dataframe
# merged_df$genres <- track_genres

merged_df <- merged_df %>% rename(track_popularity = popularity)
merged_df <- merged_df %>% rename(duration_ms = duration_ms.y)
merged_df <- merged_df %>% rename(track_id = id)
merged_df <- merged_df %>% rename(track_name = name)
merged_df <- merged_df %>% rename(track_artist = artists)


column_index <- which(names(merged_df) == "duration_ms.x")
merged_df <- merged_df[, -column_index]

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
# merged_df <- merged_df %>% rename(duration_ms = duration_ms.y)
users_top_50 <- merged_df
caption <- "Mock, T. (2022). Tidy tuesday: A weekly data project aimed at the r ecosystem. <br> github.com/rfordatascience/tidytuesday"

get_tidytuesday_songs <- function() {
    # Get the Data
    spotify_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")

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
# Select highest and lowest date
dates <- spotify_songs %>%
    group_by(playlist_genre) %>%
    summarise(min_date = min(track_album_release_date), max_date = max(track_album_release_date)) %>%
    arrange(desc(max_date))
feature_names <- c(
    "danceability", "energy", "key", "loudness", "mode", "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo",
    "duration_ms", "duration_sec"
)
eda_function <- function(spotify_songs) {


    # create a list of the names to use for the audio features

    # If playlist_genre exists, means it's in Dataset A, Dataset B does not have a playlists_genre column
    # Create a new column called playlist_genre and assign it "-" values for all rows if it doesn't exist
    if (!"playlist_genre" %in% names(spotify_songs)) {
        spotify_songs <- spotify_songs %>%
            mutate(playlist_genre = "-")
    }


    with_outliers <- spotify_songs %>%
        ggplot(aes(y = duration_ms)) +
        geom_boxplot(color = kp_cols("red"), coef = 4) +
        coord_flip() +
        labs(title = "Duration")

    duration_outliers <- boxplot(spotify_songs$duration_ms,
        plot = FALSE, range = 4
    )$out



    spotify_songs_no_outliers <- spotify_songs %>%
        filter(!duration_ms %in% duration_outliers)

    without_outliers <- spotify_songs_no_outliers %>%
        ggplot(aes(y = duration_ms)) +
        geom_boxplot(color = kp_cols("red"), coef = 4) +
        coord_flip() +
        labs(title = "Duration, outliers removed")

    gridExtra::grid.arrange(with_outliers, without_outliers, ncol = 1)


    # amount of songs
    nrow(spotify_songs)

    # genres
    spotify_songs %>%
        group_by(playlist_genre) %>%
        summarise(n = n()) %>%
        arrange(desc(n))
    View(spotify_songs)

    spotify_songs <- spotify_songs %>%
        mutate(
            duration_sec = duration_ms / 1000,
            duration_min = duration_sec / 60
        )


    audio_feature_density <- spotify_songs %>%
        select(c("playlist_genre", feature_names)) %>%
        pivot_longer(cols = feature_names) %>%
        filter(name != "duration_ms") %>% # remove the duration_ms column
        ggplot(aes(x = value)) +
        geom_density(aes(color = playlist_genre), alpha = 0.5) +
        facet_wrap(~name, ncol = 3, scales = "free") +
        labs(
            title = "",
            x = "", y = "density"
        ) +
        theme(axis.text.y = element_blank())

    # Modify scales for duration_ms feature
    if ("duration_ms" %in% feature_names) {
        audio_feature_density <- audio_feature_density + scale_x_continuous(labels = comma_format())
    }

    print(audio_feature_density)

    summary(spotify_songs)
    ## Check the structure of the data
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
    # +
    # geom_density(alpha = 1, fill = "darkorchid4") +
    # labs(title = "Danceability", x = "", y = "", subtitle = "Histogram of showing the distribution of danceability", caption = "<strong>y-axis:</strong> Frequency of Danceability. <br> <strong>x-axis:</strong> Danceability.")

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
        # labs(title = "Correlation Plot",
        # subtitle = "Correlation heatmap plot between continuous variables")+
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

    print(correlation_plot_2)
}
eda_function(spotify_songs)
eda_function(users_top_50)


closely_related_genres <- function(spotify_songs) {


    # Create new column for year based on release date which is YYYY-MM-DD so get ony the first 4 numbers
    spotify_songs <- spotify_songs %>%
        mutate(year = substr(track_album_release_date, 1, 4))

    # songs per genre
    spotify_songs %>%
        group_by(Genre = playlist_genre) %>%
        summarise(No_of_tracks = n()) %>%
        knitr::kable()

    # artists with most releases
    most_releases <- spotify_songs %>%
        group_by(Artist = track_artist) %>%
        summarise(No_of_tracks = n()) %>%
        arrange(desc(No_of_tracks)) %>%
        top_n(15, wt = No_of_tracks) %>%
        ggplot(aes(x = Artist, y = No_of_tracks)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "artists with the most releases", x = "artists", y = "no of releases")

    ggplotly(most_releases)


    # Create a vector containing only the text
    text <- spotify_songs$track_name
    # Create a corpus
    docs <- Corpus(VectorSource(text))

    # clean text data
    docs <- docs %>%
        tm_map(removeNumbers) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removeWords, c("feat", "edit", "remix", "remastered", "remaster", "radio", "version", "original", "mix"))

    # create a doument-term matrix

    dtm <- TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix), decreasing = TRUE)
    df <- data.frame(word = names(words), freq = words)

    # generate the word cloud
    wordcloud(
        words = df$word, freq = df$freq, scale = c(8, 0.25), min.freq = 1,
        max.words = 150, random.order = FALSE, rot.per = 0.25,
        colors = brewer.pal(8, "Dark2")
    )


    # grouping tracks by years

    plot_year <- spotify_songs %>%
        select(year) %>%
        group_by(year) %>%
        summarise(count = n())

    # plotting releases across years

    year_plot <- ggplot(plot_year, aes(x = year, y = count, group = 1)) +
        geom_line() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
            title = "Release of songs across years", x = "Year",
            y = "No of songs released"
        )

    ggplotly(year_plot)



    # zooming into 21st century

    plot_zoom_year <- spotify_songs %>%
        select(year) %>%
        group_by(year) %>%
        summarise(count = n()) %>%
        subset(year >= 2000)


    graph_zoom_year <- ggplot(plot_zoom_year, aes(x = year, y = count, group = 1)) +
        geom_line() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
            title = "Music in 21st Century", x = "Year",
            y = "No of songs released"
        )

    ggplotly(graph_zoom_year)

    # create a new column for duration minutes called drun_minutes from duration_ms
    spotify_songs <- spotify_songs %>%
        mutate(durn_minutes = duration_ms / 60000)

    genre_description <- spotify_songs %>%
        group_by(Genre = playlist_genre) %>%
        summarise(
            Danceability = median(danceability),
            Energy = median(energy),
            Key = median(key),
            Loudness = median(loudness),
            Mode = median(mode),
            Speechiness = median(speechiness),
            Acousticness = median(acousticness),
            Instrumentalness = median(instrumentalness),
            Liveness = median(liveness),
            Valence = median(valence),
            Tempo = median(tempo),
            Duration = median(durn_minutes)
        )

    kable(genre_description, format = "html") %>%
        kable_styling(bootstrap_options = "striped") %>%
        column_spec(2, width = "12em")



    names <- c(
        "danceability", "energy", "key", "loudness", "mode", "speechiness",
        "acousticness", "instrumentalness", "liveness", "valence", "tempo",
        "duration_ms", "durn_minutes"
    )

    # average features by genre
    avg_genre_matrix <- spotify_songs %>%
        group_by(playlist_genre) %>%
        summarise_if(is.numeric, median, na.rm = TRUE) %>%
        ungroup()

    # converting to matrix
    avg_genre_cor <- avg_genre_matrix %>%
        select(names, -mode) %>%
        scale() %>%
        t() %>%
        as.matrix() %>%
        cor()

    colnames(avg_genre_cor) <- avg_genre_matrix$playlist_genre
    row.names(avg_genre_cor) <- avg_genre_matrix$playlist_genre



    correlation_plot_2 <- corrplot(
        avg_genre_cor,
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
    print(correlation_plot_2)

    ggcorrplot_object <- ggcorrplot(
        avg_genre_cor,
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
        # labs(title = "Correlation Plot",
        # subtitle = "Correlation heatmap plot between continuous variables")+
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
}
closely_related_genres(spotify_songs)


classification_function_decision_tree <- function(spotify_songs) {
    feature_names <- c(
        "danceability", "energy", "key", "mode", "speechiness",
        "acousticness", "instrumentalness", "liveness", "valence", "tempo",
        "duration_ms"
    )

    playlist_songs_scaled <- spotify_songs %>%
        mutate_if(is.numeric, scale)

    set.seed(19234)

    # Use createDataPartition for stratified sampling
    train_index <- createDataPartition(y = spotify_songs$playlist_genre, p = 0.8, list = FALSE)

    train_set <- playlist_songs_scaled[train_index, c("playlist_genre", feature_names)]
    test_set <- playlist_songs_scaled[-train_index, c("playlist_genre", feature_names)]

    set.seed(1111)
    model_dt <- rpart(playlist_genre ~ ., data = train_set)

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
    predict_dt <- predict(object = model_dt, newdata = test_set, type = "class")

    compare_dt <- data.frame(
        true_value = test_set$playlist_genre,
        predicted_value = predict_dt,
        stringsAsFactors = FALSE
    )

    cm_dt <- table(compare_dt$true_value, compare_dt$predicted_value)

    accuracy_dt <- sum(diag(cm_dt)) / sum(cm_dt)

    precision_dt <- diag(cm_dt) / colSums(cm_dt)
    recall_dt <- diag(cm_dt) / rowSums(cm_dt)
    f1_score_dt <- 2 * ((precision_dt * recall_dt) / (precision_dt + recall_dt))

    metrics_dt <- data.frame(precision = precision_dt, recall = recall_dt, f1_score = f1_score_dt)
    row.names(metrics_dt) <- levels(as.factor(test_set$playlist_genre))

    results <- list(accuracy = accuracy_dt, metrics = metrics_dt)
    return(results)
}
results_dt <- classification_function_decision_tree(spotify_songs)
print(paste("Decision Tree Accuracy:", results_dt$accuracy))
print("Metrics:")
print(results_dt$metrics)


spotify_songs_numeric <- spotify_songs[sapply(spotify_songs, is.numeric)]

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

    print(kmeans_model)


    PC1 <- pca_results$x[, 1]
    PC2 <- pca_results$x[, 2]

    pca_data <- optimal_features_scaled %>% data.frame(PC1 = PC1, PC2 = PC2, cluster = as.factor(kmeans_model$cluster))

    pca_scatter_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(alpha = 0.6) +
        labs(
            x = "Principal Component 1",
            y = "Principal Component 2",
            color = "Cluster",
            title = "Scatter Plot of the First Two Principal Components"
        ) +
        theme(legend.position = "bottom")

    print(pca_scatter_plot)

    # Convert the scaled matrix to a data frame
    optimal_features_scaled <- as.data.frame(optimal_features_scaled)

    # Add the cluster assignments to the data frame
    optimal_features_scaled$cluster <- as.factor(kmeans_model$cluster)


    scatter_density <- ggplot(optimal_features_scaled, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(alpha = 0.6) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon") +
        theme_minimal() +
        labs(
            x = "Principal Component 1", y = "Principal Component 2", color = "Cluster", fill = "Density",
            title = paste("Scatter Plot with Clusters and Density (", loadings_text, ")", sep = "")
        ) +
        theme(legend.position = "bottom") 


    print(scatter_density)

  
    # Print silhouette plot without title
    sil <- silhouette(kmeans_model$cluster, dist(optimal_features_scaled))

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


    # Print silhouette plot without title and with custom colors

   
    # Return the optimal number of clusters and the scaled data
    return(list(optimal_k = optimal_k, scaled_data = optimal_features_scaled, cluster = kmeans_model$cluster, optimal_features = optimal_features, feature_names = feature_names))
}

# Add the new function to perform k-fold cross-validation
evaluate_models <- function(data, optimal_features) {
    data$playlist_genre <- as.factor(data$playlist_genre)

    control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

    model_cv <- train(playlist_genre ~ .,
        data = data[, c(optimal_features, "playlist_genre")],
        method = "rf",
        trControl = control,
        tuneGrid = data.frame(mtry = floor(sqrt(length(optimal_features))))
    )

    return(model_cv$results)
}

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
feature_names <- c(
    "danceability", "energy", "key",  "speechiness",
    "acousticness", "instrumentalness", "liveness", "valence", "tempo", "mode"
)
recommended_songs_cluster <- recommend_songs_cluster(spotify_songs, users_top_50, feature_names)

top_song_recomendation <- do.call(rbind, lapply(recommended_songs_cluster, function(x) data.frame(t(sapply(x, unlist)), stringsAsFactors = FALSE)))

# select the first 10 songs
top_10_song_recomendation <- top_song_recomendation[1:10, ]

# print track_name and track_artist and track genre
print(top_10_song_recomendation[, c("track_name", "track_artist", "playlist_genre", "cluster")])



### I did not have time to complete the next iteration of this analysis
# ====== 2. Clustering with more principal component ====== #


# spotify_songs_numeric <- spotify_songs[sapply(spotify_songs, is.numeric)]

# clustering_function <- function(spotify_songs, feature_names) {


#     # Subset the data to keep only the features of interest
#     spotify_cluster <- spotify_songs[, feature_names]

#     # Scaling the data
#     spotify_cluster_scaled <- scale(spotify_cluster)

#     # Feature selection using PCA
#     pca_results <- prcomp(spotify_cluster_scaled, scale. = TRUE)

#     cumulative_variance <- cumsum(pca_results$sdev^2) / sum(pca_results$sdev^2)
#     optimal_n <- which(cumulative_variance >= 0.4)[1] # 80% threshol
#     pca_data <- data.frame(pca_results$x[, 1:optimal_n])

#     plot(pca_results, type = "l")
    
#     optimal_features <- feature_names[abs(pca_results$rotation[, 1]) > 0.2]
#     optimal_features_scaled <- spotify_cluster_scaled[, optimal_features]
#     optimal_features_scaled <- as.data.frame(optimal_features_scaled)

#     optimal_features_scaled$PC1 <- pca_results$x[, 1]
#     optimal_features_scaled$PC2 <- pca_results$x[, 2]

# optimal_features_scaled <- pca_data
#     # Extract loadings for the first two principal components
#     loadings <- pca_results$rotation[, 1:2]

#     # Display the loadings
#     print(loadings)

#     # Find the indices of the features with the highest absolute loadings
#     max_loading_index_PC1 <- which.max(abs(loadings[, 1]))
#     max_loading_index_PC2 <- which.max(abs(loadings[, 2]))

#     # Get the corresponding feature names
#     max_loading_feature_PC1 <- optimal_features[max_loading_index_PC1]
#     max_loading_feature_PC2 <- optimal_features[max_loading_index_PC2]

#     # Create a string with the feature names and their loadings
#     loadings_text <- paste(
#         "PC1 (", max_loading_feature_PC1, "): ", round(loadings[max_loading_index_PC1, 1], 3),
#         ", PC2 (", max_loading_feature_PC2, "): ", round(loadings[max_loading_index_PC2, 2], 3),
#         sep = ""
#     )
#     # Elbow method
#     wss <- (nrow(optimal_features_scaled) - 1) * sum(apply(optimal_features_scaled, 2, var))
#     for (i in 2:10) wss[i] <- sum(kmeans(optimal_features_scaled, centers = i)$tot.withinss)
#     plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

#     # Silhouette method
#     sil_width <- c(NA)
#     for (i in 2:10) {
#         kmeans_fit <- kmeans(optimal_features_scaled, centers = i, nstart = 25)
#         ss <- silhouette(kmeans_fit$cluster, dist(optimal_features_scaled))
#         sil_width[i] <- mean(ss[, 3])
#     }
#     optimal_k <- which.max(sil_width)
#     print(paste0("Optimal number of clusters based on silhouette method: ", optimal_k))

#     # fviz_nbclust with the optimal number of clusters and the silhouette method
#     silhouette_method <- fviz_nbclust(optimal_features_scaled, kmeans, method = "silhouette", nstart = optimal_k)
#     print(silhouette_method)

#     # Cluster data using optimal number of clusters
#     set.seed(123)
#     kmeans_model <- kmeans(optimal_features_scaled, centers = optimal_k, nstart = 25, iter.max = 50)

#     print(kmeans_model)


#     PC1 <- pca_results$x[, 1]
#     PC2 <- pca_results$x[, 2]

#     pca_data <- optimal_features_scaled %>% data.frame(PC1 = PC1, PC2 = PC2, cluster = as.factor(kmeans_model$cluster))

#     pca_scatter_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
#         geom_point(alpha = 0.6) +
#         labs(
#             x = "Principal Component 1",
#             y = "Principal Component 2",
#             color = "Cluster",
#             title = "Scatter Plot of the First Two Principal Components"
#         ) +
#         theme(legend.position = "bottom")

#     print(pca_scatter_plot)

#     # Convert the scaled matrix to a data frame
#     optimal_features_scaled <- as.data.frame(optimal_features_scaled)

#     # Add the cluster assignments to the data frame
#     optimal_features_scaled$cluster <- as.factor(kmeans_model$cluster)


#     scatter_density <- ggplot(optimal_features_scaled, aes(x = PC1, y = PC2, color = cluster)) +
#         geom_point(alpha = 0.6) +
#         stat_density_2d(aes(fill = ..level..), geom = "polygon") +
#         theme_minimal() +
#         labs(
#             x = "Principal Component 1", y = "Principal Component 2", color = "Cluster", fill = "Density",
#             title = paste("Scatter Plot with Clusters and Density (", loadings_text, ")", sep = "")
#         ) +
#         theme(legend.position = "bottom")


#     print(scatter_density)

#     # Print silhouette plot
#     sil <- silhouette(kmeans_model$cluster, dist(optimal_features_scaled))
#     sil_plot_title <- fviz_silhouette(sil)
#     print(sil_plot_title)

#     # Print silhouette plot without title
#     sil <- silhouette(kmeans_model$cluster, dist(optimal_features_scaled))
#     sil_plot_X <- fviz_silhouette(sil) + ggtitle(NULL)
#     print(sil_plot_X)

#     # Return the optimal number of clusters and the scaled data
#     return(list(optimal_k = optimal_k, scaled_data = optimal_features_scaled, cluster = kmeans_model$cluster, optimal_features = optimal_features, feature_names = feature_names))
# }
# # Add the new function to perform k-fold cross-validation
# evaluate_models <- function(data, optimal_features) {
#     data$playlist_genre <- as.factor(data$playlist_genre)

#     control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

#     model_cv <- train(playlist_genre ~ .,
#         data = data[, c(optimal_features, "playlist_genre")],
#         method = "rf",
#         trControl = control,
#         tuneGrid = data.frame(mtry = floor(sqrt(length(optimal_features))))
#     )

#     return(model_cv$results)
# }

# recommend_songs_cluster <- function(data_a, data_b, feature_names) {
#     set.seed(123)
#     data_a <- data_a %>%
#         sample_n(5000)

#     # Step 1: Cluster the songs in dataset A using the clustering_function()
#     clustering_results <- clustering_function(data_a, feature_names)

#     data_a_scaled <- clustering_results$scaled_data
#     data_a$cluster <- clustering_results$cluster

#     # Evaluate the performance of the random forest models using k-fold cross-validation
#     model_performance <- evaluate_models(data_a, clustering_results$optimal_features)
#     cat("Model performance on dataset A:\n")
#     print(model_performance)

#     # Step 2: For each cluster, train a random forest model using the songs in that cluster
#     cluster_models <- lapply(unique(data_a$cluster), function(cluster) {
#         train_data_a <- data_a_scaled[data_a$cluster == cluster, clustering_results$optimal_features] # Remove PC1 and PC2
#         train_labels_a <- as.factor(data_a$playlist_genre[data_a$cluster == cluster])

#         # Train the random forest model with the optimal number of trees
#         randomForest(train_data_a, train_labels_a, importance = TRUE, ntree = 500)
#     })
#     # Scale data_b using the common feature names
#     data_b_scaled <- scale(data_b[, feature_names])
#     # Subset data_b_scaled to only include the optimal features
#     data_b_scaled_optimal <- data_b_scaled[, clustering_results$optimal_features]

#     # Print the feature names of the datasets
#     cat("Features in the model:\n")
#     print(clustering_results$optimal_features)
#     cat("Features in data_b_scaled_optimal:\n")
#     print(colnames(data_b_scaled_optimal))

#     # Step 3: Predict the playlist genre for dataset B using the random forest models
#     predicted_labels_b <- sapply(cluster_models, function(model) {
#         predict(model, data_b_scaled_optimal)
#     })
#     print("step 3 worked")
#     # Step 4: For each song in dataset B, find the closest song in the corresponding cluster in dataset A
#     recommended_songs <- lapply(seq_len(nrow(data_b)), function(i) {
#         cluster_predictions <- predicted_labels_b[i, ] # Use the correct index order
#         closest_cluster <- unique(data_a$cluster[data_a$playlist_genre %in% cluster_predictions])

#         if (length(closest_cluster) > 1) {
#             # If a song is predicted to belong to multiple clusters, choose the first one
#             closest_cluster <- closest_cluster[1]
#         }

#         cluster_songs <- data_a[data_a$cluster == closest_cluster, ]
#         cluster_songs_scaled <- data_a_scaled[data_a$cluster == closest_cluster, clustering_results$optimal_features] # Exclude PC1 and PC2

#         distances <- apply(cluster_songs_scaled, 1, function(x) {
#             sqrt(sum((x - data_b_scaled_optimal[i, ])^2))
#         })

#         closest_song <- cluster_songs[which.min(distances), ]
#         closest_song
#     })


#     # Return the list of recommended songs
#     return(recommended_songs)
# }
# feature_names <- c(
#     "danceability", "energy", "key",  "speechiness",
#     "acousticness", "instrumentalness", "liveness", "valence", "tempo", "mode"
# )
# recommended_songs_cluster <- recommend_songs_cluster(spotify_songs, users_top_50, feature_names)

# top_song_recomendation <- do.call(rbind, lapply(recommended_songs_cluster, function(x) data.frame(t(sapply(x, unlist)), stringsAsFactors = FALSE)))
