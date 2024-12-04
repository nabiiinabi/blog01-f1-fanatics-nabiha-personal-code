# Load packages
library(rvest)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(viridis)


# Defining the URL
url <- "https://en.wikipedia.org/wiki/List_of_Formula_One_Grands_Prix#By_race_title"

# Read the webpage
webpage <- read_html(url)

# Extract the target table as a data frame
race_table <- webpage |>
  html_element(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(23)") |>
  html_table()

# Extract race titles and race links
race_titles <- race_table[[1]]  
raw_links <- webpage |>
  html_element(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(39)") |>
  html_elements("th > a[href^='/wiki/']") |>
  html_attr("href") 

race_links <- paste0("https://en.wikipedia.org", raw_links)

# Combining race titles and race links
race_data <- tibble(
  RaceTitle = race_titles,
  Hyperlink = race_links, 
  CleanText = rep(NA, 54),
  TopWords = vector("list", 54)
)

for (i in 1:54) {
  
  # Extract raw text from the page
  text <- 
    race_data$Hyperlink[i] |>
    read_html() |>
    html_nodes("#mw-content-text > .mw-parser-output > p")|>
    html_text2()
  
  # Remove references and numbers
  clean_text <- str_replace_all(text, "\\[\\d+\\]", "")
  clean_text <- str_replace_all(clean_text, "\\d+", "")
  clean_text <- str_replace_all(clean_text, "\\b\\w*rac\\w*\\b", "") # Removing words containing race as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*gam\\w*\\b", "") # Removing words containing game as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*Prix\\w*\\b", "") # Removing words containing Prix as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*prix\\w*\\b", "")
  clean_text <- str_replace_all(clean_text, "\\b\\w*Grand\\w*\\b", "") # Removing words containing Grand as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*grand\\w*\\b", "")
  clean_text <- str_replace_all(clean_text, "\\b\\w*rac\\w*\\b", "") # Removing words containing race as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*gam\\w*\\b", "") # Removing words containing game as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*Prix\\w*\\b", "") # Removing words containing Prix as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*prix\\w*\\b", "")
  clean_text <- str_replace_all(clean_text, "\\b\\w*held\\w*\\b", "") # Removing words containing Grand as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*circuit\\w*\\b", "") # Removing words containing Grand as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*formula\\w*\\b", "") # Removing words containing formula as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*Formula\\w*\\b", "")
  clean_text <- str_replace_all(clean_text, "\\b\\w*championship\\w*\\b", "") # Removing words containing championship as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*Championship\\w*\\b", "") # Removing words containing championship as stem word
  clean_text <- str_replace_all(clean_text, "\\b\\w*event\\w*\\b", "") # Removing the word 'event'
  clean_text <- str_replace_all(clean_text, "\\b\\w*Event\\w*\\b", "")
  clean_text <- str_replace_all(clean_text, "\\b\\w*drive\\w*\\b", "") # Removing the word 'drive'
  
  # Combine the text into a single string and save it to the data frame
  race_data$CleanText[i] <- paste(clean_text, collapse = " ")
  
}
 
# Use the custom palette resembling F1 colors
f1_palette <- c('palevioletred1', 
                'orange', 
                'red1', 
                'steelblue1', 
                'navyblue')


# Extract the clean text for the current race
for (i in 1:54) {
  
  # Extract the clean text for the current race
  text_data <- data.frame(text = race_data$CleanText[i])
  
  # Tokenize the text into words
  tokenized_words <- text_data |>
    unnest_tokens(word, text)
  
  # Load and remove stop words
  data("stop_words")
  filtered_words <- tokenized_words |>
    anti_join(stop_words, by = "word")
  
  # Count word frequencies
  word_counts <- filtered_words |>
    count(word, sort = TRUE)
  
  # Select the top 30 most frequent words
  top_words <- word_counts |>
    slice_head(n = 30)
  
  # Assigning colors to each word according to custom palette
  top_words <- top_words |> 
    mutate(color_group = sample(f1_palette, size = nrow(top_words), replace = TRUE))
  
  # Save the top words as a list 
  race_data$TopWords[[i]] <- top_words
}

# Will delete this section later after adding it directly to the blog ==========


# Choosing which race to make a word cloud out of by changing the value of i

i<- 11
top_words = race_data$TopWords[[i]]
set.seed(53)
angles45 <- sample(45 * -2:2, nrow(top_words), replace = TRUE, prob = c(0.2, 0.1, 0.4, 0.1, 0.2))


# Create the word cloud
p <- ggplot(top_words, aes(label = word, size = n, color = color_group, angle = angles45)) +
  geom_text_wordcloud(seed = 53, shape = "triangle-upright", eccentricity = 0.8, family = "sans") +
  scale_size_area(max_size = 15) +
  scale_color_identity() +
  theme_minimal() +
  labs(title = paste("Word Cloud for", race_data$RaceTitle[i]))



# Saving the race data table
save(race_data, file = "data/race_data.RData")

