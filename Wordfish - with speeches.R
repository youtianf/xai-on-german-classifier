install.packages(c("quanteda", "quanteda.textmodels", "quanteda.textplots"))
install.packages("manifestoR")
install.packages("dplyr")
install.packages("ggplot2")

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(dplyr)
library(manifestoR)
library(ggplot2)

# Get manifesto data
mp_setapikey("manifesto_apikey.txt")
party_mapping <- setNames(c(1, 2, 3, 4, 5, 6),
                          c(41953, 41113, 41521, 41420, 41223, 41320))
                          # AfD, B90Grune, CDU_CSU, FDP, Linke, SPD
manifesto_data <- mp_corpus(
  countryname == "Germany" & date %in% c(201309, 201709, 202109),
  as_tibble = TRUE
) %>%
  mutate(party_number = party_mapping[as.character(party)],
         party_name = case_when(
           party_number == 1 ~ "AfD",
           party_number == 2 ~ "Greens",
           party_number == 3 ~ "CDU_CSU",
           party_number == 4 ~ "FDP",
           party_number == 5 ~ "Left",
           party_number == 6 ~ "SPD"
         )) %>%
  group_by(party_number, party_name) %>%
  summarise(text = paste(text, collapse = " "), .groups = "drop") %>%
  filter(party_number %in% c(1, 2, 3, 4, 5, 6)) %>%
  arrange(party_number)

# Get speech data
text_files_path <- "PATH"
read_text_file <- function(party_name, path) {
  file_path <- file.path(path, paste0(party_name, ".txt"))
  if(file.exists(file_path)) {
    text <- readLines(file_path, encoding = "UTF-8", warn = FALSE)
    return(paste(text, collapse = " "))
  } else {
    warning(paste("File not found:", file_path))
    return("")
  }
}

# Add speech data to manifesto data
manifesto_data$additional_text <- sapply(manifesto_data$party_name, 
                                         read_text_file, 
                                         path = text_files_path)

manifesto_data$text <- paste(manifesto_data$text, 
                             manifesto_data$additional_text, 
                             sep = " ")


cat("===== SUMMARY =====\n")
manifesto_data %>%
  mutate(api_chars = nchar(text) - nchar(additional_text),
         file_chars = nchar(additional_text)) %>%
  select(party_name, api_chars, file_chars) %>%
  print()

# Create a corpus object for quanteda
corp_ger <- corpus(manifesto_data$text)
docnames(corp_ger) <- manifesto_data$party_name

# Adds party numbers as document variables
docvars(corp_ger, "party_number") <- manifesto_data$party_number

# Check
summary(corp_ger)

# Tokenize and build dfm
toks_ger <- tokens(corp_ger,
                   remove_punct = TRUE,
                   remove_numbers = TRUE,
                   remove_symbols = TRUE) %>%
  tokens_remove(stopwords("german"))%>%
  tokens_select(min_nchar = 3)  # Remove very short words

dfmat_ger <- dfm(toks_ger) %>%
  dfm_trim(min_termfreq = 5)

# check
cat("Documents:", ndoc(dfmat_ger), "\n")
cat("Features:", nfeat(dfmat_ger), "\n")
#cat("\nTop 20 most frequent terms:\n")
#print(topfeatures(dfmat_ger, 20))

# Fit Wordfish model
wf_ger <- textmodel_wordfish(dfmat_ger,
                             dir = c(5, 1),
                             dispersion = "poisson")

# check
summary(wf_ger)

# Plot document positions
textplot_scale1d(wf_ger,
                 groups = docvars(dfmat_ger, "party_number"))

beta <- wf_ger$beta  # each word's position on left-right scale
psi  <- wf_ger$psi   # how common is a word overall
zeta <- beta * sqrt(exp(psi)) # how much a word helps distinguish left from right

names(zeta) <- featnames(dfmat_ger)
names(beta) <- featnames(dfmat_ger)
names(psi) <- featnames(dfmat_ger)


# word frequency
word_freq <- colSums(dfmat_ger)

# Create a dataframe with ALL words
df_all_words <- data.frame(
  word = names(beta),
  position_score = beta,  # negative = left, positive = right
  psi = psi,
  zeta = zeta,
  abs_zeta = abs(zeta),
  political_leaning = ifelse(beta > 0, "right", "left"),
  frequency = word_freq[names(beta)],
  stringsAsFactors = FALSE
)

# Sort by position score
df_all_words <- df_all_words %>%
  arrange(position_score)

# Add ranks
df_all_words$position_rank <- 1:nrow(df_all_words)
df_all_words$discrimination_rank <- rank(-abs(df_all_words$zeta))

# Output
top_n <- 50
top_discriminating_indices <- order(abs(df_all_words$zeta), decreasing = TRUE)[1:top_n]
df_top <- df_all_words[top_discriminating_indices,]

cat("Total words in dataset:", nrow(df_all_words), "\n\n")

cat("  Most left word:", df_all_words$word[1], "(score:", round(df_all_words$position_score[1], 4), ")\n")
cat("  Most right word:", df_all_words$word[nrow(df_all_words)], "(score:", round(df_all_words$position_score[nrow(df_all_words)], 4), ")\n")

cat("Top 10 most left words:\n")
print(df_all_words[1:10, c("word", "position_score", "frequency", "abs_zeta")])

cat("\n\nTop 10 most right words:\n")
print(df_all_words[(nrow(df_all_words)-9):nrow(df_all_words), c("word", "position_score", "frequency", "abs_zeta")])

cat("\n\nTop 10 most discriminating words:\n")
print(df_top[1:10, c("word", "position_score", "abs_zeta", "political_leaning", "frequency")])

cat("\n\nNeutral words:\n")
neutral_indices <- which(abs(df_all_words$position_score) < quantile(abs(df_all_words$position_score), 0.1))
print(df_all_words[head(neutral_indices, 10), c("word", "position_score", "frequency")])

cat("\n\nBalance in the dataset:\n")
print(table(df_all_words$political_leaning))
cat("Percentage left:", round(100 * sum(df_all_words$political_leaning == "left") / nrow(df_all_words), 2), "%\n")
cat("Percentage right:", round(100 * sum(df_all_words$political_leaning == "right") / nrow(df_all_words), 2), "%\n")

# Save
write.csv(df_all_words, "E:\\Thesis Project\\word_political_scores.csv", row.names = FALSE)

df_simple <- df_all_words[, c("word", "position_score")]
write.csv(df_simple, "E:\\Thesis Project\\word_political_scores_simple.csv", row.names = FALSE)

# Save R objects
saveRDS(wf_ger, "wordfish_model.rds")
saveRDS(corp_ger, "german_corpus.rds")
saveRDS(dfmat_ger, "german_dfm.rds")
saveRDS(df_all_words, "words_dataframe.rds")

