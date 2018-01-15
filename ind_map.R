# Test to create a plot for aspects of independence

# Set working directory
setwd("~/PhD_Main/PhD_Analysis/4_1")

library(tm)
library(tidyverse)
library(RColorBrewer)
library(viridis)


# Provide a path to the corpus files
dname <- file.path("aeo_corpus") 
aeo_corp <- dir(dname) 

# Comparison Corpus
dname2 <- file.path("19c_corpus")
c19 <- dir(dname2)

# Create Volatile Corpora
docs <- VCorpus(DirSource(dname))
c19d <- VCorpus(DirSource(dname2))

# Preprocess corpora

docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace  
docs <- tm_map(docs, PlainTextDocument)


c19d <- tm_map(c19d, removePunctuation)   # Remove punctuation   
c19d <- tm_map(c19d, removeNumbers)      # Remove numbers    
c19d <- tm_map(c19d, tolower)   # Convert to lowercase   
c19d <- tm_map(c19d, stripWhitespace)   # Strip whitespace  
c19d <- tm_map(c19d, PlainTextDocument)

# Create a Document Term Matrix 
dtm <- DocumentTermMatrix(docs)

rownames(dtm) <- gsub("\\.txt", "", aeo_corp) # Allocates text names to rows

dtm2 <- DocumentTermMatrix(c19d)

rownames(dtm2) <- gsub("\\.txt", "", c19) # Allocates text names to rows


# Convert DTM to matrix
aeo_corp_m <- as.matrix(dtm)
doc_tokens <- rowSums(aeo_corp_m)

c19_m <- as.matrix(dtm2)
doc_tokens2 <- rowSums(c19_m)

corp_rel <- 100 * (aeo_corp_m / doc_tokens)
corp_rep_df <- as.data.frame(corp_rel)

c19_rel <- 100 * (c19_m / doc_tokens2)
c19_rep_df <- as.data.frame(c19_rel)

# Select terms from ind-lexicon
lex <- read.csv("~/PhD_Main/PhD_Analysis/Data/lexicon_unique.csv")
lex <- lex[, -1]

wealth_w <- as.character(lex[,1])
marriage_w <- as.character(lex[,2])
self_w <- as.character(lex[,3])
nation_w <- as.character(lex[,4])

# Create sub-theme results
corp_marriage <- subset.data.frame(corp_rep_df, select = marriage_w)
corp_marriage$Text <- rownames(corp_marriage)
corp_marriage$Theme <- rep("Marriage", times = nrow(corp_marriage))

corp_self <- subset.data.frame(corp_rep_df, select = self_w)
corp_self$Text <- rownames(corp_self)
corp_self$Theme <- rep("Self", times = nrow(corp_self))

corp_nation <- subset.data.frame(corp_rep_df, select = nation_w)
corp_nation$Text <- rownames(corp_nation)
corp_nation$Theme <- rep("Nation", times = nrow(corp_nation))

corp_wealth <- subset.data.frame(corp_rep_df, select = wealth_w)
corp_wealth$Text <- rownames(corp_wealth)
corp_wealth$Theme <- rep("Wealth", times = nrow(corp_wealth))

# identifies terms not in the 19C corpus
m_excl <- setdiff(marriage_w,names(c19_rep_df))
n_excl <- setdiff(nation_w,names(c19_rep_df))
s_excl <- setdiff(self_w,names(c19_rep_df))
w_excl <- setdiff(wealth_w,names(c19_rep_df))
excl <- c(m_excl, n_excl, s_excl, w_excl) # Combine for comparison


c19_marriage <- subset.data.frame(c19_rep_df, 
                                  select = marriage_w[! marriage_w %in% m_excl])
c19_marriage$Text <- rownames(c19_marriage)
c19_marriage$Theme <- rep("Marriage", times = nrow(c19_marriage))

c19_self <- subset.data.frame(c19_rep_df, select = self_w[! self_w %in% s_excl])
c19_self$Text <- rownames(c19_self)
c19_self$Theme <- rep("Self", times = nrow(c19_self))

c19_nation <- subset.data.frame(c19_rep_df, 
                                select = nation_w[! nation_w %in% n_excl])
c19_nation$Text <- rownames(c19_nation)
c19_nation$Theme <- rep("Nation", times = nrow(c19_nation))

c19_wealth <- subset.data.frame(c19_rep_df, 
                                select = wealth_w[! wealth_w %in% w_excl])
c19_wealth$Text <- rownames(c19_wealth)
c19_wealth$Theme <- rep("Wealth", times = nrow(c19_wealth))

# Combine sub-theme results

corp_join <- full_join(corp_wealth, corp_marriage, by = c("Text", "Theme")) %>%
              full_join(corp_self, by = c("Text", "Theme")) %>%
              full_join(corp_nation, by = c("Text", "Theme")) 


corp_tidy <- corp_join %>%
        gather(key, Value, - Text, -Theme)

colnames(corp_tidy) <- c("Text", "Theme", "Word", "Value")   

corp_tidy <- filter(corp_tidy, Value != "NA")
corp_tidy$Author <- gsub("_.*","", corp_tidy$Text)

corp_tidy_spread <- spread(corp_tidy, key = Theme, value = Value)


c19_join <- full_join(c19_wealth, c19_marriage, by = c("Text", "Theme")) %>%
        full_join(c19_self, by = c("Text", "Theme")) %>%
        full_join(c19_nation, by = c("Text", "Theme")) 


c19_tidy <- c19_join %>%
        gather(key, Value, - Text, -Theme)

colnames(c19_tidy) <- c("Text", "Theme", "Word", "Value")   

c19_tidy <- filter(c19_tidy, Value != "NA")
c19_tidy$Author <- rep("19C", times = length(c19_tidy))

c19_tidy_spread <- spread(c19_tidy, key = Theme, value = Value)


# Create a combined copy

all_tidy <- rbind(corp_tidy, c19_tidy)

all_tidy_spread <- spread(all_tidy, key = Theme, value = Value)
all_tidy_spread[is.na(all_tidy_spread)] <- 0




# Identify which authors/texts used terms not in 19C

aeo_incl <- filter(corp_tidy, Word %in% excl) %>%
            filter(Value > 0)



###############################################################################
# Probably don't need this
#Transpose data frame
# t_corp_marriage <- as.data.frame(t(corp_marriage))
# t_corp_nation <- as.data.frame(t(corp_nation))
# t_corp_self <- as.data.frame(t(corp_self))
# t_corp_wealth <- as.data.frame(t(corp_wealth))
# 
# # Add rownames as a column
# t_corp_marriage$Word <- rownames(t_corp_marriage)
# t_corp_nation$Word <- rownames(t_corp_nation)
# t_corp_self$Word <- rownames(t_corp_self)
# t_corp_wealth$Word <- rownames(t_corp_wealth)
# 
# text <- gsub("\\.txt", "", aeo_corp)
# 
# # Tidy the data frame
# tidy_corp_m <- gather(data = t_corp_marriage, key = text, value = rel_freq, text)
# tidy_corp_m$Theme <- rep("Marriage", times = nrow(tidy_corp_m))
# 
# tidy_corp_n <- gather(data = t_corp_nation, key = text, value = rel_freq, text)
# tidy_corp_n$Theme <- rep("Nation", times = nrow(tidy_corp_n))
# 
# tidy_corp_s <- gather(data = t_corp_self, key = text, value = rel_freq, text)
# tidy_corp_s$Theme <- rep("Self", times = nrow(tidy_corp_s))
# 
# tidy_corp_w <- gather(data = t_corp_wealth, key = text, value = rel_freq, text)
# tidy_corp_w$Theme <- rep("Wealth", times = nrow(tidy_corp_w))
# 
# 
# # Combine the data frames
# tidy_corp <- rbind(tidy_corp_m, tidy_corp_n, tidy_corp_s, tidy_corp_w)
# 
# # Add columns for Date and Author
# 
# tidy_corp$Author <- gsub("_.*","", tidy_corp$text)
# tidy_corp$Year <- gsub("[^0-9]", "", tidy_corp$text)
# 
# tidy_corp_cl <- subset(tidy_corp, rel_freq > 0)
# # Save data
# write_csv(tidy_corp_cl, "4_1_Results/tidy_aeo.csv")




################################################################################

# calculating mean rel-freq per text per theme

# Convert to a Tibble
tidy_aeo_rf <- tbl_df(corp_tidy) %>%
                        group_by(Text, Theme) %>%
                        summarise(Mean_rel = mean(Value))

tidy_aeo_rf$Author <- gsub("_.*","", tidy_aeo_rf$Text)


tidy_aeo_spread <- spread(tidy_aeo_rf, key = Theme, value = Mean_rel)

tidy_c19_rf <- tbl_df(c19_tidy) %>%
        group_by(Text, Theme) %>%
        summarise(Mean_rel = mean(Value))

tidy_c19_rf$Author <- rep("19C", times = 188)


tidy_c19_spread <- spread(tidy_aeo_rf, key = Theme, value = Mean_rel)

all_tidy_mean <- all_tidy %>%
                    group_by(Text, Theme) %>%
                    summarise(Mean = mean(Value)) %>%
                    spread(key = Theme, value = Mean)

all_tidy_mean$Author <- c(rep("19C", times = 47), rep( "JA", times = 6), 
                          rep("ME", times = 16), rep("SO", times = 6))




###############################################################################

# Plot the results



# display.brewer.all(n = NULL, type = "all", select = NULL, exact.n = TRUE,
#                  colorblindFriendly = T)

co <- brewer.pal(n = 3, "Set1")
col2 <- viridis(3)
col2 <- append(col2, "black")
co3 <- brewer.pal(n = 4, "Set1")
co4 <- viridis(4)



ggplot(corp_tidy, aes(x = Theme, y = Value, color = Author)) +
        geom_point(aes(fill = Author), size = 1.5, 
                   position = position_jitter(w = 0.4, h = 0),
                   alpha = 0.4) +
        theme_bw() +
        theme() +
        ggtitle("Relative Frequencies of Independence Related Terms") +
        labs(x = "Theme", y = "Relative Frequency",
             subtitle = "Each theme consists of the top 100 unique terms for that theme from the independence lexicon") +
        scale_fill_manual(values = col2) 


ggplot(corp_tidy, aes(x = Theme, y = Value, col = Theme)) +
        geom_jitter(alpha = 0.4) +
        theme_bw() +
        theme() +
        ggtitle("Relative Frequencies of Independence Related Terms by Author") +
        labs(x = "Theme", y = "Relative Frequency",
             subtitle = "Each theme consists of the top 100 unique terms for that theme from the independence lexicon") +
        scale_fill_manual(values = co4) +
        facet_grid(. ~ Author)

corp_tidy %>%
        filter(Value > 0) %>%
        ggplot(aes(x = Author, y = Value, col = Author)) +
        geom_jitter(alpha = 0.4) +
        theme_bw() +
        theme() +
        ggtitle("Relative Frequencies of Independence Related Terms by Theme") +
        labs(y = "Relative Frequency",
             subtitle = "Each theme consists of the top 100 unique terms for that theme from the independence lexicon") +
        scale_fill_manual(values = col2) +
        facet_grid(. ~ Theme)

# Not happy with this - does not work well
ggplot(all_tidy_mean, aes(x = Marriage, y = Self, color = Author)) +
        geom_point(aes(fill = Author), size = 1.5, 
                   position = position_jitter(w = 0.4, h = 0),
                   alpha = 0.6) 

# Principal component analysis  


all.pca <- prcomp(all_tidy_mean[ , 2:5],
                 center = TRUE,
                 scale. = TRUE)

plot(all.pca, type = "l")
summary(all.pca)

scores = as.data.frame(all.pca$x)
scores$Author <- c(rep("19C", times = 47), rep( "JA", times = 6), 
                          rep("ME", times = 16), rep("SO", times = 6))

# plot of observations
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores), color = Author)) +
        geom_hline(yintercept = 0, colour = "gray65") +
        geom_vline(xintercept = 0, colour = "gray65") +
        geom_text(alpha = 0.9, size = 4) +
        ggtitle("PCA plot of Themes")

# 
# ggplot(ind_df, aes(Wealth, Nation, colour = Author)) +
#         geom_point(shape = 16, size = 4, show.legend = T, alpha = .4) +
#         theme_minimal()
# 
# ggplot(ind_df, aes(Wealth, Self, colour = Author)) +
#         geom_point(shape = 16, size = 4, show.legend = T, alpha = .4) +
#         theme_minimal()
# 

#####


