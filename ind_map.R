# Test to create a plot for aspects of independence

# Set working directory
setwd("~/PhD_Main/PhD_Analysis/4_1")

library(tm)

# Provide a path to the corpus files
dname <- file.path("aeo_corpus") 
aeo_corp <- dir(dname) 

# Create Volatile Corpus
docs <- VCorpus(DirSource(dname))

# Preprocess corpus


docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace  
docs <- tm_map(docs, PlainTextDocument)

# Create a Document Term Matrix 
dtm <- DocumentTermMatrix(docs)

rownames(dtm) <- gsub("\\.txt", "", aeo_corp) # Allocates text names to rows


# Convert DTM to matrix
aeo_corp_m <- as.matrix(dtm)
doc_tokens <- rowSums(aeo_corp_m)

corp_rel <- round(100 * (aeo_corp_m / doc_tokens), 2)

corp_rep_df <- as.data.frame(corp_rel)

lex <- read.csv("~/PhD_Main/PhD_Analysis/Data/lexicon_unique.csv")
lex <- lex[, -1]

wealth_w <- as.character(lex[,1])
marriage_w <- as.character(lex[,2])
self_w <- as.character(lex[,3])
nation_w <- as.character(lex[,4])


corp_wealth <- subset.data.frame(corp_rep_df, select = wealth_w)
corp_marriage <- subset.data.frame(corp_rep_df, select = marriage_w)
corp_self <- subset.data.frame(corp_rep_df, select = self_w)
corp_nation <- subset.data.frame(corp_rep_df, select = nation_w)

library(tidyverse)
# Transpose data frame
t_corp_marriage <- as.data.frame(t(corp_marriage))
t_corp_nation <- as.data.frame(t(corp_nation))
t_corp_self <- as.data.frame(t(corp_self))
t_corp_wealth <- as.data.frame(t(corp_wealth))

# Add rownames as a column
t_corp_marriage$Word <- rownames(t_corp_marriage)
t_corp_nation$Word <- rownames(t_corp_nation)
t_corp_self$Word <- rownames(t_corp_self)
t_corp_wealth$Word <- rownames(t_corp_wealth)

text <- gsub("\\.txt", "", aeo_corp)

# Tidy the data frame
tidy_corp_m <- gather(data = t_corp_marriage, key = text, value = rel_freq, text)
tidy_corp_m$Theme <- rep("Marriage", times = nrow(tidy_corp_m))

tidy_corp_n <- gather(data = t_corp_nation, key = text, value = rel_freq, text)
tidy_corp_n$Theme <- rep("Nation", times = nrow(tidy_corp_n))

tidy_corp_s <- gather(data = t_corp_self, key = text, value = rel_freq, text)
tidy_corp_s$Theme <- rep("Self", times = nrow(tidy_corp_s))

tidy_corp_w <- gather(data = t_corp_wealth, key = text, value = rel_freq, text)
tidy_corp_w$Theme <- rep("Wealth", times = nrow(tidy_corp_w))


# Combine the data frames
tidy_corp <- rbind(tidy_corp_m, tidy_corp_n, tidy_corp_s, tidy_corp_w)

# Add columns for Date and Author

tidy_corp$Author <- gsub("_.*","", tidy_corp$text)
tidy_corp$Year <- gsub("[^0-9]", "", tidy_corp$text)

tidy_corp_cl <- subset(tidy_corp, rel_freq > 0)
# Save data
write_csv(tidy_corp_cl, "4_1_Results/tidy_aeo.csv")




library(RColorBrewer)

# display.brewer.all(n = NULL, type = "all", select = NULL, exact.n = TRUE,
#                  colorblindFriendly = T)

co <- brewer.pal(n = 3, "Set1")

qplot(x = Year, y = rel_freq, data = tidy_corp, color = Author, geom = "point")

# ggplot(tidy_corp, aes(x = Theme, y = rel_freq, colour = Author)) +
#         geom_point(size = 1.5, position = position_jitter(w = 0.4, h = 0)) +
#         theme_bw() +
#         theme() +
#         ggtitle("Relative Frequencies of Independence Related Terms") +
#         labs(x = "Theme", y = "Relative Frequency",
#              subtitle = "Each theme consists of the top 100 unique terms for that theme from the independence lexicon") +
#         scale_fill_manual(values = co) 
        

ggplot(tidy_corp_cl, aes(x = Theme, y = rel_freq, color = Author)) +
        geom_point(aes(fill = Author), size = 1.5, 
                   position = position_jitter(w = 0.4, h = 0),
                   alpha = 0.6) +
        theme_bw() +
        theme() +
        ggtitle("Relative Frequencies of Independence Related Terms") +
        labs(x = "Theme", y = "Relative Frequency",
             subtitle = "Each theme consists of the top 100 unique terms for that theme from the independence lexicon") +
        scale_fill_manual(values = co) 


################################################################################
th <- c("Marriage", "Nation", "Self", "Wealth") 

# calculating mean rel-freq per text per theme
df <- data.frame()

ja1 <- subset(tidy_corp_cl, text == tx[1])
ja2 <- subset(tidy_corp_cl, text == tx[2])
ja3 <- subset(tidy_corp_cl, text == tx[3])
ja4 <- subset(tidy_corp_cl, text == tx[4])
ja5 <- subset(tidy_corp_cl, text == tx[5])
ja6 <- subset(tidy_corp_cl, text == tx[6])


ja1_m <- round(mean(ja1$rel_freq[which(ja1$Theme == th[1])]), 2)
                
                
                df <- rbind(df, c(y, tx[1], th[j]))

sapply(ja1$rel_freq[which(ja1$Theme == th)], mean)      # not right but close  






###############################################################################



ind_df <- as.data.frame(cbind(corp_wealth$Total, 
                              corp_marriage$Total, 
                              corp_self$Total, 
                              corp_nation$Total))

colnames(ind_df) <- c("Wealth", "Marriage", "Self", "Nation")





min(ind_df$Wealth)
max(ind_df$Marriage)
max(ind_df$Self)
min(ind_df$Nation)

# ind_df$Wealth <- ind_df$Wealth - 2 * ind_df$Wealth
# ind_df$Nation <- ind_df$Nation - 2 * ind_df$Nation

library(ggplot2)

ggplot(ind_df, aes(Wealth, Marriage, colour = Author)) +
        geom_point(shape = 16, size = 4, show.legend = T, alpha = .4) +
        theme_minimal()

ggplot(ind_df, aes(Marriage, Self, colour = Author)) +
        geom_point(shape = 16, size = 4, show.legend = T, alpha = .4) +
        theme_minimal()

ggplot(ind_df, aes(Self, Nation, colour = Author)) +
        geom_point(shape = 16, size = 4, show.legend = T, alpha = .4) +
        theme_minimal()

ggplot(ind_df, aes(Wealth, Nation, colour = Author)) +
        geom_point(shape = 16, size = 4, show.legend = T, alpha = .4) +
        theme_minimal()

ggplot(ind_df, aes(Wealth, Self, colour = Author)) +
        geom_point(shape = 16, size = 4, show.legend = T, alpha = .4) +
        theme_minimal()


#####


