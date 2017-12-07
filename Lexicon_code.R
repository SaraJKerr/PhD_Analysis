################################################################################
# File-Name: Lexicon_code.R                                                    #
# Date: 8 November 2017                                                        #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: PhD Thesis                                                          #
# Purpose: Code to create a lexicon of terms related to independence           #
# Data Used: clmet3.1 corpus                                                   #
# Packages Used:  wordVectors                                                  #
# Last Updated: 7 December 2017                                                #
################################################################################

# Creating a lexicon of independence terms

setwd("~/PhD_Main/PhD_Analysis/Data/clmet/corpus/txt")

library(wordVectors)

prep_word2vec("plain", "Clmet_processed_period_1_2.txt", lowercase = T)

clmet <- train_word2vec("Clmet_processed_period_1_2.txt", 
                         output = "Clmet_model_period_1_2.bin", threads = 1, 
                          vectors = 300, window = 12)


# If .bin file is already created - start here
clmet <- read.vectors("Clmet_model_period_1_2.bin")

# independence_250 <- closest_to(clmet, "independence", n = 250)
independence_500 <- closest_to(clmet, "independence", n = 500)
ind_500_names <- independence_500$word

head(independence_500)
ind <- independence_500[1:100, ]

# Extract words
ind_names <- ind$word


# Create lists for key areas of independence using vector addition
ind_wealth <- closest_to(clmet, ~ "independence" + "wealth", n = 100)

wealth <- ind_wealth$word

head(ind_wealth)

ind_self <- closest_to(clmet, ~ "independence" + "self", n = 100)

self <- ind_self$word

head(ind_self)

ind_marriage <- closest_to(clmet, ~ "independence" + "marriage", n = 100)

marriage <- ind_marriage$word

head(ind_marriage)

ind_nation <- closest_to(clmet, ~ "independence" + "nation", n = 100)

nation <- ind_nation$word

head(ind_nation)


# Combine each name file into a data frame and save
lexicon_full <- cbind(ind_names, wealth, marriage, self, nation)

write.csv(lexicon_full, "~/PhD_Main/PhD_Analysis/Data/lexicon_full.csv")

# Filter lexicon to remove words not in AEO Corpus
setwd("~/PhD_Main/PhD_Analysis/4_1")

library(tm)

input.dir <- "AEO_corpus" 
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt")

# Create Volatile Corpus
docs <- VCorpus(DirSource(input.dir))

# Preprocess corpus
docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers  
#docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, tolower)   # Convert to lowercase   
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace  
docs <- tm_map(docs, PlainTextDocument)

# Create formats needed for analysis
# Create a Document Term Matrix 
dtm <- DocumentTermMatrix(docs)

rownames(dtm) <- files # Allocates text names to rows

# Convert DTM to matrix
m <- as.matrix(dtm)
doc_tokens <- rowSums(m)
tokens <- sum(rowSums(m))

# Identify independence terms which appear in the AEO texts
words <- colnames(m)
ind_terms <- ind_names[which(ind_names %in% words == T)]
marriage_terms <- marriage[which(marriage %in% words == T)]
nation_terms <- nation[which(nation %in% words == T)]
self_terms <- self[which(self %in% words == T)]
wealth_terms <- wealth[which(wealth %in% words == T)]

length(ind_terms)
length(marriage_terms)
length(nation_terms)
length(self_terms)
length(wealth_terms)

# Create lexicon with top 50 filtered terms for each category
lexicon <- cbind(ind_terms[1:50], wealth_terms[1:50], marriage_terms[1:50],
                      self_terms[1:50], nation_terms[1:50])

write.csv(lexicon, "~/PhD_Main/PhD_Analysis/Data/lexicon.csv")
