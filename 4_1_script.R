################################################################################
# File-Name: 4_1_script.R                                                      #
# Date: 4 October 2017                                                         #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: PhD Thesis                                                          #
# Purpose: Code for Chapter 4.1 Frequency Analysis                             #
# Data Used: ja_corpus, me_corpus, so_corpus, AEO_Corpus 19C_corpus                                             #
# Packages Used:  tm, koRpus, ggplot2                                          #
# Last Updated: 1 November 2017                                                 #
################################################################################

# Code for Chapter 4 Term-Document Matrices, Section 1 Frequency Analysis

# Requirements: folders containing texts in plain text format: ja_corpus, 
# me_corpus, so_corpus, AEO_corpus and 19C_corpus

# Results are saved in the 4_1_results and 4_1_plots folders

# Sources:  https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to
#           -cluster-analysis-using-r/
#           https://www.r-bloggers.com/hierarchical-clustering-in-r-2/
#           http://www.sthda.com/english/articles/25-cluster-analysis-in-r-
#           practical-guide/106-cluster-analysis-in-r-simplified-and-enhanced/
#           Quantitative Corpus Linguistics with R - Stefan Th. Gries (2009)

# Set working directory
setwd("~/PhD_Main/PhD_Analysis/4_1")

# Load packages
library(gsubfn)
library(tm)
library(cluster)
library(factoextra)

# Frequency Lists - individual texts

# Create a path to the files
# JA
input.dir <- "ja_corpus" 
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt") 
text <- c("JA_SS", "JA_PP", "JA_MP", "JA_E", "JA_NA", "JA_P")

# SO
input.dir <- "so_corpus" 
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt") 
text <- c("SO_SC", "SO_NS", "SO_WG", "SO_M", "SO_OD", "SO_FM")

# ME
input.dir <- "me_corpus" 
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt") 
text <- c("ME_CR", "ME_B", "ME_MG", "ME_E", "ME_L", "ME_A", "ME_D", "ME_M",
          "ME_MF", "ME_A_1811", "ME_C", "ME_V", "ME_P", "ME_H", "ME_O")

for(i in 1:length(files)) {
        x <- scan(file = paste0(input.dir, "/", files[i]), what = "char", sep = "\n", 
                  quote = "", comment.char = "")
                  word_list <- strsplit(x, "\\W")
                  type <- unlist(word_list)
                  not_blank <- which(type != "")
                  type <- type[not_blank]
                  freq_list <- table(type)
                  sorted_list <- sort(freq_list, decreasing = T)
                  title <- text[i]
                  y <- paste0("4_1_results/", title, "_raw_freq", ".csv")
                  y2 <- paste0("4_1_results/", title, "_rel_freq", ".csv")
                  write.csv(sorted_list, y)
                  
                  rel_freq <- round(100 * (sorted_list / sum(sorted_list)), 2)
                  write.csv(rel_freq, y2)
}

# csv files for raw frequencies and relative frequencies created

# Frequencies for each author corpus

# JA
# Provide a path to the corpus files
dname <- file.path("ja_corpus") 
ja <- dir(dname) 

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
text <- c("JA_SS", "JA_PP", "JA_MP", "JA_E", "JA_NA", "JA_P")
rownames(dtm) <- text # Allocates text names to rows

# Convert DTM to matrix
m <- as.matrix(dtm)
doc_tokens <- rowSums(m)
tokens <- sum(rowSums(m))

ja_corpus_raw_freq <- colSums(m)

ja_corpus_rel_freq <- round(100 * (ja_corpus_raw_freq / tokens), 2)

write.csv(ja_corpus_raw_freq, "4_1_results/ja_corpus_raw_freq.csv")
write.csv(ja_corpus_rel_freq, "4_1_results/ja_corpus_rel_freq.csv")

# SO
# Provide a path to the corpus files
dname <- file.path("so_corpus") 
so <- dir(dname) 

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
text <- c("SO_SC", "SO_NS", "SO_WG", "SO_M", "SO_OD", "SO_FM")
rownames(dtm) <- text # Allocates text names to rows

# Convert DTM to matrix
m <- as.matrix(dtm)
doc_tokens <- rowSums(m)
tokens <- sum(rowSums(m))

so_corpus_raw_freq <- colSums(m)

so_corpus_rel_freq <- round(100 * (so_corpus_raw_freq / tokens), 2)

write.csv(so_corpus_raw_freq, "4_1_results/so_corpus_raw_freq.csv")
write.csv(so_corpus_rel_freq, "4_1_results/so_corpus_rel_freq.csv")

# ME
# Provide a path to the corpus files
dname <- file.path("me_corpus") 
me <- dir(dname) 

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
text <- c("ME_CR", "ME_B", "ME_MG", "ME_E", "ME_L", "ME_A", "ME_D", "ME_M",
          "ME_MF", "ME_A_1811", "ME_C", "ME_V", "ME_P", "ME_H", "ME_O")
rownames(dtm) <- text # Allocates text names to rows

# Convert DTM to matrix
m <- as.matrix(dtm)
doc_tokens <- rowSums(m)
tokens <- sum(rowSums(m))

me_corpus_raw_freq <- colSums(m)

me_corpus_rel_freq <- round(100 * (me_corpus_raw_freq / tokens), 2)

write.csv(me_corpus_raw_freq, "4_1_results/me_corpus_raw_freq.csv")
write.csv(me_corpus_rel_freq, "4_1_results/me_corpus_rel_freq.csv")

me_rel <- read.csv("4_1_results/me_corpus_rel_freq.csv", header = T)
so_rel <- read.csv("4_1_results/so_corpus_rel_freq.csv", header = T)
ja_rel <- read.csv("4_1_results/ja_corpus_rel_freq.csv", header = T)

colnames(me_rel) <- c("word", "freq")
colnames(so_rel) <- c("word", "freq")
colnames(ja_rel) <- c("word", "freq")

# Sort data frames by rel_freq

me_rel_sort <- me_rel[order(me_rel$freq, decreasing = T),  ]
so_rel_sort <- so_rel[order(so_rel$freq, decreasing = T),  ]
ja_rel_sort <- ja_rel[order(ja_rel$freq, decreasing = T),  ]

################################################################################ 


# Measures of Lexical Variety

################################################################################ 
# Lexical Co-occurrence: Collocations

################################################################################ 
# (Lexico-)Grammatical Co-occurrence: Concordances




################################################################################ 
# Whole corpus

# Provide a path to the corpus files
dname <- file.path("AEO_Corpus") 

# List the files in the directory and save
dir(dname) 

aeo <- dir(dname) 

# Create Volatile Corpus
docs <- VCorpus(DirSource(dname))

# Preprocess corpus

docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   

# The standard stoplist words are removed 
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace  

# To check the preprocessing
writeLines(as.character(docs[[10]]))

docs <- tm_map(docs, PlainTextDocument)

# Create a Document Term Matrix 
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- aeo # Allocates text names to rows

# Print a summary
dtm

# Convert DTM to matrix
m <- as.matrix(dtm)
rowSums(m)
tokens <- sum(rowSums(m))


doc_tokens <- rowSums(m)


# Compute Euclidean distance between document vectors
d <- dist(m)


# Run hierarchical clustering using Ward’s method
groups <- hclust(d, method = "ward.D")


# Plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang = -1, main = "Cluster Dendrogram of JAMESO Corpus", 
     sub = "Calculated using Ward's method")


# An alternative uses the factoextra package
m_scale <- scale(m)
dist.eucl <- dist(m, method = "euclidean")
# m2 <- round(as.matrix(dist(m_scale, method = "euclidean")), 2)
fviz_dist(dist.eucl)

fviz_nbclust(m_scale, kmeans,
             method = "gap_stat")


################################################################################ 

# Stylometric Analysis




