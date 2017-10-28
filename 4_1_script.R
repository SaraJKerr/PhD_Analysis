################################################################################
# File-Name: 4_1_script.R                                                      #
# Date: 4 October 2017                                                         #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: PhD Thesis                                                          #
# Purpose: Code for Chapter 4.1 Frequency Analysis                             #
# Data Used: AEO_Corpus 19C_corpus                                             #
# Packages Used:  tm, koRpus, ggplot2                                          #
# Last Updated: 27 October 2017                                                 #
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

# Frequency Lists
ss <- scan(file = "ja_corpus/JA_1811_SS.txt", what = "char", sep = "\n", 
           quote = "", comment.char = "")
head(ss)

ss <- tolower(ss)
word_list <- strsplit(ss, "\\W")
type <- unlist(word_list)
not_blank <- which(type != "")
type <- type[not_blank]
freq_list <- table(type)
sorted_list <- sort(freq_list, decreasing = T)

head(sorted_list)

write.csv(sorted_list, "ss_freq_sorted.csv")

length(type)

rel_freq <- round(100 * (sorted_list / sum(sorted_list)), 2)

write.csv(rel_freq, "ss_rel_freq.csv")

# Measures of Lexical Variety

# Lexical Co-occurrence: Collocations

# (Lexico-)Grammatical Co-occurrence: Concordances





# Load packages
library(tm)
library(cluster)
library(factoextra)

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






