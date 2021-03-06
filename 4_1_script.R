################################################################################
# File-Name: 4_1_script.R                                                      #
# Date: 4 October 2017                                                         #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: PhD Thesis                                                          #
# Purpose: Code for Chapter 4.1 Frequency Analysis                             #
# Data Used: ja_corpus, me_corpus, so_corpus, AEO_Corpus, 19c_corpus           #
# Packages Used:  tm, koRpus, ggplot2                                          #
# Last Updated: 15 November 2017                                               #
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
#           Text Analysis with R for Students of Literature - M Jockers (2014)

##########
# SET UP #
##########

# Set working directory
setwd("~/PhD_Main/PhD_Analysis/4_1")

# Load packages
library(gsubfn)
library(tm)
library(cluster)
library(factoextra)

######################################
# Frequency Lists - individual texts #
######################################

# Create a path to the files
# JA
input.dir1 <- "ja_corpus" 
# Read the name of all .txt files
files1 <- dir(input.dir1, "\\.txt") 
text <- c("JA_SS", "JA_PP", "JA_MP", "JA_E", "JA_NA", "JA_P")

# SO
input.dir2 <- "so_corpus" 
# Read the name of all .txt files
files2 <- dir(input.dir2, "\\.txt") 
text2 <- c("SO_SC", "SO_NS", "SO_WG", "SO_M", "SO_OD", "SO_FM")

# ME
input.dir3 <- "me_corpus" 
# Read the name of all .txt files
files3 <- dir(input.dir3, "\\.txt") 
text3 <- c("ME_CR", "ME_B", "ME_PT", "ME_MG", "ME_E", "ME_L", "ME_A", "ME_D", "ME_M",
          "ME_MF", "ME_A_1811", "ME_C", "ME_V", "ME_P", "ME_H", "ME_O")

# m_corpus
input.dir4 <- "m_corpus"
files4 <- dir(input.dir4, "\\.txt")
text4 <- files4

# f_corpus
input.dir5 <- "f_corpus"
files5 <- dir(input.dir5, "\\.txt")
text5 <- files5

# Amend for each corpus
for(i in 1:length(files1)) {
        x <- scan(file = paste0(input.dir1, "/", files1[i]), what = "char", sep = "\n", 
                  quote = "", comment.char = "")
                  word_list <- strsplit(x, "\\W")
                  type <- unlist(word_list)
                  not_blank <- which(type != "")
                  type <- type[not_blank]
                  freq_list <- table(type)
                  sorted_list <- sort(freq_list, decreasing = T)
                  title <- files4[i]
                  y <- paste0("4_1_results/", title, "_raw_freq", ".csv")
                  y2 <- paste0("4_1_results/", title, "_rel_freq", ".csv")
                  write.csv(sorted_list, y)
                  
                 rel_freq <- round(100 * (sorted_list / sum(sorted_list)), 2)
                write.csv(rel_freq, y2)
}

# csv files for raw frequencies and relative frequencies created

######################################
# Frequencies for each author corpus #
######################################

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
dtm_ja <- DocumentTermMatrix(docs)
text_ja <- c("JA_SS", "JA_PP", "JA_MP", "JA_E", "JA_NA", "JA_P")
rownames(dtm_ja) <- text_ja # Allocates text names to rows

# Convert DTM to matrix
m_ja <- as.matrix(dtm_ja)
doc_tokens_ja <- rowSums(m_ja)
tokens_ja <- sum(rowSums(m_ja))

ja_corpus_raw_freq <- colSums(m_ja)

ja_corpus_rel_freq <- round(100 * (ja_corpus_raw_freq / tokens_ja), 2)

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
dtm_so <- DocumentTermMatrix(docs)
text_so <- c("SO_SC", "SO_NS", "SO_WG", "SO_M", "SO_OD", "SO_FM")
rownames(dtm_so) <- text_so # Allocates text names to rows

# Convert DTM to matrix
m_so <- as.matrix(dtm_so)
doc_tokens_so <- rowSums(m_so)
tokens_so <- sum(rowSums(m_so))

so_corpus_raw_freq <- colSums(m_so)

so_corpus_rel_freq <- round(100 * (so_corpus_raw_freq / tokens_so), 2)

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
dtm_me <- DocumentTermMatrix(docs)
text_me <- c("ME_CR", "ME_B", "ME_PT", "ME_MG", "ME_E", "ME_L", "ME_A", "ME_D", "ME_M",
             "ME_MF", "ME_A_1811", "ME_C", "ME_V", "ME_P", "ME_H", "ME_O")
rownames(dtm_me) <- text_me # Allocates text names to rows

# Convert DTM to matrix
m_me <- as.matrix(dtm_me)
doc_tokens_me <- rowSums(m_me)
tokens_me <- sum(rowSums(m_me))

me_corpus_raw_freq <- colSums(m_me)

me_corpus_rel_freq <- round(100 * (me_corpus_raw_freq / tokens_me), 2)

write.csv(me_corpus_raw_freq, "4_1_results/me_corpus_raw_freq.csv")
write.csv(me_corpus_rel_freq, "4_1_results/me_corpus_rel_freq.csv")


# f_corpus
# Provide a path to the corpus files
dname <- file.path("f_corpus") 
f_corp <- dir(dname) 

# Create Volatile Corpus
docs <- VCorpus(DirSource(dname))

# Preprocess corpus

docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace  
docs <- tm_map(docs, PlainTextDocument)

# Create a Document Term Matrix 
dtm_f <- DocumentTermMatrix(docs)
text_f <- f_corp
rownames(dtm_f) <- text_f # Allocates text names to rows

# Convert DTM to matrix
m_f <- as.matrix(dtm_f)
doc_tokens_f <- rowSums(m_f)
tokens_f <- sum(rowSums(m_f))

f_corpus_raw_freq <- colSums(m_f)

f_corpus_rel_freq <- round(100 * (f_corpus_raw_freq / tokens_f), 2)

write.csv(f_corpus_raw_freq, "4_1_results/f_corpus_raw_freq.csv")
write.csv(f_corpus_rel_freq, "4_1_results/f_corpus_rel_freq.csv")

# m_corpus
# Provide a path to the corpus files
dname <- file.path("m_corpus") 
m_corp <- dir(dname) 

# Create Volatile Corpus
docs <- VCorpus(DirSource(dname))

# Preprocess corpus

docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace  
docs <- tm_map(docs, PlainTextDocument)

# Create a Document Term Matrix 
dtm_m <- DocumentTermMatrix(docs)
text_m <- m_corp
rownames(dtm_m) <- text_m # Allocates text names to rows

# Convert DTM to matrix
m_m <- as.matrix(dtm_m)
doc_tokens_m <- rowSums(m_m)
tokens_m <- sum(rowSums(m_m))

m_corpus_raw_freq <- colSums(m_m)

m_corpus_rel_freq <- round(100 * (m_corpus_raw_freq / tokens_m), 2)

write.csv(m_corpus_raw_freq, "4_1_results/m_corpus_raw_freq.csv")
write.csv(m_corpus_rel_freq, "4_1_results/m_corpus_rel_freq.csv")

# aeo_corp
# Provide a path to the corpus files
dname <- file.path("AEO_corpus") 
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
dtm_aeo <- DocumentTermMatrix(docs)
text_aeo <- aeo_corp
rownames(dtm_aeo) <- text_aeo # Allocates text names to rows

# Convert DTM to matrix
m_aeo <- as.matrix(dtm_aeo)
doc_tokens_aeo <- rowSums(m_aeo)
tokens_aeo <- sum(rowSums(m_aeo))

aeo_corpus_raw_freq <- colSums(m_aeo)

aeo_corpus_rel_freq <- round(100 * (aeo_corpus_raw_freq / tokens_aeo), 2)

write.csv(aeo_corpus_raw_freq, "4_1_results/aeo_corpus_raw_freq.csv")
write.csv(aeo_corpus_rel_freq, "4_1_results/aeo_corpus_rel_freq.csv")

# all_corpus
# Provide a path to the corpus files
dname <- file.path("all_corpus") 
all_corp <- dir(dname) 

# Create Volatile Corpus
docs <- VCorpus(DirSource(dname))

# Preprocess corpus

docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace  
docs <- tm_map(docs, PlainTextDocument)

# Create a Document Term Matrix 
dtm_all <- DocumentTermMatrix(docs)
text_all <- all_corp
rownames(dtm_all) <- text_all # Allocates text names to rows

# Convert DTM to matrix
m_all <- as.matrix(dtm_all)
doc_tokens_all <- rowSums(m_all)
tokens_all <- sum(rowSums(m_all))

all_corpus_raw_freq <- colSums(m_all)

all_corpus_rel_freq <- round(100 * (all_corpus_raw_freq / tokens_all), 2)

write.csv(all_corpus_raw_freq, "4_1_results/all_corpus_raw_freq.csv")
write.csv(all_corpus_rel_freq, "4_1_results/all_corpus_rel_freq.csv")



###########################
# EXAMINE TEXT SIMILARITY #
###########################
# AEO
# Compute Euclidean distance between document vectors
d_aeo <- dist(m_aeo)

# Run hierarchical clustering using Ward’s method
groups <- hclust(d_aeo, method = "ward.D")

# Plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang = -1, main = "Cluster Dendrogram of Austen Edgeworth
     Owenson Corpus",  sub = "Calculated using Ward's method", 
     xlab = "Texts")

# library(ape)
# 
# plot(as.phylo(groups), type = "fan")
# 
# colors <- c("red", "blue", "green", "black")
# clus4 <- cutree(groups, 4)
# plot(as.phylo(groups), type = "fan", tip.color = colors[clus4],
#      label.offset = 1, cex = 0.7)

library(ggdendro)
library(dendextend)

ggdendrogram(groups, rotate = T, theme_dendro = F) +
        geom_hline(aes(yintercept = 20000, colour="red")) +
        theme(legend.position = "none") +
        labs(title ="AEO Corpus Dendrogram of Text Grouping Based on Document Term Matrix",
             subtitle = "Calculated using Ward's method", y="Height", x="Text")

# dend <- groups %>% as.dendrogram %>%
#         set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
#         set("labels_cex", 0.7) 
#         
# # plot the dend in usual "base" plotting engine:
# plot(dend )
# 
# ggd1 <- as.ggdend(dend)
# 
# ggplot(ggd1, horiz = T) +
#         geom_hline(aes(yintercept = 20000, colour="red")) +
#         theme(legend.position = "none") +
#         labs(title ="Dendrogram of Text Grouping - AEO Corpus",
#              subtitle = "Calculated using Ward's method", y="Height", x="Text")



# Extracted Terms
# Compute Euclidean distance between document vectors
dx <- dist(x)

# Run hierarchical clustering using Ward’s method
groupsx <- hclust(dx, method = "ward.D")

# Plot dendogram, use hang to ensure that labels fall below tree
plot(groupsx, hang = -1, main = "Cluster Dendrogram of Austen, Edgeworth and
     Owenson Extracted Terms",  sub = "Calculated using Ward's method")



################################################################################

# me_rel <- read.csv("4_1_results/me_corpus_rel_freq.csv", header = T)
# so_rel <- read.csv("4_1_results/so_corpus_rel_freq.csv", header = T)
# ja_rel <- read.csv("4_1_results/ja_corpus_rel_freq.csv", header = T)
# m_rel <- read.csv("4_1_results/m_corpus_rel_freq.csv", header = T)
# f_rel <- read.csv("4_1_results/f_corpus_rel_freq.csv", header = T)
# 
# colnames(me_rel) <- c("word", "freq")
# colnames(so_rel) <- c("word", "freq")
# colnames(ja_rel) <- c("word", "freq")
# colnames(f_rel) <- c("word", "freq")
# colnames(m_rel) <- c("word", "freq")
# 
# # Sort data frames by rel_freq
# 
# me_rel_sort <- me_rel[order(me_rel$freq, decreasing = T),  ]
# so_rel_sort <- so_rel[order(so_rel$freq, decreasing = T),  ]
# ja_rel_sort <- ja_rel[order(ja_rel$freq, decreasing = T),  ]
# f_rel_sort <- f_rel[order(f_rel$freq, decreasing = T),  ]
# m_rel_sort <- m_rel[order(m_rel$freq, decreasing = T),  ]

################################################################################

# Analysis based on Jockers 2014

input.dir <- "ja_corpus" 
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt") 

# Sense and Sensibility
ja_ss  <- scan(file = paste0(input.dir, "/", files[1]), what = "char", 
                        sep = "\n", quote = "", comment.char = "")

novel <- paste(ja_ss, collapse = " ")
novel_l <- tolower(novel)
novel_words <- strsplit(novel_l, "\\W")
novel_words <- unlist(novel_words)
not_blank <- which(novel_words != "")
novel_words <- novel_words[not_blank]
ja_ss_words <- novel_words

ja_ss_freq <- table(ja_ss_words)

ja_ss_sort <- sort(ja_ss_freq, decreasing = T)

ja_ss_sort[1:10]
ja_ss_sort["independent"]/ja_ss_sort["dependent"]
ja_ss_sort["he"]/ja_ss_sort["she"]

ja_ss_rel <- 100 * (ja_ss_sort / sum(ja_ss_sort))

plot(ja_ss_rel[1:10], type = "b", xlab = "Top Ten Words JA SS", xaxt = "n")
axis(1, 1:10, labels = names(ja_ss_rel[1:10]))

# Pride and Prejudice
ja_pp  <- scan(file = paste0(input.dir, "/", files[2]), what = "char", 
               sep = "\n", quote = "", comment.char = "")

novel <- paste(ja_pp, collapse = " ")
novel_l <- tolower(novel)
novel_words <- strsplit(novel_l, "\\W")
novel_words <- unlist(novel_words)
not_blank <- which(novel_words != "")
novel_words <- novel_words[not_blank]
ja_pp_words <- novel_words

ja_pp_freq <- table(ja_pp_words)

ja_pp_sort <- sort(ja_pp_freq, decreasing = T)

ja_pp_sort[1:10]

ja_pp_rel <- 100 * (ja_pp_sort / sum(ja_pp_sort))

plot(ja_pp_rel[1:10], type = "b", xlab = "Top Ten Words JA PP", xaxt = "n")
axis(1, 1:10, labels = names(ja_pp_rel[1:10]))

ja_mp  <- scan(file = paste0(input.dir, "/", files[3]), what = "char", 
               sep = "\n", quote = "", comment.char = "")

novel <- paste(ja_mp, collapse = " ")
novel_l <- tolower(novel)
novel_words <- strsplit(novel_l, "\\W")
novel_words <- unlist(novel_words)
not_blank <- which(novel_words != "")
novel_words <- novel_words[not_blank]
ja_mp_words <- novel_words

ja_mp_freq <- table(ja_mp_words)

ja_mp_sort <- sort(ja_mp_freq, decreasing = T)

ja_mp_sort[1:10]

ja_mp_rel <- 100 * (ja_mp_sort / sum(ja_mp_sort))

plot(ja_mp_rel[1:10], type = "b", xlab = "Top Ten Words JA MP", xaxt = "n")
axis(1, 1:10, labels = names(ja_mp_rel[1:10]))

ja_e  <- scan(file = paste0(input.dir, "/", files[4]), what = "char", 
               sep = "\n", quote = "", comment.char = "")

novel <- paste(ja_e, collapse = " ")
novel_l <- tolower(novel)
novel_words <- strsplit(novel_l, "\\W")
novel_words <- unlist(novel_words)
not_blank <- which(novel_words != "")
novel_words <- novel_words[not_blank]
ja_e_words <- novel_words

ja_e_freq <- table(ja_e_words)

ja_e_sort <- sort(ja_e_freq, decreasing = T)

ja_e_sort[1:10]

ja_e_rel <- 100 * (ja_e_sort / sum(ja_e_sort))

plot(ja_e_rel[1:10], type = "b", xlab = "Top Ten Words JA E", xaxt = "n")
axis(1, 1:10, labels = names(ja_e_rel[1:10]))

ja_na  <- scan(file = paste0(input.dir, "/", files[5]), what = "char", 
               sep = "\n", quote = "", comment.char = "")

novel <- paste(ja_na, collapse = " ")
novel_l <- tolower(novel)
novel_words <- strsplit(novel_l, "\\W")
novel_words <- unlist(novel_words)
not_blank <- which(novel_words != "")
novel_words <- novel_words[not_blank]
ja_na_words <- novel_words

ja_na_freq <- table(ja_na_words)

ja_na_sort <- sort(ja_na_freq, decreasing = T)

ja_na_sort[1:10]

ja_na_rel <- 100 * (ja_na_sort / sum(ja_na_sort))

plot(ja_na_rel[1:10], type = "b", xlab = "Top Ten Words JA NA", xaxt = "n")
axis(1, 1:10, labels = names(ja_na_rel[1:10]))


ja_p  <- scan(file = paste0(input.dir, "/", files[6]), what = "char", 
               sep = "\n", quote = "", comment.char = "")

novel <- paste(ja_p, collapse = " ")
novel_l <- tolower(novel)
novel_words <- strsplit(novel_l, "\\W")
novel_words <- unlist(novel_words)
not_blank <- which(novel_words != "")
novel_words <- novel_words[not_blank]
ja_p_words <- novel_words

ja_p_freq <- table(ja_p_words)

ja_p_sort <- sort(ja_p_freq, decreasing = T)

ja_p_sort[1:10]

ja_p_rel <- 100 * (ja_p_sort / sum(ja_p_sort))

plot(ja_p_rel[1:10], type = "b", xlab = "Top Ten Words JA P", xaxt = "n")
axis(1, 1:10, labels = names(ja_p_rel[1:10]))

# independent

length(ja_ss_words[which(ja_ss_words == "independent")])

length(ja_pp_words[which(ja_pp_words == "independent")])

length(ja_mp_words[which(ja_mp_words == "independent")])

length(ja_e_words[which(ja_e_words == "independent")])

length(ja_na_words[which(ja_na_words == "independent")])

length(ja_p_words[which(ja_p_words == "independent")])


################################################################################ 


# Measures of Lexical Variety

################################################################################ 
# Lexical Co-occurrence: Collocations

################################################################################ 
# (Lexico-)Grammatical Co-occurrence: Concordances




################################################################################ 
# Whole AEO corpus

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
plot(groups, hang = -1, main = "Cluster Dendrogram of AEO Corpus", 
     sub = "Calculated using Ward's method")


# An alternative uses the factoextra package
m_scale <- scale(m)
dist.eucl <- dist(m, method = "euclidean")
# m2 <- round(as.matrix(dist(m_scale, method = "euclidean")), 2)
fviz_dist(dist.eucl)

################################################################################

# Whole 19thC + AEO corpus

# Provide a path to the corpus files
dname <- file.path("all_Corpus") 

# List the files in the directory and save
dir(dname) 

all_corpus <- dir(dname) 

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
rownames(dtm) <- all_corpus # Allocates text names to rows

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
par(cex=0.5)
plot(groups, hang = -1, main = "Cluster Dendrogram of Full 19thC Corpus", 
      sub = "Calculated using Ward's method")


# An alternative uses the factoextra package
m_scale <- scale(m)
dist.eucl <- dist(m, method = "euclidean")
# m2 <- round(as.matrix(dist(m_scale, method = "euclidean")), 2)
fviz_dist(dist.eucl)

# Enhanced k-means clustering
res.km <- eclust(m_scale, "kmeans", nstart = 25)

fviz_gap_stat(res.km$gap_stat)

res.km$nbclust

res.hc <- eclust(df, "hclust")

fviz_dend(res.hc, rect = TRUE) # dendrogram

################################################################################ 

# Stylometric Analysis

library(stylo)
# Can be used for network creation
stylo(gui = T, corpus.dir = "AEO_Corpus")

stylo(gui = F, corpus.dir = "19c_corpus")



# Using Oppose to identify words significantly preferred or avoided.
# Using the 19c_corpus 41 novels (28 F, 13 M)


ja_oppose <- oppose(gui = T, primary.corpus.dir = "ja_corpus", 
       secondary.corpus.dir = "19c_corpus", write.pdf.file = T )

me_oppose <- oppose(gui = F, primary.corpus.dir = "me_corpus", 
                    secondary.corpus.dir = "19c_corpus", write.pdf.file = T )

so_oppose <- oppose(gui = F, primary.corpus.dir = "so_corpus", 
                    secondary.corpus.dir = "19c_corpus", write.pdf.file = T )

summary(ja_oppose) # Lists the variables saved they can be accessed using $

ja_comparison <- ja_oppose$comparison
me_comparison <- me_oppose$comparison
so_comparison <- so_oppose$comparison

ja_comparison["independence", ]
me_comparison["independence", ]
so_comparison["independence", ]

ja_comparison["dependence", ]
me_comparison["dependence", ]
so_comparison["dependence", ]

me_oppose$words.preferred.scores
me_comparison["whilst", ]
