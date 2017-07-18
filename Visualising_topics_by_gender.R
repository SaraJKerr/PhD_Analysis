# Load packages
library(ggplot2)
library(RColorBrewer)
library(viridis)

# Set working Directory
setwd()

# Bocorpus without names
# Reading in the keys and composition files - these must be in csv format

# Keys
keys <- read.csv("bocorpus_no_names_keys20.csv", header = F)
t <- as.vector(keys$V3)
x <- strsplit(t, " ")
words <- as.data.frame(x)
n <- seq(0, 19, 1)
name <- vector()
for (i in 1:length(n)) {
        name[i] <- paste0("Topic ", n[i])
}
colnames(words) <- name

write.csv(words, "bocorpus_no_names_words.csv")

# Composition
goth <- read.csv("bocorpus_no_names_composition20.csv", header = F )
goth <- goth[-1,]
goth$V1 <- seq(1:21)
files <- dir("bocorpus")
goth$V2 <- files

# Prepare data file
goth_topics <- goth[2:3] # add file names and topic number
rownames(goth_topics) <- seq(1:21)
colnames(goth_topics) <- c("Name", "Topic")

goth_topics$Date <- c(1764, 1778, 1786, 1794, 1796, 1798, 1798, 1817, 1818, 
                      1819, 1837, 1838, 1847, 1847, 1860, 1872, 1886, 1892,
                      1893, 1897, 1898)
goth_topics$Gender <- c("M","F","M","F","M","M","F","F","F","M","M","M","F","F",
                        "M","M","M","F","M","M","M")

# Plot Topics by Date and Gender

ggplot(goth_topics, aes(x = Date, y = Topic, color = Gender)) +
        geom_point(size = 3) +
        scale_y_continuous(breaks = seq(0, 19, 1)) +
        scale_x_continuous(breaks = seq(1750, 1900, 25)) +
        ggtitle("Bocorpus (without names) Topics by Gender") +
        ylab("Topic Number") +
        xlab("Date of Publication")

# Bocorpus tagged nouns and verbs
# Reading in the keys and composition files - these must be in csv format

# Keys
keys <- read.csv("bocorpus_tagged_nv_keys20.csv", header = F)
t <- as.vector(keys$V3)
x <- strsplit(t, " ")
words2 <- as.data.frame(x)
n <- seq(0, 19, 1)
name <- vector()
for (i in 1:length(n)) {
        name[i] <- paste0("Topic ", n[i])
}
colnames(words2) <- name

write.csv(words2, "bocorpus_tagged_words.csv")

# Composition
goth <- read.csv("bocorpus_tagged_nv_composition20.csv", header = F )
goth <- goth[-1,]
goth$V1 <- seq(1:21)
files <- dir("bocorpus")
goth$V2 <- files

# Prepare data file
goth_topics <- goth[2:3] # add file names and topic number
rownames(goth_topics) <- seq(1:21)
colnames(goth_topics) <- c("Name", "Topic")

goth_topics$Date <- c(1764, 1778, 1786, 1794, 1796, 1798, 1798, 1817, 1818, 
                      1819, 1837, 1838, 1847, 1847, 1860, 1872, 1886, 1892,
                      1893, 1897, 1898)
goth_topics$Gender <- c("M","F","M","F","M","M","F","F","F","M","M","M","F","F",
                        "M","M","M","F","M","M","M")

# Plot Topics by Date and Gender

ggplot(goth_topics, aes(x = Date, y = Topic, color = Gender)) +
        geom_point(size = 3) +
        scale_y_continuous(breaks = seq(0, 19, 1)) +
        scale_x_continuous(breaks = seq(1750, 1900, 25)) +
        ggtitle("Bocorpus Tagged Nouns and Verbs Topics by Gender") +
        ylab("Topic Number") +
        xlab("Date of Publication")
