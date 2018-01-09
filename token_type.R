setwd("~/PhD_Main/PhD_Analysis/4_1")
input.dir1 <- "m_corpus"
files1 <- dir(input.dir1, "\\.txt")
leng <- data.frame()
for(i in 1:length(files1)) {
        x <- scan(file = paste0(input.dir1, "/", files1[i]), what = "char",
                  sep = "\n")
        x_p <- tolower(x)
        x_p <- strsplit(x_p, "\\W")
        x_p <- unlist(x_p)
        x_p <- x_p[which(x_p != "")]
        x_l <- length(x_p)
        x_l_u <- length(unique(x_p))
        leng_x <- cbind(x_l, x_l_u)
        leng <- rbind(leng, leng_x)
}

rownames(leng) <- files1
colnames(leng) <- c("Tokens", "Type")

m_tokens <- sum(leng$Tokens)

input.dir2 <- "f_corpus"
files2 <- dir(input.dir2, "\\.txt")
leng2 <- data.frame()

for(i in 1:length(files2)) {
        x <- scan(file = paste0(input.dir2, "/", files2[i]), what = "char",
                  sep = "\n")
        x_p <- tolower(x)
        x_p <- strsplit(x_p, "\\W")
        x_p <- unlist(x_p)
        x_p <- x_p[which(x_p != "")]
        x_l <- length(x_p)
        x_l_u <- length(unique(x_p))
        leng_x <- cbind(x_l, x_l_u)
        leng2 <- rbind(leng2, leng_x)
}

rownames(leng2) <- files2
colnames(leng2) <- c("Tokens", "Type")

f_tokens <- sum(leng2$Tokens)


m_tokens
f_tokens

###############################################################################
# Aside - male and female corpora as a proportion of texts published
nineteen <- data.frame()
year <- c(1800:1820)
male <- c(0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 2)
female <-c(1, 2, 1, 1, 2, 0, 1, 1, 1, 2, 1, 1, 4, 2, 2, 0, 1, 2, 2, 1, 0)
# Total counts from Garside2000
w <- c(39,43,28,33,30,34,37,31,50,37,49,37,33,38,40,24,31,30,30,32,26) 
m <- c(28,18,20,35,35,29,21,31,41,31,22,32,16,19,16,17,15,16,19,19,33)
nineteen <- as.data.frame(cbind(year, male, female, m, w))
colnames(nineteen) <- c("year", "male", "female", "total_m", "total_f")
nineteen$m_percent <- round(100 * (male / m), 2)
nineteen$f_percent <- round(100 *(female/w), 2)

total_male <- sum(male)
total_female <- sum(female)
total_b_m <- sum(m)
total_b_f <- sum(w)

male_pc <- round(100 * (total_male/total_b_m), 2)
female_pc <- round(100 * (total_female/total_b_f), 2)

library(tidyverse)
library(magrittr)
library(ggplot2)

nineteen_tidy <- nineteen %>% gather(gender, count, -year)

head(nineteen_tidy)
tail(nineteen_tidy)


ggplot() +
        geom_bar(data=filter(nineteen_tidy, gender %in% c("male", "female")), 
                 aes(x = year, y = count, fill=gender) , 
                 stat ="identity") +
        geom_point(data=filter(nineteen_tidy, gender %in% c("m_percent", "f_percent")),
                   aes(x = year, y = count, colour=gender)) +
        geom_line(data=filter(nineteen_tidy, gender %in% c("m_percent", "f_percent")), 
                  aes(x = year, y = count, colour=gender, group =gender)) +
        scale_x_continuous(breaks = seq(1800, 1820, 1)) +
        labs(title = "19th Century Corpus - Texts by Year by Gender",
             subtitle = "Percentage of total texts published by gender",
             y = "Number of Texts/Percentage of Texts", x = "Year")
       

###############################################################################
# Type Token All

input.dir <- "all_corpus"
files <- dir(input.dir, "\\.txt")
tt_corpus <- data.frame()
for(i in 1:length(files)) {
        x <- scan(file = paste0(input.dir, "/", files[i]), what = "char",
                  sep = "\n")
        x_p <- tolower(x)
        x_p <- strsplit(x_p, "\\W")
        x_p <- unlist(x_p)
        x_p <- x_p[which(x_p != "")]
        x_l <- length(x_p)
        x_l_u <- length(unique(x_p))
        leng_x <- cbind(x_l, x_l_u)
        tt_corpus <- rbind(tt_corpus, leng_x)
}


colnames(tt_corpus) <- c("Tokens", "Type")



tt_corpus$Text <- gsub("\\.txt", "", files)
tt_corpus$TTR <- round(tt_corpus$Type / tt_corpus$Tokens, 2)

tt_corpus$Year <- gsub("[^0-9]", "", tt_corpus$Text)

x <- sapply(strsplit(as.character(tt_corpus$Text), ""), tail, 1) 
m <- which(x == "M")
f <- which(x == "F")
a <- which(x == "A")
e <- which(x == "E")
o <- which(x == "O")

tt_corpus$Author[m] <- "Male"
tt_corpus$Author[f] <- "Female"
tt_corpus$Author[a] <- "Austen"
tt_corpus$Author[e] <- "Edgeworth"
tt_corpus$Author[o] <- "Owenson"



write.csv(tt_corpus, "4_1_results/TTR_Corpus.csv")


library(ggplot2)
library(RColorBrewer)

# display.brewer.all(n = NULL, type = "all", select = NULL, exact.n = TRUE,
#                  colorblindFriendly = T)

co <- brewer.pal(n = 5, "Set1")

# Plotting TTR by Date and Author



tt_corpus$Author <- factor(tt_corpus$Author)

ggplot(tt_corpus, aes(x = Year, y = TTR, group = Author)) +
        geom_point(aes(shape = Author, fill = Author), colour = "black", size = 2, 
                   position = position_jitter(w = 0.15, h = 0)) +
        theme_bw() +
        theme() +
        ggtitle("Type-Token Ratio for JA, ME, SO and 19C Corpora") +
        labs(x = "Year", y = "Type-Token Ratio", 
             subtitle = "Dotted line = mean TTR, Dashed line = mean male TTR, Dot-dash line = mean female TTR") +
       # theme(legend.position = "bottom", legend.direction = "horizontal") +
        scale_fill_manual(values = co) +
        scale_shape_manual(values = c(21, 21, 25, 24, 21)) +
        geom_hline(yintercept = mean(tt_corpus$TTR) , 
                   colour = "black", linetype = "dotted" ) +
        geom_hline(yintercept = mean(tt_corpus$TTR[which(tt_corpus$Author == "Male")]) , 
                   colour = "#984EA3", linetype = "dashed" ) +
        geom_hline(yintercept = mean(tt_corpus$TTR[which(tt_corpus$Author == "Female")]), 
                   colour = "#4DAF4A", linetype = "dotdash")
        
low <- subset(tt_corpus, tt_corpus$TTR < 0.075)
high <- subset(tt_corpus, tt_corpus$TTR > 0.125)

v_low <- subset(tt_corpus, tt_corpus$TTR < 0.05)
# Create a subset of the data and calculate the variance

aeo <- subset(tt_corpus, Author == "Austen" | Author == "Edgeworth" | 
                      Author == "Owenson")

means<- round(tapply(tt_corpus$TTR, tt_corpus$Author, mean), digits = 2)

boxplot(tt_corpus$TTR ~ tt_corpus$Author, main = "Type-Token Ratio by Author (mean is black dot)",
        xlab = "Author", 
        ylab = "Type-Token Ratio", col = co)
points(means, col = "black", pch = 18)

# Test for homogeneity of variance - two tests
# If test p value is > 0.05 homogeneity of variance is sufficient for use of aov

library(car)
leveneTest(TTR ~ Author, data = tt_corpus)
fligner.test(TTR ~ Author, data = tt_corpus)

tt_aov <- aov(tt_corpus$TTR ~ tt_corpus$Author)

summary(tt_aov)

tuk <- TukeyHSD(tt_aov)

tuk

plot(tuk)

leveneTest(TTR ~ Author, data = aeo)
fligner.test(TTR ~ Author, data = aeo)

aeo_aov <- aov(aeo$TTR ~ aeo$Author)

tuk_aeo <- TukeyHSD(aeo_aov)

plot(tuk_aeo)





