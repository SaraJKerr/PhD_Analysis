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
# Last Updated: 24 November 2017                                                #
################################################################################

# Creating a lexicon of independence terms

setwd("~/PhD_Main/PhD_Analysis/Data/clmet/corpus/txt")

library(wordVectors)

prep_word2vec("plain", "Clmet_processed_period_1_2.txt", lowercase = T)

clmet <- train_word2vec("Clmet_processed_period_1_2.txt", 
                         output = "Clmet_model_period_1_2.bin", threads = 1, 
                          vectors = 300, window = 12)

clmet <- read.vectors("Clmet_model_period_1_2.bin")

# independence_250 <- closest_to(clmet, "independence", n = 250)
independence_500 <- closest_to(clmet, "independence", n = 500)

head(independence_500)

ind_wealth <- closest_to(clmet, ~ "independence" + "wealth", n = 100)

head(ind_wealth)

ind_status <- closest_to(clmet, ~ "independence" + "status", n = 100)

head(ind_status)

ind_thought <- closest_to(clmet, ~ "independence" + "thought", n = 100)

head(ind_thought)

ind_self <- closest_to(clmet, ~ "independence" + "self", n = 100)

head(ind_self)

ind_marriage <- closest_to(clmet, ~ "independence" + "marriage", n = 100)

head(ind_marriage)

ind_nation <- closest_to(clmet, ~ "independence" + "nation", n = 100)

head(ind_nation)

ind_500_names <- independence_500$word

