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
# Last Updated: 8 November 2017                                                #
################################################################################

# Creating a lexi independence collection of terms

setwd("~/PhD_Main/PhD_Analysis/Data/clmet/corpus/txt")

library(wordVectors)

prep_word2vec("plain", "Clmet_processed.txt", lowercase = T)

clmet <- train_word2vec("Clmet_processed.txt", 
                         output = "Clmet_model.bin", threads = 1, 
                         vectors = 100, window = 12)

independence_250 <- closest_to(clmet, "independence", n = 250)
independence_500 <- closest_to(clmet, "independence", n = 500)

ind_250_names <- independence_250$word
ind_500_names <- independence_500$word
