################################################################################
# File-Name: Text_cleaning.R                                                   #
# Date: 3 November 2017                                                        #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: PhD Thesis                                                          #
# Purpose: Cleaning Owenson files                                              #
# Last Updated: 7 November 2017                                                #
################################################################################

# As all but two of Owenson's novels had to be sourced from Archive.org and 
# rescanned using Abbyy FineReader there are a number of OCR errors which 
# remain - the majority of these are page numbers and random letters used as
# notes for the bookbinders. This script carries out minimal cleaning by 
# removing individual letters (with the exception of a, i, A and I), and
# digits. The files are then resaved.

# To remove random letters and page numbers from Owenson

input.dir <- "so_corpus" 
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt") 

setwd("~/PhD_Main/PhD_Analysis/4_1/so_corpus")

x <- scan(file = files[6], what = "char", 
                  sep = "\n", quote = "", comment.char = "")
# To remove random letters
x <- gsub(" *\\b[^aiAI]{1}\\b *", "", x)

# To remove numbers
x <- gsub("\\d", "", x)

# To remove headers
x <- gsub("DOMINICK", "", x) # Specific capitalised words

# Write cleaned file back to folder
write(x, paste0("clean/", files[2]))
