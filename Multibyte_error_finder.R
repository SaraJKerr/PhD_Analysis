################################################################################
# File-Name: Multibyte_error_finder.R                                          #
# Date: 2 November 2017                                                        #
# Author: Sara J Kerr                                                          # 
# ORCID:orcid.org/0000-0002-2322-1178                                          #
# Institution: Maynooth University                                             #
# Project: PhD Thesis                                                          #
# Purpose: Code to find UTF-8 errors in text in response to multibyte error    #
# Source: https://stackoverflow.com/questions/4993837/r-invalid-multibyte-string
#         Answer by Ram Narasimhan                                             #
# Last Updated: 2 November 2017                                                #
################################################################################

# function

findOffendingCharacter <- function(x, maxStringLength=256){  
        print(x)
        for (c in 1:maxStringLength){
                offendingChar <- substr(x,c,c)
                #print(offendingChar) #uncomment if you want the indiv characters printed
                #the next character is the offending multibyte Character
        }    
}

# Read text into R
string_vector <- scan("m_corpus/Shelley_1810_Z_M.txt", what = "character", 
                      sep = "\n")

# Test if file is the source of the error
x <- tolower(string_vector) # checks if text is at fault

# Locate error - rerun after correction to check
lapply(string_vector, findOffendingCharacter) # finds error

