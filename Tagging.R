#################
## Preparation ##
#################

# Prior to running the script TreeTagger needs to be downloaded to your computer
# it can be downloaded from http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# Instructions for downloading and set up of TreeTagger are on the site.
# If error 'error TreeTagger/lib/english.par not found' appears check the
# TreeTagger 'lib' folder and rename the 'english-utf8.par' file 'english.par'.

####################################
## Reading in and Processing Text ##
####################################

# Install and load required package
# install.packages(koRpus)
library(koRpus)

filename <- list.files("MS/fecorpus/", pattern="*.txt")

tagging <- function(corpus) {
# Read in text and apply POS tagger
text_tagged <- treetag(paste0("MS/", corpus, "/", filename[i]),
                       treetagger="manual", lang="en", 
                       TT.options=list(path="TreeTagger", preset="en"))

# This creates a kRp.tagged file

# Check that the processing has worked
#head(taggedText(text_tagged))

# View the structure of the file
#str(describe(text_tagged))

# View the slot names of the file
#slotNames(text_tagged)

##################################################################
## Extract the Tagged words and create separate files if needed ##
##################################################################

# Extract the words, tags and description
tagged_doc <- text_tagged@TT.res[, c(1,2,6)]

# Check that this has worked
#head(tagged_doc)

# Extract nouns
single_nouns <- subset(tagged_doc, tag == "NN")
plural_nouns <- subset(tagged_doc, tag == "NNS")
# nouns <- rbind(single_nouns, plural_nouns)
# nouns <- nouns$token

# Extract verbs (excluding 'to be', 'to do' and 'to have')
verb1 <- subset(tagged_doc, tag == "VV")
verb2 <- subset(tagged_doc, tag == "VVD")
verb3 <- subset(tagged_doc, tag == "VVG")
verb4 <- subset(tagged_doc, tag == "VVN")
verb5 <- subset(tagged_doc, tag == "VVP")
verb6 <- subset(tagged_doc, tag == "VVZ")
nouns_verbs <- rbind(single_nouns, plural_nouns, verb1, verb2, verb3, verb4, 
                     verb5, verb6)
nouns_verbs <- nouns_verbs$token

# Save noun file as plain text
write(nouns_verbs, file = paste0("MS/Tagged_nv/fecorpus_nv/", filename[i]))

}

for(i in 1:length(filename)) {
        tagging("fecorpus")
}