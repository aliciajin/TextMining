#############################
# < Yiqian Jin >
# 04 
# The following code analyzes the federalist papers
#############################

# 1a
library(tm)
# This code uses tm to preprocess the papers into a format useful for NB
preprocess.directory = function(dirname){
	ds = DirSource(dirname)
	fp = Corpus( ds )
	# inspect to verify
	# inspect(fp[1])
	# identical(fp[[1]], fp[["Federalist01.txt"]])
	# iterate through and clean this up using tm functionality
	for (i in 1:length(fp)){
		# make all words lower case
		fp[i] = tm_map( fp[i] , tolower);
		# remove all punctuation
		fp[i] = tm_map( fp[i] , removePunctuation);
		# remove stopwords like the, a, and so on.	
		fp[i] = tm_map( fp[i], removeWords, stopwords("english"));
		# remove stems like suffixes
		fp[i] = tm_map( fp[i], stemDocument)
		# remove extra whitespace
		fp[i] = tm_map( fp[i], stripWhitespace)	
	}
	writeCorpus( fp , sprintf('%s_clean',dirname) )
}
##########################################

# 1b
##########################################
# To read in data from the directories:
# Partially based on code from C. Shalizi
read.directory <- function(dirname) {
    # Store the infiles in a list
    infiles = list();
    # Get a list of filenames in the directory
    filenames = dir(dirname,full.names=TRUE);
    for (i in 1:length(filenames)){
        infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
         }
    return(infiles)
}
##########################################

# 1c
##########################################
make.sorted.dictionary.df <- function(infiles){
    # This returns a dataframe that is sorted by the number of times 
    # a word appears
    
    # List of vectors to one big vetor
    dictionary.full <- unlist(infiles) 
    # Tabulates the full dictionary
    tabulate.dic <- tabulate(factor(dictionary.full)) 
    # Find unique values
    dictionary <- unique(dictionary.full) 
    # Sort them alphabetically
    dictionary <- sort(dictionary)
    dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
    sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
    return(sort.dictionary.df)
}
##########################################

# 1d
##########################################
make.document.term.matrix <- function(infiles,dictionary){
    num.infiles <- length(infiles);
    num.words <- nrow(dictionary);
    # Instantiate a matrix where rows are documents and columns are words
    dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
    for (i in 1:num.infiles){
        num.words.infile <- length(infiles[[i]]);
        infile.temp <- infiles[[i]];
        for (j in 1:num.words.infile){
            ind <- which(dictionary == infile.temp[j])[[1]];
            # print(sprintf('%s,%s', i , ind))
            dtm[i,ind] <- dtm[i,ind] + 1;
        }
    }
return(dtm);
}
##########################################

# 1e
##########################################
make.log.pvec <- function(dtm,mu){
    # Sum up the number of instances per word
    pvec.no.mu <- colSums(dtm)
    # Sum up number of words
    n.words <- sum(pvec.no.mu)
    # Get dictionary size
    dic.len <- length(pvec.no.mu)
    # Incorporate mu and normalize
    log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
    return(log.pvec)
}
##########################################




