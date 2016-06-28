library(RTextTools)                                                                                                                                                                                       
library(quanteda)                                                                                                                                                                                         
library(NLP)                                                                                                                                                                                              
library(tm)                                                                                                                                                                                               

p4kReviews <- readLines("p4k_reviews.csv")                                                                                                                                                                
## extract scores from each review 
scores <- lapply((strsplit(p4kReviews , ",")) , function(x) unlist(x)[2])                                                                                                                                 
scores <- unlist(scores)                                                                                                                                                                                  

## extract text from each review                                                                                                                                                                                                       
text <- lapply((strsplit(p4kReviews , ",")) , function(x) unlist(x)[3])                                                                                                                                   
text <- unlist(text)                                                                                                                                                                                      

median(scores)                                                                                                                                                                                            

## if score is less than median, mark the review as a negative review and conversely, otherwise.                                                                                                                                                                                                            
positiveReviews <- p4kReviews[lapply((strsplit(p4kReviews , ",")) , function(x) unlist(x)[2]) >= 7.1]                                                                                                     
negativeReviews <- p4kReviews[lapply((strsplit(p4kReviews , ",")) , function(x) unlist(x)[2]) < 7.1]                                                                                                      

## finding the 10th and 90th percentile values                                                                                                                                                            
scores <- as.numeric(scores)                                                                                                                                                                              
quantile(scores , c(0.10 , 0.90) , na.rm = TRUE)                                                                                                                                                          

anchorPositive <- p4kReviews[lapply((strsplit(p4kReviews , ",")) , function(x) unlist(x)[2]) <= 5.4]                                                                                                      

anchorNegative <- p4kReviews[lapply((strsplit(p4kReviews , ",")) , function(x) unlist(x)[2]) >= 8.2] 

##################################################################
##################### Vector of Wordscores #######################
##################################################################

aPositiveTexts = c()
aPositiveTexts <- sapply(anchorPositive , function(x) c(aPositiveTexts , x))

aNegativeTexts = c()
aNegativeTexts <- sapply(anchorNegative , function(x) c(aNegativeTexts , x))

aPositiveWords <- tokenize(anchorPositive , removePunct = TRUE )
aNegativeWords <- tokenize(anchorNegative , removePunct = TRUE )

aPW <- dfm(aPositiveWords , ignoredFeatures = stopwords("english"))
aNW <- dfm(aNegativeWords , ignoredFeatures = stopwords("english"))

Fwr <- dfm(aPositiveTexts , aNegativeTexts)




