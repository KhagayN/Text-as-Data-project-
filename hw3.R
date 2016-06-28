require(quanteda, warn.conflicts = FALSE, quietly = TRUE)
library(quantedaData)
library(lda)
library(topicmodels)
library(LDAvis)
library(clue)
library(stm)

setwd("~/Documents/Text_as_Data/HW3/")
data(immigNewsCorpus, package = "quantedaData")

topPapers <- sort(table(immigNewsCorpus[["paperName"]]), decreasing = TRUE)
reducedCorpus <- subset(immigNewsCorpus, paperName %in% names(topPapers)[1:4])
load("custom_stopwords.RData")

#1.b)
reducedCorpusdfm <- dfm(reducedCorpus , ignoredFeatures = custom_stopwords)

#1.c)
reducedCorpusdfm <- trim(reducedCorpusdfm, minCount = 30, minDoc = 20)

#1.d)
SEED <- 2010 
tm <- LDA(reducedCorpusdfm,k = 30, method = "Gibbs", control = list(seed = SEED, 
                                                              iter = 1000))
#1.e)
terms(tm, k = 10)

#1.f)
TM <- list(Gibbs = tm)

doc_topics<-TM[["Gibbs"]]@gamma

## the top topics for articles published by the Daily Mail newspaper 
maxMail <- apply(doc_topics[1:392,], 1, which.max)

## create a histogram type summary to find top topic. 
table(maxMail)

## the top topic for articles published by the Guardian 
maxGuardian <- apply(doc_topics[393:804,], 1, which.max)

table(maxGuardian)

## function created by Kevin Munger to find second most popular topic. 
k = 30 
which.max2<-function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max_2nd = apply(doc_topics, 1, which.max2)
## find 2nd most popular topic in Daily Mail newspaper articles. 
table(unlist(max_2nd)[1:392])

## find 2nd most popular topic in Guardian newspaper articles. 
table(unlist(max_2nd)[393:804])

#1.h
jsonLDA <- createJSON(phi = ldaPOST$terms, 
           theta = ldaPOST$topics, 
           doc.length = ntoken(reducedCorpusdfm), 
           vocab = features(reducedCorpusdfm), 
           term.frequency = colSums(reducedCorpusdfm))

serVis(jsonLDA, out.dir = "visCollLDA", open.browser = TRUE)

#2.a 

SEED <- 2014
tm2 <- LDA(reducedCorpusdfm,k = 30, method = "Gibbs", control = list(seed = SEED, 
                                                                    iter = 1000))
#2.b
## save betas from each topic model

beta1 <- tm@beta
beta2 <- tm2@beta

align <- clue::solve_LSAP(beta1%*%t(beta2), maximum=TRUE)

#2.c
for (i in 1:30) {intersect(terms(tm, k=10)[,i], terms(tm2, k=10)[,i])} 

#2.d.
SEED <- 2015 
tm3 <- LDA(reducedCorpusdfm,k = 10, method = "Gibbs", control = list(seed = SEED, 
                                                                     iter = 1000))

SEED <- 2014 
tm4 <- LDA(reducedCorpusdfm,k = 10, method = "Gibbs", control = list(seed = SEED, 
                                                              iter = 1000))

for (i in 1:10) {intersect(terms(tm3, k=10)[,i], terms(tm4, k=10)[,i])} 

#3.a. 
dailyMaildates <- reducedCorpus$documents$day[1249:1660]
guardiandates <- reducedCorpus$documents$day[533:921]

dates <- c(dailyMaildates, guardiandates)

dates <- as.Date(as.numeric(dates), origin = "2013-01-01")
## Jan. 14 is day 1. 

#3.b. 

dailyMailPapers <- immigNewsCorpus$documents[1249:1660, 1]
guardianPapers <- immigNewsCorpus$documents[533:921,1]
papers <- c(dailyMailPapers, guardianPapers)
stemPapers <- textProcessor(papers, stem = TRUE)

## article labels 
paperz <- rep("Daily Mail", 389)
paperz[390:801] <- rep("Guardian", 412)

data <- data.frame()

out_21 <- prepDocuments(stemPapers$documents, stemPapers$vocab, stemPapers$meta, lower.thresh = 20)
## daily mail newspapers first then the guardian. 
fitSpec0 <- stm(out_21$documents ,out_21$vocab, K=0, init.type="Spectral", 
                content = ~paperz, 
                prevalence = ~paperz + as.numeric(dates), max.em.its = 30)


##4.a.

mydfm <- dfm(subset(inaugCorpus, Year >1900))

##4.b.
## anchoring two documents as the extremes. 
obama2009 <- inaugCorpus$documents$texts[56]
nixon2006 <- inaugCorpus$documents$texts[46]

## get all the speeches and all the years they were made. 
docs <- inaugCorpus$documents[1]
docs<- apply(docs, 1, function(x) paste(x, collapse =" "))
year <- inaugCorpus$documents$Year

aug_dfm <- data.frame(year = as.numeric(year), stringsAsFactors = TRUE)
aug_dfm$text <- docs

## create dfm from those speeches. 
rep_dem_dfm <- dfm(aug_dfm$text, stem=T, ignoredFeatures = stopwords("english"))


aug_fit <- textmodel_wordfish(rep_dem_dfm, c(46,56))

## 4.d.

which(aug_fit@features == "fascism")
aug_fit@beta[5051] # -2.747111

## 4.e.
words <- aug_fit@psi
names(words) <- aug_fit@features

weights <- aug_fit@beta

plot(weights, words)




