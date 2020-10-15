
##### AUTHORS:     Rachel Porter
##### DATE:        15 October 2020
##### PURPOSE:     Intro to Text Analysis - Day 2
##### COURSE:      Odum -- Short Course -- Text Analysis

#install.packages("")
library(readr)
library(tm)
library(quanteda)
library(stm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(keyATM)
rm(list=ls())

################# PREPROCESSING STEP  ###############

## load in data on campaign positions 
data <- read_csv("~/Dropbox/Text_Class/odum_text_2020/postions_data.csv")

## remove any tags, unwanted text for analysis
data$trimmed <- gsub("\n", "", data$trimmed)
data$trimmed <- gsub("d5", "", data$trimmed)
data$trimmed <- gsub("d0", "", data$trimmed)
data$trimmed <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", data$trimmed)

## remove numbers, any words that do not begin with a letter
data$trimmed <- gsub('[0-9]+', '', data$trimmed)
data$trimmed <- gsub("[^a-zA-Z]", " ", data$trimmed)

## drop any observations with no text
data <- subset(data, !is.na(data$trimmed))

## create corpus object, set text field
issues <- corpus(data, text_field = "trimmed")

## finish preprocessing steps 
iss_dfm <- dfm(issues, stem = T,
               remove = stopwords('english'), remove_punct = T)

## optional: examine top features
topfeatures(iss_dfm, 100)

## Convert dfm to stm structure 
iss_stm <- convert(iss_dfm, to = 'stm')
plotRemoved(iss_stm$documents, lower.thresh=seq(1,200, by=20))

## Determining drop threshold 
iss_stm <- prepDocuments(iss_stm$documents, iss_stm$vocab,
                         iss_stm$meta, lower.thresh = 5)


#####################################################
            #STRUCTURAL TOPIC MODELS#
#####################################################


################# TOPIC MODELING STEP  ###############

# This is where our researcher determination becomes very important
# the choices we make here will have consequences on our results
# I would suggest replicating your analysis across multiple topic /
# drop thresholds to ensure results are not **model dependent**

# FINDING THE "RIGHT" NUMBER OF TOPICS 
# Per Grimmer and Stewart (2013), there is no surefire way to choose your
# number of topics; but there are data-driven approaches for making this 
# determination. The searchK function allows for the repeated estimation of 
# stms across different k-topic thresholds. These repeated estimations can 
# then be compared across several diagnostic values to determine K. 

model_search <- searchK(iss_stm$documents, iss_stm$vocab, K = c(8:12),
                        prevalence = ~ gender + as.factor(ss_party) + openseat,
                        data = iss_stm$meta, seed = 374075)
plot(model_search)
# Held-out likelihood — the probability of held-out documents given a trained model; goal close to 0
# Residuals           — If the model is correctly specified (if it correctly specified the multinomial 
#                       likelihood the dispersion of residuals sigma2 should be 1; If > 1, too few topics, 
#                       cannot account well for topic over-dispersion
# Lower-bound         — approximation to the lower bound on the marginal likelihood. You can think of 
#                       it as the model's internal measure of fit. Goal is to MAXIMIZE
# Semantic Coherence  — Semantic coherence is maximized when the most probable words in a given topic 
#                       frequently co-occur together. Goal is to MAXIMIZE.

initial_model <- stm(documents = iss_stm$documents, vocab = iss_stm$vocab,
                       K = 9, prevalence = ~ gender + as.factor(ss_party) + openseat,
                       data = iss_stm$meta, init.type = "Spectral")

# What is init.type? The spectral approach utilizes the connection of LDA with non-negative matrix 
# factorization that provides theoretical guarantees that the optimal parameters will be recovered. 
# This overcomes issue of local modes, which may cause differences in results based on changes in 
# starting values (i.e. different seeds set). Spectral initialization should produce more consistent
# results across multiple runs; setting the seed becomes unnecesscary.

labelTopics(initial_model, n = 10)

# Highest Prob.***    — Words with the highest probability of ocurring in a given topic 
# FREX words          — Highest probability words re-weighted by how exclusive they are to a given topic 
# Lift words          — Words divided by their overall frequency in other topics 
# Score words         — Divide log frequency of a word in a topic by log frequency in other topics 

# Plotting overall topic frequency 
plot(initial_model, type = "summary", xlim = c(0, .5))

# Plotting overall topic quality 
plot = topicQuality(initial_model,documents=iss_stm$document)

# Making that plot a little better 
labels <- c("Women's Rights", "Family Leave", "Reproductive Rights", "Poverty", 
            "Body Autonomy", "Discrimination", "Violence", "Healthcare", "Wage Equality")

semcoh <- semanticCoherence(initial_model, documents = iss_stm$documents)
exclusivity <- exclusivity(initial_model)

semco <- data.frame(labels, semcoh, exclusivity)

ggplot(semco, aes(x = semcoh, y = exclusivity, label=labels)) +
  geom_point() +
  geom_text(aes(label=labels),hjust=-.05, vjust=.2) +
  ylim(8,10) +
  xlim(-70,-20) +
  theme_bw() +
  xlab("Semantic Coherence") + ylab("Topic-Word Exclusivity") +
  theme(text = element_text(size=18))


################# TESTING MY HYPOTHESIS  ###############

prep <- estimateEffect(1:9 ~ gender, initial_model, meta = iss_stm$meta)

plot(prep, covariate="gender", c(1:9), 
         model=initial_model, method="difference",
     cov.value1="0", cov.value2="1", ci.level = .90,
     main = "Relationship between Gender & Issue Coverage",
     xlab = "Greater Female Candidate Coverage ... Greater Male Candidate Coverage",
     xlim=c(-.15,.15), labeltype = "custom", custom.labels = labels)


#####################################################
          #KEYWORD ASSISTED TOPIC MODELS#
#####################################################

## Convert dfm to keyATM structure 
keyATM_docs <- keyATM_read(texts = iss_dfm)
summary(keyATM_docs)

## recalling top words across documents
topfeatures(iss_dfm, n = 100)

## specify keywords for topic modeling 
keywords <- list(Healthcare             = c("abort", "contracept", "healthcar", "afford", "access"),
                 Harassment             = c("violenc", "sexual", "abus"))

## visualize keyword occurance across documents; determine strength of keywords 
visualize_keywords(docs = keyATM_docs, keywords = keywords, label_size = 5)

## specify covariate data for modeling 
covariate_data = data[,c("gender", "openseat", "ss_party")]
covariate_data$ss_party <- as.factor(covariate_data$ss_party)

## specifying topic model 
out <- keyATM(docs              = keyATM_docs,
              no_keyword_topics = 3,
              keywords          = keywords,
              model             = "covariates",
              model_settings    = list(covariates_data = covariate_data,
                                       covariates_formula = ~ gender + openseat + ss_party),
              options           = list(seed = 250, iterations = 5000))

## assessing topic content 
top_words(out, n=15)

## examining our hypothesis 
strata_topic <- by_strata_DocTopic(out, by_var = "gender",
                                   labels = c("Male", "Female"))
fig_doctopic <- plot(strata_topic, var_name = "Gender", show_topic = c(1,2))
fig_doctopic
