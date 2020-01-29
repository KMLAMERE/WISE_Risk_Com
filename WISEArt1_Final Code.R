###########################################################################################
###########################################################################################
#------------------------------------------------------------------------------------------
#TOPIC MODELLING CODE FOR "Bridging Perspectives: Risk Communication within Interdisciplinary Teams" 
#by Kelsey LaMere, Annukka M. Lehikoinen, Arho Toikka, Jussi T. Eronen, and Sakari Kuikka
#------------------------------------------------------------------------------------------

#### KL: Stm WorkFlow and Code From Teaching Link Below  
#### KL: https://github.com/dondealban/learning-stm/blob/master/stm-test-workflow.R


###########################################################################################
###########################################################################################

# ----------------------------------------
# INSTALL PACKAGES
# ----------------------------------------
install.packages('stm')
install.packages("RTextTools")
install.packages("readr")
install.packages("wordcloud")
install.packages("topicmodels")
install.packages("tm")
install.packages("huge")
install.packages("tidyr")
install.packages("igraph")
install.packages("stmCorrViz")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")



# ----------------------------------------
# LOAD LIBRARIES
# ----------------------------------------

library(stm)
library(RTextTools)
library(readr)
library(wordcloud)
library(topicmodels)
library(tm)
library(huge)
library(tidyr)
library(igraph)
library(stmCorrViz)
library(ggplot2)
library(dplyr)
library(tidyverse)

# ----------------------------------------
# LOAD DATA
# ----------------------------------------

##### KL: Import all texts into R (as .txt files)

Amara <- read_file("Amara_Ahv.txt")

Auyero <- read_file("Auyero_Qui.txt")

Aven <- read_file("Aven_Ahv.txt")

Burgman <- read_file("Burgman_Lehi.txt")

Carey <-read_file("Carey_Maj.txt")

Casti <-read_file("Casti_Wil.txt")

Clark <-read_file("Clark_Kui.txt")

CNA <-read_file("CNA_Ero.txt")

Dietz <-read_file("Dietz_Lehi.txt")

EASAC <-read_file("EASAC_Lun.txt")

Ewald <- read_file("Ewald_Leh.txt")

Finucane <-read_file("Finucane_Kaa.txt")

Gardiner <-read_file("Gardiner_Lah.txt")

Gigerenzer <-read_file("Gigerenzer_Jan.txt")

Gordon <-read_file("Gordon_Pan.txt")

Gregory <-read_file("Gregory_Lam.txt")

Haila <-read_file("Haila_Lahd.txt")

Hakala <-read_file("Hakala_Maj.txt")

Holappa <-read_file("Holappa_Jar.txt")

Horton <-read_file("Horton_Lah.txt")

Ilmola <-read_file("Ilmola_Ahv.txt")

Japp <- read_file("Japp_Vir.txt")

Jonsson <-read_file("Jonsson_Lam.txt")

Lehikoinen <-read_file("Lehikoinen_Lam.txt")

Linss <-read_file("Linss_Pan.txt")

Luhmann <-read_file("Luhmann_Leh.txt")

Marewski <-read_file("Marewski_Kaa.txt")

Newbold <-read_file("Newbold_Ero.txt")

Omalley <-read_file("Omalley_Leh.txt")

Ostrom <-read_file("Ostrom_Toi.txt")

Paxton <-read_file("Paxton_Qui.txt")

Placani <-read_file("Placani_Lah.txt")

Raftery <-read_file("Raftery_Ero.txt")

Rhamstorf <-read_file("Rhamstorf_Ero.txt")

Roe <-read_file("Roe_Huk.txt") 

Rogelij <-read_file("Rogelij_Lun.txt")

Sawyer <-read_file("Sawyer_Qui.txt")

Shove <-read_file("Shove_Jar.txt") 

Skinner <-read_file("Skinner_Lehi.txt")

Slovic <-read_file("Slovic_Kaa.txt") 

Spiegelhalter <-read_file("Spiegelhalter_Kui.txt")

Stirling_Janasik <-read_file("Stirling_Jan.txt")

Stirling_Hukkinen <-read_file("Stirling_Huk.txt")

VanNotten <-read_file("VanNotten_Ahv.txt")

Varis <-read_file("Varis_Kui.txt") 

Weimer <-read_file("Weimer_Pan.txt")

Werrell <-read_file("Werrell_Lahd.txt") 

Werrell_Eronen <-read_file("Werrell_Ero.txt")

Wilenius <-read_file("Wilenius_Ahv.txt") 

WEF <-read_file("WEF_Maj.txt")

Xie <-read_file("Xie_Lun.txt")

Zografos <-read_file("Zografos_Lahd.txt")

#### KL: Create a vector of all texts (52 total)

Texts <- c(Amara, Auyero, Aven, Burgman, Carey, Casti, Clark, CNA, Dietz, EASAC, Ewald, Finucane, 
           Gardiner, Gigerenzer, Gordon, Gregory, Haila, Hakala, Holappa, Horton, Ilmola, Japp, Jonsson, Lehikoinen, Linss, 
           Luhmann, Marewski, Newbold, Omalley, Ostrom, Paxton, Placani, Raftery, Rhamstorf, Roe, Rogelij, Sawyer, Shove, 
           Skinner, Slovic, Spiegelhalter, Stirling_Janasik, Stirling_Hukkinen, VanNotten, Varis, Weimer, Werrell, 
           Werrell_Eronen, Wilenius, WEF, Xie, Zografos)

#### KL: Create vectors containing metadata (article's 1st author (Author), publication year (Year), 
######### contributing WISE member pseudonym (Alias), and WISE members' disciplines

# KL: Author - First Author of text
Author <- read_csv("Author.csv")

# KL: Year - Year Published
Year <- read_csv("Year.csv")

#KL: Alias - alias title (discipline + #) for each contributing WISE member

#Alias <-read_csv("Alias.csv")

Contributor <-read_csv("Contributor.csv")

#KL: Discipline - Discipline of contributing WISE members 

Discipline <- read_csv("Contributor_Disciplines.csv")

#### KL: Create a data frame containing meta data and texts (Texts) 

article_df <- data.frame(Author, Year, Discipline, Contributor, Texts)

#### KL: Check dataframe to make sure everything looks ok before going further 

# article_df
# head(article_df) 

# ----------------------------------------
# PREPARE AND PRE-PROCESS TEXTS
# ----------------------------------------

#### KL: Custom Stop word list - list of words that should be removed from the texts before running the topic model. 
######### These words were removed based on discussion amongst the authroship team, which found them to be overly common and lacking value for analysis

CustStopWords <- c("can", "also", "may", "well", "one", "two", "three", "four", "five", "first", "second", "third", "fourth", 
                   "fifth", "e", "Will", "way", "rather", "like", "thus", "therefore", "figure", 
                   "table", "example", "since", "fig", "however", "within", "many", "Many", "use", "used", "let's", "'re", "'ve", "'ll", "'re",
                   "'ve", "'ll", "risk", "risks", "Risk", "Risks", "even", "just")


#### KL: Use textProcessor function to automatically remove a) punctuation; b) stop words; c) numbers, d) convert to lowercase, 
######### e) include words if over 3 characters long, anf f) remove custom stop words (CustStopWords)


processed_df <- textProcessor(article_df$Texts, metadata = article_df, lowercase = TRUE,
                              removestopwords = TRUE, removenumbers = TRUE, removepunctuation = TRUE,
                              stem = FALSE, wordLengths = c(3, Inf), sparselevel = 1,
                              language = "en", verbose = TRUE, onlycharacter = FALSE,
                              striphtml = FALSE, customstopwords = CustStopWords, v1 = FALSE)

#### KL: Structure and index for usage in the STM model. Ensure that object has no missing
####### values. Remove low frequency words using 'lower.thresh' option. See ?prepDocuments 
####### for more information.

out <- prepDocuments(processed_df$documents, processed_df$vocab, processed_df$meta, lower.thresh = 2, upper.thresh = 52, verbose = TRUE)

# The output includes object meta, documents, and vocab 

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

##### Check how many words and documents are removed at different 
###### lower.thresholds. Save plot as pdf.

#pdf("stm-plot-removed2.pdf", width=10, height=8.5)
#plotRemoved(processed_df$documents, lower.thresh=seq(1,52, by=1))
#dev.off()

# ----------------------------------------
# ESTIMATE THE STRUCTURAL TOPIC MODEL (STM)
# ----------------------------------------

#### KL: Set seed equal to a large prime number. Use a large prime random generator. 

First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 8, prevalence = ~NULL,
                 max.em.its = 500, data = out$meta, seed = 1299709,
                 init.type = "Spectral", verbose = FALSE)


#### KL: Check that the model converges & save plot as a PDF

windows()
pdf("ConvergencePlot.pdf")

plot(First_STM$convergence$bound, type = "l",
        ylab = "Approximate Objective",
        main = "Convergence")

dev.off()


plot(First_STM$convergence$bound, type = "l",
     col = "forest green",
     xlab = "Index",
     ylab = "Approximate Objective",
     main = "Convergence")

#-------------------------------------------------
# DETERIMINE the NUMBER OF TOPICS (K VALUE)
#-------------------------------------------------

##### KL: Determine the best K value defined by exclusivity and semantic 
##### KL: coherence.Save plot as pdf file.
##### KL: Use this approach coupled with common sense, check that the topics produced 
##### KL: by models with differnt K values and deermine which produces the most reasonable and understandbale topics 

kResult <- searchK(out$documents, out$vocab, K=c(2:50), prevalence=NULL,
                   data=meta)
pdf("stm-plot-searchk_Final2.pdf", width=10, height=8.5)
plot(kResult)
dev.off()

# ------------------------------------------------
# KL: EXCLUSIVITY VS. SEMANTIC COHERANCE PLOT
# ------------------------------------------------

### KL: Code from Julia Silge's Blog: https://juliasilge.com/blog/evaluating-stm/ 

kResultSel<- kResult$results
pdf("stm_exclusVSsemcoh4_10_Final_test9.pdf", width=7, height=7)
kResultSel %>%
  select(K, exclus, semcoh) %>%
  filter(K %in% c(2:50 )) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence, but lower exclusivity")

dev.off()

# ----------------------------------------
# VIZUALIZE & INTERPRET STM 
# ----------------------------------------


##### KL: Plot the STM and save as PDFs

pdf("stm-plot-FirstSTM-basic8_Final.pdf", width=10, height=8.5)
plot(First_STM)
dev.off()

pdf("stm-plot-FirstSTM-labels_Final.pdf", width=10, height=8.5)
plot(First_STM, type="labels", topics=c(1:8))
dev.off()

pdf("stm-plot-FirstSTM-perspectives-two-topic_Final.pdf", width=14, height=12.5)
plot(First_STM, type="perspectives", topics=c(1,6))
dev.off()

#### KL: Use labelTopics() to find common and exclusive terms for each topic 

labelTopicsSel <- labelTopics(First_STM, c(1:8))
sink("stm-list-label-topics-selected_Final.txt", append=FALSE, split=TRUE)
print(labelTopicsSel)
sink()


#### KL:  findThoughts().
# Read documents that are highly correlated with the user-specified topics using the 
# findThoughts() function. Object 'thoughts1' contains 3 documents about topic 1 and
# 'texts=shortdoc' gives just the first 250 words. 


### KL: ERROR##### FIND THOUGHTS NOT WORKING #######

thoughts <- findThoughts(First_STM, texts=Texts, n=3)
thoughtsfound <- thoughts$index
sink("Find_thought_Final.txt", append=FALSE, split=TRUE)
print(thoughtsfound)
sink()

# estimateEffect().
# Explore how prevalence of topics varies across documents according to document
# covariates (metadata). First, users must specify the variable that they wish to use 
# for calculating an effect. If there are multiple variables specified in 
# estimateEffect(), then all other variables are held at their sample median. These 
# parameters include the expected proportion of a document that belongs to a topic as
# a function of a covariate, or a first difference type estimate, where topic prevalence
# for a particular topic is contrasted for two groups (e.g., liberal versus conservative).

predict_topics<-estimateEffect(formula = 1:8 ~ Contributor, stmobj = First_STM, metadata = out$meta, uncertainty = "Global")

#pdf("stm-plot-estimate-effect-categorical1_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(1),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

#pdf("stm-plot-estimate-effect-categorical2_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(2),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

#pdf("stm-plot-estimate-effect-categorical3_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(3),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

#pdf("stm-plot-estimate-effect-categorical4_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(4),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

#pdf("stm-plot-estimate-effect-categorical5_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(5),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

#pdf("stm-plot-estimate-effect-categorical6_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(6),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

#pdf("stm-plot-estimate-effect-categorical7_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(7),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

#pdf("stm-plot-estimate-effect-categorical8_Final.pdf", width=10, height=8.5)
windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(8),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
#dev.off()

windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(9),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)

windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(10),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)

windows()
plot(predict_topics, covariate = "Contributor", model = NULL, topics = c(11),
     method = c("pointestimate"),
     cov.value1 = NULL, cov.value2 = NULL, moderator = NULL,
     moderator.value = NULL, npoints = 100, nsims = 100, ci.level = 0.95,
     xlim = NULL, ylim = NULL, xlab = "", ylab = NULL, main = "",
     printlegend = T, labeltype = "numbers", n = 7, frexw = 0.5, add = F,
     linecol = NULL, width = 25, verbose.labels = T, family = NULL,
     custom.labels = NULL, omit.plot = FALSE)
# ----------------------------------------
# VISUALISE &  PRESENT STMs RESULTS
# ----------------------------------------

# KL: WORD CLOUD.

pdf("FirstSTM-wordcloud_All_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=NULL)
dev.off()

pdf("FirstSTM-wordcloud_1_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=1)
dev.off()

#pdf("FirstSTM-wordcloud_2_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=2)
#dev.off()

pdf("FirstSTM-wordcloud_3_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=3)
dev.off()

pdf("FirstSTM-wordcloud_4_Final.pdf", width=10, height=8.5)
stm::cloud(First_STM, topic=4)
dev.off()

#pdf("FirstSTM-wordcloud_5_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=5)
#dev.off()

#pdf("FirstSTM-wordcloud_6_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=6)
#dev.off()

#pdf("FirstSTM-wordcloud_7_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=7)
#dev.off()

#pdf("FirstSTM-wordcloud_8_Final.pdf", width=10, height=8.5)
windows()
stm::cloud(First_STM, topic=8)

windows()
stm::cloud(First_STM, topic=9)
windows()
stm::cloud(First_STM, topic=10)
windows()
stm::cloud(First_STM, topic=11)

#dev.off()

#### Arho's code

library(broom)
library(tidytext)
library(tidyr)
library(dplyr)
library(igraph)
#install.packages('pander')
library(pander)

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

# Build network and handle data
# takes the topic proportions by document and calculates per contributor distributions
#doc_to_topic_distributions <- tidy(First_STM, matrix="gamma", document_names = metadata_combined$Texts)
#doc_to_topic_distributions_wide <- spread(doc_to_topic_distributions, topic, gamma)
#metadata_arrange <- arrange(metadata_combined, Texts)
#doc_to_topic_distributions_wide$Contributor <- metadata_arrange$Contributor
# summarize & drop first column which has the document titles
#contributor_matrix <- doc_to_topic_distributions_wide[-1] %>% group_by(Contributor) %>% summarise_all(funs(mean))

# KL: Trying to get this code to work 
# Build network and handle data
# takes the topic proportions by document and calculates per contributor distributions
doc_to_topic_distributions <- tidy(First_STM, matrix="gamma", document_names = meta$Texts)
doc_to_topic_distributions_wide <- spread(doc_to_topic_distributions, topic, gamma)
metadata_arrange <- arrange(meta, Texts)
doc_to_topic_distributions_wide$Alias <- metadata_arrange$Alias
# summarize & drop first column which has the document titles
Alias_matrix <- doc_to_topic_distributions_wide[-1] %>% group_by(Alias) %>% summarise_all(funs(mean))






# change <1% topic use to zero
Alias_matrix[Alias_matrix<0.01] <- 0

# make a two-mode contributor to topic network (dropping first column which has the names)
twomodenetwork <- graph_from_incidence_matrix(Alias_matrix[-1], weighted=T)

# add vertices names (contributor name + topicN)
#V(twomodenetwork)$name <- c(contributor_matrix$Contributor, c(paste("Topic", seq(1:8), sep="")))
#V(twomodenetwork)$Discipline_names <- c(Discipline$Disciplines, c(paste("Topic", seq(1:8), sep="")))
#V(twomodenetwork)$Discipline_color <- c(Discipline$Disciplines, rep("Topic", 8))
#V(twomodenetwork)$Discplnes_shortened <- c(Discipline$Discplnes_shortened, c(paste("Topic", seq(1:8), sep="")))

#KL: More bug fixing attempts
# add vertices names (contributor name + topicN)
V(twomodenetwork)$name <- c(Alias_matrix$Alias, c(paste("Topic", seq(1:8), sep="")))
V(twomodenetwork)$Discipline_names <- c(Discipline$Disciplines, c(paste("Topic", seq(1:8), sep="")))
V(twomodenetwork)$Discipline_color <- c(Discipline$Discipline, rep("Topic", 8))
V(twomodenetwork)$Discplnes_shortened <- c(Discipline$Discplnes_shortened, c(paste("Topic", seq(1:8), sep="")))

# calculate topic prevalence
topic_doc_dt <- First_STM %>% make.dt() %>% t() 
topicProportions <- topic_doc_dt[-1,] %>% rowMeans(topic_doc_dt[-1,]) %>% round(4)
topicProportions_ordered <-  topicProportions %>% sort(decreasing = T)
topicOrder <- c(2,6,5,4,1,3,8,7)

V(twomodenetwork)$topicprevalance <- c(rep(0, 19), topicProportions)

# calcu
topic_and_term_distributions <- tidy(First_STM, matrix="beta")
top10_by_topic <- topic_and_term_distributions %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic_renumbered_by_prevalence = topic %>% recode("1" = "5", "2"="2", "3"="6", "4"="4",
                                                           "5"="7", "6"="3", "7"="1", "8"="8")) %>%
  arrange(topic_renumbered_by_prevalence)

topic_titles <- c(paste(names(topicProportions), ": ", topicProportions*100, "%", " of corpus", sep=""))
names(topic_titles) <- c(seq(1:8))

ggplot(top10_by_topic, aes(reorder_within(term, beta, topic_renumbered_by_prevalence), beta)) +
  geom_col() +
  facet_wrap(~ topic_renumbered_by_prevalence, scales = "free", nrow=4, ncol=2, labeller=as_labeller(topic_titles)) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) + 
  labs(
    x = "Term in topic",
    y = "Probability of term in topic",
    title = "Fig x. Ten most common terms per topic",
    caption = "Note: the probability scale varies by topic (as some topics are more focused in terminology). TopicX should be changed to final label.")

panderOptions('digits' , 2)
pander(Alias_matrix)

# two-mode network with person names for checking 
library(ggraph)
set.seed(430) 
ggraph(twomodenetwork, layout="fr") + geom_edge_link(aes(edge_width=weight), alpha = 0.1) + 
  geom_node_label(aes(label=name, fill=Discipline_color, size=topicprevalance), repel=T) +
  guides(color="none", size="none", shape="none", edge_width="none") +
  scale_size(range = c(3, 11)) +
  labs(title = "Network with names for sanity check",
       subtitle="Color = discipline, size = topic prevalence (0 for persons), edge width = % of topic used by person", 
       x="", y="") + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())

# two mode network with node labels
install.packages('ggthemes')
library(ggthemes)

set.seed(430)
ggraph(twomodenetwork, layout="fr") +
  geom_edge_arc(aes(edge_width=weight), alpha = 0.07) + 
  geom_node_label(aes(label=Discplnes_shortened, fill=Discipline_color, size=topicprevalance), repel=T) +
  guides(size="none", shape="none", edge_width="none") + 
  ggthemes::scale_colour_tableau() +
  scale_size(range = c(3, 11)) + scale_edge_width_continuous(range=c(1,3)) +
  labs(title = "Network with discipline labels",
       subtitle="Color = discipline, size = topic prevalence (0 for persons), edge width = % of topic used by person",
       x="", y="") + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())

# two-mode netwrok with node points
set.seed(430)
ggraph(twomodenetwork, layout="fr") +
  geom_node_point(aes(fill=Discipline_color, size=topicprevalance), colour="black", pch=21) +
  geom_edge_arc(aes(edge_width=weight), alpha = 0.07) + 
  geom_node_label(aes(label=Discplnes_shortened, fill=Discipline_color), repel=T) +
  guides(size="none", shape="none", edge_width="none") + 
  ggthemes::scale_colour_tableau() +
  scale_size(range = c(1, 12)) + scale_edge_width_continuous(range=c(1,3)) +
  labs(title = "Network with discipline labels and circles",
       subtitle="Color = discipline, size of circle = topic prevalence (0 for persons), edge width = % of topic used by person",
       x="", y="") + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())

#### Useful tidbits 
#article_df$Author[]
#saveRDS(meta, file = ".rds", ascii = FALSE, version = NULL,
compress = TRUE, refhook = NULL)

#### Saving as RDS 
saveRDS(out, file = "out16.1.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(docs, file = "docs_out16.1.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(vocab, file = "vocab_out16.1.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(meta, file = "meta_out16.1.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(First_STM, file = "First_STM16.1.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
