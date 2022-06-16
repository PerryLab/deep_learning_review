#Deep learning trends and keyword analysis. This script assumes that the deepLearning_corpus R project it is associated with has been opened and that the directory structure has been preserved.

#Search term field for scopus: 

##Environmental science subject area:
# TITLE-ABS-KEY ( ( "deep learn*" AND ecol* ) OR ( "machine learn*" AND ecol* ) OR ( "artificial intelligence" AND ecol* ) ) AND SUBJAREA (envi)

 
## Agriculture subject area:
# TITLE-ABS-KEY ( ( "deep learn*" AND ecol* ) OR ( "machine learn*" AND ecol* ) OR ( "artificial intelligence" AND ecol* ) ) AND SUBJAREA (agri)


# Libraries ---------------------------------------------------------------
#Data manipulation
library(janitor)
library(dplyr) 
library(tidyr)
library(tidytext)

#Keyword network
library(akc)

#Graphing 
library(ggplot2)
library(viridis)

# Data Preparation --------------------------------------------------------

#ARTICLES DOWNLOADED ON 13/10/2021 (12:08 NZT)

#Articles pulled by the comprehensive search of artificial intelligence, machine learning and deep learning in the ecological literature  (environmental science and agriculture subject areas of scopus)

#envi subject area of scopus
aiScopus.df <- read.csv("./data/enviScopus.csv") %>% 
  #Including the papers in the agri subject area
  bind_rows(read.csv("./data/agriScopus.csv")) %>% 
  #Tidying column names for convenience
  clean_names() %>% 
  #Distinct based on title because not all have DOI - removing double ups from agri and envi
  distinct(title, .keep_all = TRUE)

#Just the papers that came up as a result of searching deep learning in the above subject areas  
dlScopus.df <- read.csv("./data/dlScopus.csv") %>%  
  clean_names() %>%
  group_by(year) %>% 
  summarise(dlCount = n()) %>% 
  mutate(dl_nn = dlCount/1) #Normalising to 1 as the first dl paper (according to search by title, keyword and abstract) was published in 2006

#Total count of articles published by scopus by year
allScopus.df <- data.frame(year = 1971:2021, 
                           all_articles = read.csv("./data/allArticles_03062022.csv"), 
                           envi =  read.csv("./data/allEnvArticles.csv")) %>%
  mutate(envi_nn = envi / 55700) #55660 = the number published in 2000


# Trends in ML -----------------------------------------------------------
#Summarising count by year for graph of trends
aiSum.df <- aiScopus.df %>% 
  #Counts by year
  group_by(year) %>% 
  summarise(aiCount = n()) %>% 
  #Merging dataframes for plotting
  left_join(allScopus.df, by = "year") %>% 
  left_join(dlScopus.df, by = "year") %>% 
  #Changing NA's to 0 for plotting
  mutate(dl_nn = replace(dl_nn, is.na(dl_nn), 0)) 
#Find the baseline number to normalise against -CODE NOT WORKING
aiStandard <- filter(aiSum.df, year == 2000) %>%
  select(aiCount) %>% 
  slice(1)

#Manipulating to create normalised column
aiTrends.df <- aiSum.df %>%
  mutate(ai_nn = aiCount/7) %>% #Divide by the standard (year 2000)
  #Pivoting to long format for plotting
  pivot_longer(cols = c(dl_nn, ai_nn, envi_nn), names_to = "article_type", values_to = "article_nn") %>% 
  #Altering for more informative labels
  mutate(article_type = ifelse(article_type == "dl_nn", "Deep Learning", ifelse(article_type == "ai_nn", "Artificial Intelligence", "Environmental Science"))) %>% 
  #Altering ordering on plot
  mutate(article_type = factor(article_type, levels = c("Deep Learning", "Artificial Intelligence", "Environmental Science")))

#Plotting trends through time to visualise how they differ from one another...
ggplot(filter(aiTrends.df, year != 2022), 
       aes(x = year, 
           y = article_nn, 
           colour = article_type)) +
  geom_line(size = 1.15) +
  scale_colour_manual(values = viridis::magma(3, begin = 0.2, end = 0.8, direction = - 1), name = "Article Type") +
  labs(y = "Normalised Article Count",
       x = "Year") +
  xlim(c(1980, 2021)) +
  geom_vline(xintercept = 2000, linetype = "dashed") +
  geom_vline(xintercept = 2006, linetype = "dotted") +
  theme_classic()

ggsave("./graphs/publishingTrends.pdf", width = 7.75, height = 4.5)
ggsave("./graphs/publishingTrends.png", width = 7.75, height = 4.5)


# Keyword analysis (Deep Learning) ----------------------------------------
data(stop_words) #stop words to be removed from scopus database

#Extra words that André chose on the basis of repeat groupings...
sillyWords <- data.frame(word = c("model", "models",  "modelling", "modeling", "learn", "learning", "machine",  "environment", "intelligence", "deep", "result", "method", "â", "1", "study", "factor", "index", "base", "analysis", "artificial", "algorithm", "research", "performance", "approach", "develop", "accuracy", "train", "neural", "process", "provide", "feature", "sample", "dataset", "network", "net", "article", "data set", "guangdong", "china", "south africa", "united states", "sacramento-san joaquin delta", "malaysia", "poland", "brain", "mean square error", "root mean square error", "root mean square errors", "priority journal", "bayesian analysis", "netherlands", "india", "madre de dios", "indicator indicator", "clinical evaluation", "least squares method", "information management", "comparative study", "controlled study", "errors", "support vector machine", "support vector machines", "decision trees", "major clinical study", "lanzhou", "cellular automaton", "light detection and ranging", "correlation", "hubei", "south korea", "three gorges reservoir", "principle component analysis", "beijing [china]", "random forest", "parameter estimation", "emotion"), lexicon = "Andre") %>% 
  bind_rows(stop_words, .) %>% 
  mutate(word = toupper(word))

#Keyword data cleaning
dlKeywords.df <- read.csv("./data/dlScopus.csv") %>%   
  clean_names() %>%
  #Using index_keywords as these are less varied than author ones
  select(c(title, year, index_keywords)) %>% 
  tibble() %>% 
  #Cleaning and separating the keywords
  keyword_clean(id = "title", 
                keyword = "index_keywords",
                lemmatize = FALSE) %>%
  #Making the upper case for uniformity
  mutate(keyword = toupper(keyword)) %>% 
  #Removing unhelpful words
  anti_join(sillyWords, 
            by = c("keyword" = "word")) %>% 
  #Replacing keywords that are synonymous
  mutate(keyword = replace(keyword, keyword == "ECOSYSTEMS", "ECOSYSTEM"),
         keyword = replace(keyword, keyword == "CONVOLUTIONAL NEURAL NETWORKS", "CONVOLUTIONAL NEURAL NETWORK"),
         keyword = replace(keyword, keyword == "CONVOLUTION", "CONVOLUTIONAL NEURAL NETWORK"),
         keyword = replace(keyword, keyword == "ECOSYSTEM SERVICES", "ECOSYSTEM SERVICE"),
         keyword = replace(keyword, keyword == "HUMANS", "HUMAN"),
         keyword = replace(keyword, keyword == "ANIMALS", "ANIMAL"),
         keyword = replace(keyword, keyword == "ANIMALIA", "ANIMAL"),
         keyword = replace(keyword, keyword == "GREENHOUSE GAS", "GREENHOUSE GASES"),
         keyword = replace(keyword, keyword == "PUBLIC ATTITUDE", "PUBLIC OPINION"),
         keyword = replace(keyword, keyword == "FORECASTING METHOD", "FORECASTING"),
         keyword = replace(keyword, keyword == "ACCURACY ASSESSMENT", "PERFORMANCE ASSESSMENT"),
         keyword = replace(keyword, keyword == "RIVER WATER", "WATER"),
         keyword = replace(keyword, keyword %in% c("CHEMICAL OXYGEN DEMAND", "BIOCHEMICAL OXYGEN DEMAND"), "OXYGEN DEMAND"),
         keyword = replace(keyword, keyword %in% c("DEEP NEURAL NETWORKS", "NEURAL NETWORKS", "ARTIFICIAL NEURAL NETWORK", "MACHINE LEARNING", "NEURAL NETWORKS, COMPUTER", "LEARNING NEURAL NETWORKS", "NEURAL NETWORK MODEL", "LEARNING SYSTEMS", "NEURAL-NETWORKS", "MACHINE-LEARNING", "LEARNING ALGORITHMS", "LEARNING MODELS", "ARTIFICIAL INTELLIGENCE", "NEURAL NETWORK METHOD", "RESIDUAL NEURAL NETWORKS"), "DEEP LEARNING")) 

#Graphing
dlKeywords.df %>% 
  keyword_group(id = "id", 
                keyword = "keyword") %>%
  keyword_vis(max_nodes = 10, facet = TRUE) +
  scale_fill_manual(values = viridis::viridis(n = 3, begin = 0.5, end = 1)) +
  scale_alpha_manual(values = 0.3)

#Saving
ggsave("./graphs/keywordNetwork.pdf", width = 15, height = 7.15)
ggsave("./graphs/keywordNetwork.png", width = 15, height = 7.15)
