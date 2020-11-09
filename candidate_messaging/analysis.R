# Some analysis on candidate messaging against outcome

library(rjson)
library(tidyr)
library(tibble)
library(stringr)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(tidytext)
library(glmnet)
library(ROCR)
library(wordcloud)

# election results data from https://www.wsj.com/election-results-2020-data/house.json
election_results_json <- fromJSON(file = "/home/jason/candidate_messaging/data/election_results.json")
print(election_results_json$meta$wsjTimestamp)
election_results <- enframe(unlist(election_results_json$data))
election_results <- separate(data = election_results, col = name, into = c("district", "variable"), sep = "\\.", extra='merge')
election_results_wide <- election_results %>%
  dplyr::filter(variable %in% c('cand.party','cand.votes')) %>%
  dplyr::mutate(party = as.character(dplyr::lag(value))) %>%
  dplyr::filter(variable == 'cand.votes')

# look for cases with more than 1 dem or rep
gops <- election_results_wide %>% 
  filter(party %in% c('gop')) %>% 
  dplyr::group_by(district) %>% 
  summarise(n=n()) %>% 
  filter(n>1) %>%
  pull(district)

dems <- election_results_wide %>% 
  filter(party %in% c('dem')) %>% 
  dplyr::group_by(district) %>% 
  summarise(n=n()) %>% 
  filter(n>1) %>%
  pull(district)

election_results_final <- election_results_wide %>%
  filter(!(district %in% c(dems,gops))) %>%
  filter(party %in% c('gop','dem')) %>%
  spread(party,value,fill=0) %>%
  mutate(dem=as.numeric(dem),
         gop=as.numeric(gop)) %>%
  mutate(total = dem+gop,
         dem_2_way = dem/(dem+gop),
         rep_2_way = gop/(dem+gop)) %>%
  filter(total>0)

# 2018 election results
# from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IG0UN2
historic <- read.csv('/home/jason/candidate_messaging/data/1976-2018-house2.csv')
historic <- historic %>%
  filter(year==2018) %>%
  filter(party %in% c('democrat','republican')) %>%
  mutate(district = paste0(state_po,'-',district)) %>%
  select(district,party,candidatevotes)

# look for cases with more than 1 dem or rep
gops <- historic %>% 
  filter(party %in% c('republican')) %>% 
  dplyr::group_by(district) %>% 
  summarise(n=n()) %>% 
  filter(n>1) %>%
  pull(district)

dems <- historic %>% 
  filter(party %in% c('democrat')) %>% 
  dplyr::group_by(district) %>% 
  summarise(n=n()) %>% 
  filter(n>1) %>%
  pull(district)

historic_final <- historic %>%
  filter(!(district %in% c(dems,gops))) %>%
  spread(party,candidatevotes,fill=0) %>%
  mutate(dem=as.numeric(democrat),
         gop=as.numeric(republican)) %>%
  mutate(total_2018 = dem+gop,
         dem_2_way_2018 = dem/(dem+gop),
         rep_2_way_2018 = gop/(dem+gop)) %>%
  filter(total_2018>0) %>%
  select(district,total_2018,dem_2_way_2018,rep_2_way_2018) %>%
  mutate(district=str_replace(district,'-0','-1'))

# Combine to get shift!
combined_results <- election_results_final %>%
  left_join(historic_final, 'district') %>%
  mutate(dem_delta = dem_2_way - dem_2_way_2018)

# scatterplot 2020 vs 2018
combined_results %>%
  filter(!(dem_2_way %in% c(0,1))) %>%
  filter(!(dem_2_way_2018 %in% c(0,1))) %>%
ggplot(aes(x=dem_2_way_2018,y=dem_2_way)) +
  geom_point(alpha=0.5,size=2) +
  geom_smooth(method='lm') +
  theme_minimal() +
  geom_abline(slope=1,intercept=0) +
  xlab('Dem 2-way Vote Fraction 2018') + 
  ylab('Dem 2-way Vote Fraction 2020') 

# histogram
combined_results %>%
  filter(!(dem_2_way %in% c(0,1))) %>%
  filter(!(dem_2_way_2018 %in% c(0,1))) %>%
  ggplot() +
  theme_minimal() +
  geom_histogram(aes(x=dem_delta),bins=50) +
  xlab('Dem 2 way Delta (2020 - 2018)') +
  ylab('')

# outliers are:
# dem outperformance in NC-6 & NC-2 in both cases, replacing a retiring Rep incumbent
# However - redistricting is likely a factor in both cases

combined_results %>%
  filter(!(dem_2_way %in% c(0,1))) %>%
  filter(!(dem_2_way_2018 %in% c(0,1))) %>%
  ggplot(aes(x=dem_2_way,y=dem_delta)) +
  geom_point(alpha=0.5,size=2) +
  geom_smooth(method='lm') +
  theme_minimal() +
  xlab('Dem 2-way Vote Fraction 2020') + 
  ylab('Dem 2 way Delta Vote Fraction(2020 - 2018)') 


cleaned_results <- combined_results %>%
  filter(!(dem_2_way %in% c(0,1))) %>%
  filter(!(dem_2_way_2018 %in% c(0,1))) %>%
  filter(!(district %in% c('NC-6','NC-2'))) %>%
  filter(!(is.na(dem_delta)))


# Read in tweet data
# useful examples-
# https://cfss.uchicago.edu/notes/supervised-text-classification/
# https://juliasilge.com/blog/tidy-text-classification/
tweet_dir <- "/home/jason/candidate_messaging/raw/"
tweets <- list.files(path = tweet_dir, pattern = "*.csv") %>% 
  map(function(x) {
    read.csv(paste0(tweet_dir, x))
  }) %>%
  reduce(rbind) %>%
  as_tibble()

# Need to filter on validity here - otherwise end up upweighting rare terms
candidate_connection <- read.csv('/home/jason/candidate_messaging/data/active_accounts.csv')
# accounts associated with a race we have data for
valid_accounts <- candidate_connection  %>%
  mutate(district = paste0(state,'-', district)) %>%
  as_tibble() %>%
  inner_join(cleaned_results,by="district") %>%
  pull(user_name)

# remove eday+
tweets_processed <- tweets %>%
  mutate(Date = as.Date(paste(substr(tweets$Date,5,10), '2020'), format = "%b %d %Y")) %>%
  filter(Date < '2020-11-03') %>%
  filter(SN %in% valid_accounts) %>%
  mutate(Text = as.character(Text))

n <- nrow(tweets_processed)
  
tokenized <- tweets_processed %>%
  unnest_tokens(output = word, input = Text) %>%
  # remove numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # stem the words - this may be right, but it leads to confusing output...
  # mutate(word = SnowballC::wordStem(word)) %>%
  # remove uncommon words
  group_by(word) %>%
  filter(n() > n*0.001) %>%
  # remove other junk
  filter(!(word %in% c("aren't","it's","i'm","https","amp","t.co","rt","http"))) %>%
  ungroup()

# 87 words if 1% filter. 1.3k words at 0.1% filter
word_count <- tokenized %>% group_by(word) %>% summarise(count=n()) %>% arrange(desc(count))

wordcloud(words = word_count$word,
          freq = word_count$count,
          min.freq = 1,
          max.words=200,
          random.order=FALSE,
          rot.per=0.35,
          scale=c(3,0.2),
          colors=brewer.pal(8, "Dark2"))


# create modeling data - join everything together!
combined_data <- candidate_connection  %>%
  mutate(district = paste0(state,'-', district)) %>%
  as_tibble() %>%
  inner_join(tokenized,by=c("user_name"="SN")) %>%
  inner_join(cleaned_results,by="district") %>%
  select(user_name, office_title, district,word,dem_delta) %>%
  mutate(user_name=as.character(user_name))

modeling_data <- combined_data %>%
  select(user_name, word) %>%
  count(user_name, word) %>%
  cast_sparse(user_name, word, n)

outcome <- tibble(user_name = rownames(modeling_data)) %>%
  inner_join(combined_data %>% select(user_name, dem_delta) %>% distinct(),"user_name")

nrow(modeling_data) == nrow(outcome)

# fit lasso model
model <- cv.glmnet(modeling_data, outcome$dem_delta, family = "gaussian")

plot(model)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.min) %>%
  filter(term != '(Intercept)') %>%
  arrange(desc(estimate))

coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(x=fct_reorder(term, estimate),
             y=estimate,
             fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  theme_minimal() +
  coord_flip() +
  labs(
    y = 'Impact on Change in Vote Share (2020-2018)',
    x = NULL,
    title = "Words correlated with change in 2-way share"
  )

### now bootstrap
bootstrapped_coefs <- NULL
for (i in 1:50){

  indices <- sample(nrow(modeling_data),replace = TRUE)
  # glmnet without regularization
  model <- glmnet(modeling_data[indices,],
                  outcome$dem_delta[indices],
                  alpha=0,
                  lambda=0,
                  family = "gaussian")
  
  coefs <- model %>%
    tidy() %>%
    filter(term != '(Intercept)') %>%
    mutate(run=i)
  
  if (is.null(bootstrapped_coefs)){
    bootstrapped_coefs <- coefs
  } else {
    bootstrapped_coefs <- rbind(bootstrapped_coefs, coefs)
  }
}


bootstrapped_coefs %>%
  inner_join(
    bootstrapped_coefs %>% 
      dplyr::group_by(term) %>%
      summarise(impact = mean(estimate)) %>%
      ungroup() %>%        
      dplyr::group_by(impact>0) %>%
      dplyr::top_n(10, abs(impact)) %>%
      ungroup(), on='term') %>%
  ggplot(aes(x=fct_reorder(term, estimate),
             y=estimate,
             color = impact > 0,
             fill = impact > 0)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    y = 'Impact on Change in Vote Share (2020-2018)',
    x = NULL,
    title = "Words correlated with change in 2-way share"
)


# for spot checks
for_checks <- candidate_connection  %>%
  mutate(district = paste0(state,'-', district)) %>%
  as_tibble() %>%
  inner_join(tweets_processed,by=c("user_name"="SN")) %>%
  inner_join(cleaned_results,by="district") %>%
  select(user_name, district, Text, dem_2_way, dem_2_way_2018, dem_delta)

for_checks %>%
  filter(grepl('acres',tolower(Text))) %>%
  View()

for_checks %>%
  dplyr::mutate(has = ifelse(grepl('acres',tolower(Text)),'yes','no')) %>%
  dplyr::group_by(has) %>%
  dplyr::summarise(count=n(),
            avg=mean(dem_delta))


for_checks %>%
  filter(grepl('backbone',tolower(Text))) %>%
  View()

for_checks %>%
  dplyr::mutate(has = ifelse(grepl('backbone',tolower(Text)),'yes','no')) %>%
  dplyr::group_by(has) %>%
  dplyr::summarise(count=n(),
                   avg=mean(dem_delta))
