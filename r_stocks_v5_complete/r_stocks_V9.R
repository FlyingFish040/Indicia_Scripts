## ----message=FALSE, warning=FALSE, paged.print=TRUE--------------------------------------------------------------------------------------------------------
require(rvest) #Web scraping
require(xfun)
require(tidyverse)
require(tidytext)
require(lsa) #stopwoorden Engels
require(lubridate)
require(googlesheets4)
require(gargle)
require(dplyr)
require(httr)
require(jsonlite)
require(httpuv)
require(rtweet)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#auth Google
gs4_auth(email = TRUE, path = "./lets-roar-dashboard-indicia-e14ed872df3a.json")

#auth Twitter
api_key <- "vSkjr2yq2JXe82rdvSiSn1bu7"
api_secret_key <- "Nfl7vQG0q8cWrZYTqRTXcmA2YonD3VId7DvqjN1GKQLwOqjNIZ"
access_token <- "1400025718977961984-YjENARs3tKImkn7qrh0BFctxfpzUMZ"
access_token_secret <- "TheVuShnnlMvH5WHcpV3QUsjOyuRkpeWFNSZmb9KmvJb4"

token <- create_token(
  app = "Indicia Data Dashboard",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------

time_current <- format(Sys.time(), "%H:%M")
date_current <- as.Date(today())

if((time_current > "03:00")&(time_current < "03:15]")) {
  
  cleanup_sheet_stocks <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Y6OJ1UYOgf8zDXmf6tuPqSjdFEP13Hy-Uz3wqNlApDI/edit?usp=sharing", sheet = "Reddit_master", col_types = "ccD")
  
  cleanup_sheet_crypto <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1FUoxWJQ6TU2B08-7J7k93B8bp6nSp5IPaqwQF7e8hik/edit?usp=sharing", sheet = "Reddit_master", col_types = "ccD")
  
  cleanup_sheet_twitter <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/11w8u27x1TDmsmbtgLYvilhUu_4CPlCxrBbbS7caqgQY/edit?usp=sharing", sheet = "Twitter_master", col_types = "cD")
  
  
  #regels verwijderen
  cleanup_sheet_stocks <- cleanup_sheet_stocks[cleanup_sheet_stocks$date > (date_current - 7), ]
  cleanup_sheet_crypto <- cleanup_sheet_crypto[cleanup_sheet_crypto$date > (date_current - 7), ]
  cleanup_sheet_twitter <- cleanup_sheet_twitter[cleanup_sheet_twitter$date > (date_current - 7), ]
  
  
  #terugsturen
  write_sheet(ss = "https://docs.google.com/spreadsheets/d/1Y6OJ1UYOgf8zDXmf6tuPqSjdFEP13Hy-Uz3wqNlApDI/edit?usp=sharing", cleanup_sheet_stocks, sheet = "Reddit_master")
  
  sheet_write(ss = "https://docs.google.com/spreadsheets/d/1FUoxWJQ6TU2B08-7J7k93B8bp6nSp5IPaqwQF7e8hik/edit?usp=sharing", cleanup_sheet_crypto, sheet = "Reddit_master")
  
  sheet_write(ss = "https://docs.google.com/spreadsheets/d/11w8u27x1TDmsmbtgLYvilhUu_4CPlCxrBbbS7caqgQY/edit?usp=sharing", cleanup_sheet_twitter, sheet = "Twitter_master")
  
}


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
webpage_stocks <- "https://www.reddit.com/r/stocks/new/"
webpage_investing <- "https://www.reddit.com/r/investing/new/"
webpage_wallstreetbets <- "https://www.reddit.com/r/wallstreetbets/new/"
webpage_fatfire <- "https://www.reddit.com/r/fatFIRE/new/"
webpage_robinhood <- "https://www.reddit.com/r/RobinHood/new/"
webpage_stockmarket <- "https://www.reddit.com/r/StockMarket/new/"

webpage_cryptocurrency <- "https://www.reddit.com/r/CryptoCurrency/new/"
webpage_crypto <- "https://www.reddit.com/r/crypto/new/"
webpage_cryptomarkets <- "https://www.reddit.com/r/CryptoMarkets/new/"
webpage_cryptomoonshots <- "https://www.reddit.com/r/CryptoMoonShots/new/"
webpage_cryptocurrencies <- "https://www.reddit.com/r/CryptoCurrencies/new/"
webpage_altcoin <- "https://www.reddit.com/r/altcoin/new/"

data_stocks <- read_html(webpage_stocks)
data_investing <- read_html(webpage_investing)
data_wallstreetbets <- read_html(webpage_wallstreetbets)
data_fatfire <- read_html(webpage_fatfire)
data_robinhood <- read_html(webpage_robinhood)
data_stockmarket <- read_html(webpage_stockmarket)

data_cryptocurrency <- read_html(webpage_cryptocurrency)
data_crypto <- read_html(webpage_crypto)
data_cryptomarkets <- read_html(webpage_cryptomarkets)
data_cryptomoonshots <- read_html(webpage_cryptomoonshots)
data_cryptocurrencies <- read_html(webpage_cryptocurrencies)
data_altcoin <- read_html(webpage_altcoin)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
twitter <- search_tweets(
  "investing", n = 75, include_rts = TRUE, lang = "en")

twitter <- twitter %>%
  select("text")



## ----warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
H3_stocks <- data_stocks %>%
  html_nodes('p, h3') %>%
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_investing <- data_investing %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_wallstreetbets <- data_wallstreetbets %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_fatfire <- data_fatfire %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_robinhood <- data_robinhood %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_stockmarket <- data_stockmarket %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()


H3_cryptocurrency <- data_cryptocurrency %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_crypto <- data_crypto %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_cryptomarkets <- data_cryptomarkets %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_cryptomoonshots <- data_cryptomoonshots %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_cryptocurrencies <- data_cryptocurrencies %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()

H3_altcoin <- data_altcoin %>%
  html_nodes('p, h3') %>% #
  html_text(trim = TRUE) %>%
  str_squish() %>%
  str_trim()



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
H3_master_stocks <- c(H3_stocks, H3_investing, H3_wallstreetbets, H3_fatfire, H3_robinhood, H3_stockmarket)
H3_master_crypto <- c(H3_cryptocurrency, H3_crypto, H3_cryptomarkets, H3_cryptomoonshots, H3_cryptocurrencies, H3_altcoin)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
H3_master_stocks_DD <- readRDS("./H3_master_stocks.RDS")
H3_master_crypto_DD <- readRDS("./H3_master_crypto.RDS")

H3_stocks <- setdiff(H3_stocks, H3_master_stocks_DD)
H3_investing <- setdiff(H3_investing, H3_master_stocks_DD)
H3_wallstreetbets <- setdiff(H3_wallstreetbets, H3_master_stocks_DD)
H3_fatfire <- setdiff(H3_fatfire, H3_master_stocks_DD)
H3_robinhood <- setdiff(H3_robinhood, H3_master_stocks_DD)
H3_stockmarket <- setdiff(H3_stockmarket, H3_master_stocks_DD)

H3_cryptocurrency <- setdiff(H3_cryptocurrency, H3_master_crypto_DD)
H3_crypto <- setdiff(H3_crypto, H3_master_crypto_DD)
H3_cryptomarkets <- setdiff(H3_cryptomarkets, H3_master_crypto_DD)
H3_cryptomoonshots <- setdiff(H3_cryptomoonshots, H3_master_crypto_DD)
H3_cryptocurrencies <- setdiff(H3_cryptocurrencies, H3_master_crypto_DD)
H3_altcoin <- setdiff(H3_altcoin, H3_master_crypto_DD)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(H3_master_stocks, "./H3_master_stocks.RDS")
saveRDS(H3_master_crypto, "./H3_master_crypto.RDS")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
keywords_twitter <- twitter%>%
  unnest_tokens(word, text)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
text_stocks <- enframe(H3_stocks, name="id", value="waarde")
keywords_stocks <- text_stocks %>%
  unnest_tokens(word,waarde)

text_investing <- enframe(H3_investing, name="id", value="waarde")
keywords_investing <- text_investing %>%
  unnest_tokens(word,waarde)

text_wallstreetbets <- enframe(H3_wallstreetbets, name="id", value="waarde")
keywords_wallstreetbets <- text_wallstreetbets %>%
  unnest_tokens(word,waarde)

text_fatfire <- enframe(H3_fatfire, name="id", value="waarde")
keywords_fatfire <- text_fatfire %>%
  unnest_tokens(word,waarde)

text_robinhood <- enframe(H3_robinhood, name="id", value="waarde")
keywords_robinhood <- text_robinhood %>%
  unnest_tokens(word,waarde)

text_stockmarket <- enframe(H3_stockmarket, name="id", value="waarde")
keywords_stockmarket <- text_stockmarket %>%
  unnest_tokens(word,waarde)


text_cryptocurrency <- enframe(H3_cryptocurrency, name="id", value="waarde")
keywords_cryptocurrency <- text_cryptocurrency %>%
  unnest_tokens(word,waarde)

text_crypto <- enframe(H3_crypto, name="id", value="waarde")
keywords_crypto <- text_crypto %>%
  unnest_tokens(word,waarde)

text_cryptomarkets <- enframe(H3_cryptomarkets, name="id", value="waarde")
keywords_cryptomarkets <- text_cryptomarkets %>%
  unnest_tokens(word,waarde)

text_cryptomoonshots <- enframe(H3_cryptomoonshots, name="id", value="waarde")
keywords_cryptomoonshots <- text_cryptomoonshots %>%
  unnest_tokens(word,waarde)

text_cryptocurrencies <- enframe(H3_cryptocurrencies, name="id", value="waarde")
keywords_cryptocurrencies <- text_cryptocurrencies %>%
  unnest_tokens(word,waarde)

text_altcoin <- enframe(H3_altcoin, name="id", value="waarde")
keywords_altcoin <- text_altcoin %>%
  unnest_tokens(word,waarde)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
keywords_stocks <- keywords_stocks %>%
  mutate("r/stocks")
colnames(keywords_stocks)[3] <- "subsource"

keywords_investing <- keywords_investing %>%
  mutate("r/investing")
colnames(keywords_investing)[3] <- "subsource"

keywords_wallstreetbets <- keywords_wallstreetbets %>%
  mutate("r/wallstreetbets")
colnames(keywords_wallstreetbets)[3] <- "subsource"

keywords_fatfire <- keywords_fatfire %>%
  mutate("r/fatfire")
colnames(keywords_fatfire)[3] <- "subsource"

keywords_robinhood <- keywords_robinhood %>%
  mutate("r/robinhood")
colnames(keywords_robinhood)[3] <- "subsource"

keywords_stockmarket <- keywords_stockmarket %>%
  mutate("r/stockmarket")
colnames(keywords_stockmarket)[3] <- "subsource"


keywords_cryptocurrency <- keywords_cryptocurrency %>%
  mutate("r/cryptocurrency")
colnames(keywords_cryptocurrency)[3] <- "subsource"

keywords_crypto <- keywords_crypto %>%
  mutate("r/crypto")
colnames(keywords_crypto)[3] <- "subsource"

keywords_cryptomarkets <- keywords_cryptomarkets %>%
  mutate("r/cryptomarkets")
colnames(keywords_cryptomarkets)[3] <- "subsource"

keywords_cryptomoonshots <- keywords_cryptomoonshots %>%
  mutate("r/cryptomoonshots")
colnames(keywords_cryptomoonshots)[3] <- "subsource"

keywords_cryptocurrencies <- keywords_cryptocurrencies %>%
  mutate("r/cryptocurrencies")
colnames(keywords_cryptocurrencies)[3] <- "subsource"

keywords_altcoin <- keywords_altcoin %>%
  mutate("r/altcoin")
colnames(keywords_altcoin)[3] <- "subsource"
  


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
keywords_master_stocks <- rbind(keywords_fatfire, keywords_investing, keywords_robinhood, keywords_stockmarket, keywords_wallstreetbets, keywords_stocks)

keywords_master_crypto <- rbind(keywords_cryptocurrency, keywords_crypto, keywords_cryptomarkets, keywords_cryptomoonshots, keywords_cryptocurrencies, keywords_altcoin)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
keywords_master_stocks <- keywords_master_stocks %>% 
  mutate(date=today())

keywords_master_crypto <- keywords_master_crypto %>% 
  mutate(date=today())

keywords_twitter <- keywords_twitter %>%
  mutate(date=today())


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#standaard stopwoorden in tabel verzamelen
stopwords_en_tibble <- enframe(stopwords_en, name="id", value="word")



## ----------------------------------------------------------------------------------------------------------------------------------------------------------

extra_words_filter2 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1dBDCMvuopEGki2SJcmDH5dOs-kp5D3tgYu79gn26UfA/edit?usp=sharing", sheet = "Filter", col_names = TRUE, col_types = "c")



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
stopwords_en_tibble_new <- rbind(stopwords_en_tibble, extra_words_filter2)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
clean_keywords_master_stocks <- keywords_master_stocks %>% 
      anti_join(stopwords_en_tibble_new, by = c("word" = "word")) %>%
      anti_join(stop_words, by = c("word" = "word"))

clean_keywords_master_crypto <- keywords_master_crypto %>% 
      anti_join(stopwords_en_tibble_new, by = c("word" = "word")) %>%
      anti_join(stop_words, by = c("word" = "word"))

clean_keywords_twitter <- keywords_twitter %>% 
      anti_join(stopwords_en_tibble_new, by = c("word" = "word")) %>%
      anti_join(stop_words, by = c("word" = "word"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------

clean_keywords_master_stocks <- subset(clean_keywords_master_stocks, select = c(word, subsource, date))

clean_keywords_master_crypto <- subset(clean_keywords_master_crypto, select = c(word, subsource, date))



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
clean_keywords_master_stocks <- clean_keywords_master_stocks[duplicated(clean_keywords_master_stocks), ]
clean_keywords_master_crypto <- clean_keywords_master_crypto[duplicated(clean_keywords_master_crypto), ]
clean_keywords_twitter <- clean_keywords_twitter[duplicated(clean_keywords_twitter), ]


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
#append
sheet_append(ss = "https://docs.google.com/spreadsheets/d/1Y6OJ1UYOgf8zDXmf6tuPqSjdFEP13Hy-Uz3wqNlApDI/edit?usp=sharing", clean_keywords_master_stocks, sheet = "Reddit_master")

sheet_append(ss = "https://docs.google.com/spreadsheets/d/1FUoxWJQ6TU2B08-7J7k93B8bp6nSp5IPaqwQF7e8hik/edit?usp=sharing", clean_keywords_master_crypto, sheet = "Reddit_master")

sheet_append(ss = "https://docs.google.com/spreadsheets/d/11w8u27x1TDmsmbtgLYvilhUu_4CPlCxrBbbS7caqgQY/edit?usp=sharing", clean_keywords_twitter, sheet = "Twitter_master")




## ----------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())

