# consumer_key <- 'gPdU5pebOV4N6oJxdvbUz7YqX'
# consumer_secret <- 'x7dloLsXUhhItrxoiQYA4885s2QqjlT2W6fKqbNrXbYRGeYDFH'
# access_token <- '26087075-OQxeNzkmQP4PDCNBcYUT44MQA7DzprLPXEtVcVjlB'
# access_secret <- '0zkDMaKyMhB3EV012ZSI09jkui8Awtn8OsZByjfCME0sY'
library(twitteR)
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

textos <- searchTwitter("#R", n=500, lang = "pt-br")
