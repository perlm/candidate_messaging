import configparser, datetime, twitter, sys, os, glob, re, time
import pandas as pd
import numpy as np

#################################################
# Collect some tweets!
#################################################

def tweetLogIn():
    config = configparser.ConfigParser()
    config.read('{}/.python_keys.conf'.format(os.path.expanduser("~")))
    t = twitter.Twitter(auth=twitter.OAuth(token=config.get('twitter','token'), token_secret=config.get('twitter','token_secret'), consumer_key=config.get('twitter','consumer_key'), consumer_secret=config.get('twitter','consumer_secret')),
        retry=True)
    return t

def readAccounts():
    # https://www.propublica.org/datastore/dataset/politicians-tracked-by-politwoops
    file = "{}/candidate_messaging/data/dem_candidates.csv".format(os.path.expanduser("~"))
    df = pd.read_csv(file,header=None)
    return df

def getTweets(screen_name, count=200):
    # Given a screen_name

    t = tweetLogIn()
    
    # api documentation
    # https://developer.twitter.com/en/docs/twitter-api/v1/tweets/timelines/api-reference/get-statuses-home_timeline
    
    # could fix this:
    # https://stackoverflow.com/questions/38717816/twitter-api-text-field-value-is-truncated

    df = pd.DataFrame(columns=['SN','Text','Date'])
    tweets = t.statuses.user_timeline(screen_name=screen_name, count=count, tweet_mode='extended')
    for tweet in tweets:
        df = df.append({'SN':screen_name,
                        'Text':tweet['full_text'],
                        'Date':tweet['created_at']},
            ignore_index=True)
    return df
    

def storeTweets(df, name): 
    # Stores raw tweet data
    raw_file = '{}/candidate_messaging/raw/{}.csv'.format(os.path.expanduser("~"), name)
    df.to_csv(raw_file, index=False)

if __name__ == '__main__':

    # put together raw output (tweets)
    accounts = readAccounts()
    for index, row in accounts.iterrows():
        # if csv does not exist!
        name = row[0]
        print(name)
        raw_out = "{}/candidate_messaging/raw/{}.csv".format(os.path.expanduser("~"), name)
        if not os.path.exists(raw_out):
            try:
                tweets = getTweets(name)
            except:
                tweets = None
            if tweets is not None and not tweets.empty:
                storeTweets(tweets, name)

            # should be good enough. :--)
            time.sleep(5)


    # while testing:
    # t.application.rate_limit_status()
