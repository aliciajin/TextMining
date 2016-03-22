# -*- coding: utf-8 -*-
"""
Created on Thu Nov  5 20:32:47 2015

@author: Alicia Jin
text mining 
"""

import nltk
from nltk.corpus import stopwords
from nltk import * # for freq
import pandas as pd

colnames = ['name', 'scraper', 'url', 'address', 'country', 'description']
dataset = pd.read_csv('/users/apple/Documents/powerlinx/description_sample_11_3.csv', names = colnames)

###################### remove ads, get description2 ###########
def ads_clean(text):
    ads1 = 'Do you want to be at the top of the ThomasNet search results? Promote Your Business'
    ads2 = "popadbanner{        background-image: url('//cdn.thomasnet.com/images/made2spec/fabricationservices.png');}        Don't show again        Close Window                Let ThomasNet Build a Better Short List for You! SUBMIT YOUR RFQ AND GET UP TO 5 QUOTES                     FROM QUALIFIED FABRICATORSTell us what you're looking for and our team of engineers will connect you with up to 5 Fabricators with the capabilities and expertise you need. Promote Your Business"
    if ads1 in text:
        text = text.replace(ads1, ' ')
    elif ads2 in text:
        text = text.replace(ads2, ' ')
    return text

desp_clean = []
status_list = []
for sen in dataset['description']:
    desp_clean.append(ads_clean(sen))
    status = ' ' if ads_clean(sen) == sen else 'Changed'
    status_list.append(status)
dataset['description2'] = desp_clean
dataset['clean_status'] = status_list
###################
dataset.to_csv('/users/apple/Documents/powerlinx/description_cleaned4.csv')

#####  statistical analysis on description length, divided by scraper group
dataset['desc_length'] = map(lambda x: len(x), dataset['description'] )
by_scraper = dataset.groupby('scraper')
len(by_scraper)
by_scraper.count().head()
by_scraper.size().head()
by_scraper.mean()
by_scraper.var()

by_scraper.mean().to_csv('/users/apple/Documents/powerlinx/desp_length_mean.csv')
by_scraper.var().to_csv('/users/apple/Documents/powerlinx/desp_length_var.csv')
######################

####### calculate original words freq distribution  ########
import re
from nltk.corpus import stopwords
stopwords = set(stopwords.words('english'))
#####  function to get tokenize
def freq(scraper_name):
    list_of_description = dataset[dataset.scraper == scraper_name].description
    joined = ' '.join(list_of_description).lower()
    joined_no_punctuation = re.findall(r'\w+', joined)
    words_no_stop = [word for word in joined_no_punctuation if word not in stopwords]
    joined_clean = ' '.join(words_no_stop)
    tokens = nltk.wordpunct_tokenize(joined_clean)
    words_clean = nltk.Text(tokens)
    return FreqDist(words_clean)

#freq('linkedin').most_common(100)
scraper_list = set(dataset.scraper)
freq_df = pd.DataFrame(columns = scraper_list)
for name in scraper_list:
    freq_df[name] = freq(name).most_common(100)
freq_df.to_csv('/users/apple/Documents/powerlinx/freq_df.csv')

################################################
### get the 100 most frequent phrases: phrase_df ######
def phrase(scraper_name):
    list_of_description = dataset[dataset.scraper == scraper_name].description
    joined = ' '.join(list_of_description).lower()
    joined_no_punctuation = re.findall(r'\w+', joined)
    words_no_stop = [word for word in joined_no_punctuation if word not in stopwords]
    joined_clean = ' '.join(words_no_stop)
    tokens = nltk.wordpunct_tokenize(joined_clean)
    words_clean = nltk.Text(tokens)
    return words_clean.collocations(100)
    
scraper_list = set(dataset.scraper)
phrase_df = pd.DataFrame(columns=scraper_list)
for name in scraper_list:
    phrase_df[name] = phrase(name)
phrase_df.to_csv('/users/apple/Documents/powerlinx/phrase_df.csv')

########   language detect trial  ########
from nltk.corpus import stopwords
def language_detect(text):
    tokens = nltk.wordpunct_tokenize(text)
    language_ratios = {}
    words = [word.lower() for word in tokens]
    words_set = set(words)
    for language in stopwords.fileids():
        stopwords_set = set(stopwords.words(language))
        commen_elements = words_set.intersection(stopwords_set)
        language_ratios[language] = len(commen_elements)
    return max(language_ratios, key=language_ratios.get)

language_detect(sen)
sen2="MyVitaly è un’azienda, ma soprattutto un’amicizia che lega quattro persone alla cura del corpo come benessere psico-fisico ispirandosi alla medicina alternativa dei popoli"
language_detect(sen2)
sen4 = 'Want Llc is a Law Practice company located in 445 W 23rd St Apt 8b, New York, New York, United States.'
language_detect(sen4)

#############




    
    
    
    