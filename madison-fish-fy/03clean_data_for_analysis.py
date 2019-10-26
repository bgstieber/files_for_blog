import pandas as pd
from textblob import TextBlob

def convert_date(x):
    return(x[6:] + "-" + x[:2] + "-" + x[3:5])

def clean_review(x):
    x = x.rstrip().replace("\r", "").replace("\n", "").replace("Comment: ", "")
    return(x)    

fish_data = pd.read_csv('full_review_data.csv')

for i in range(fish_data.shape[0]):
    fish_data.review_date[i] = convert_date(fish_data.review_date[i])
    fish_data.review[i] = clean_review(str(fish_data.review[i]))

len_review = [len(r) for r in fish_data.review]

fish_data = fish_data.assign(review_length = len_review)    
    
polarity = []
subjectivity = []

for i in range(fish_data.shape[0]):
    tb_review = TextBlob(fish_data.review[i]).sentiment
    
    polarity.append(tb_review[0])
    subjectivity.append(tb_review[1])
    
fish_data = fish_data.assign(polarity = polarity)
fish_data = fish_data.assign(subjectivity = subjectivity)

