import requests
from bs4 import BeautifulSoup
import pandas as pd
import itertools
import time
# create headers to run beautiful soup, makes the website "think" we're real
headers = {'User-Agent': "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1",
           'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
           'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
           'Accept-Encoding': 'none',
           'Accept-Language': 'en-US,en;q=0.8',
           'Connection': 'keep-alive'}

# handy function to convert a url into a bs object
# added `decode` argument because the page sometimes gets wonky
def url_to_bs(url, decode = False):
    request_url = requests.get(url = url,
                               headers = headers)
    
    if decode:
        new_r = request_url.content.decode(errors = 'ignore')
    else:
        new_r = request_url.content
    
    return BeautifulSoup(new_r)

def get_data_from_review(url, decode = True):
    
    u = url_to_bs(url, decode)
    
    try:
        recommendation = [s.text for s in u.find_all("strong")]
        recommendation = list(filter(lambda x: "Overall" in x, recommendation))
    except:
        recommendation = [""]
        print("recommendation could not be read")
    # ratings are ordered
    # fish, potato, tartar, bread, misc
    try:
        ratings = [r.text for r in u.find('tr').find_all('span')]
    except:
        ratings = ["", "", "", "", ""]
        print("ratings could not be read")
    
    try: 
        all_divs = u.find_all("div")
        review_text = list(filter(lambda x: "Comment:" in x.text, all_divs))[0]
        review_text = review_text.text
        review_text = [review_text[review_text.find("Comment:"):]]
    except:
        review_text = [""]
        print("review could not be read")
    
    full_data = [recommendation, ratings, review_text]
    
    full_data = list(itertools.chain.from_iterable(full_data))
    
    return(full_data)
    
 
 review_info = pd.read_csv("fish_fry_review_info.csv")
 
 review_urls = review_info['review_link']
 len_urls = len(review_urls)
 review_data = []
 
 for i in range(len_urls):
     
     review_data.append(get_data_from_review(review_urls[i]))
     
     print("iteration ", i, " finished")
     print(len_urls - (i + 1), " iterations remaining")
     print(100 * round((i + 1)/len_urls, 3), "% complete")
     
     time.sleep(3) # sleep for three seconds to avoid hitting too often
     
     
 
 
 
 
 