import requests
from bs4 import BeautifulSoup
import pandas as pd
import re
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
    
    recommendation = [s.text for s in u.find_all("strong")]
    recommendation = filter(lambda x: "Overall" in x, recommendation)
    # ratings are ordered
    # fish, potato, tartar, bread, misc
    ratings = [r.text for r in u.find('tr').find_all('span')]
    
    all_divs = u.find_all("div")
    
    review_text = list(filter(lambda x: "Comment:" in x, all_divs))[0]
    
    review_text = review_text.text
    
    review_text = review_text[review_text.find("Comment:")]
    
    full_data = [recommendation, ratings, review_text]
    
    return(full_data)
    
    

review_info = pd.read_csv("fish_fry_review_info.csv")

uu = "http://madisonfishfry.com/reviews.php?restaurant=361&event=1257"

fd = get_data_from_review(uu)