# BRAD comment this code please!
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

# identify distinct reviews for a website
def restaurant_url_to_events(url, decode = True):
    
    uu = url_to_bs(url, decode)
    
    selects = uu.find_all("select")

    events = list(filter(lambda x: x['name'] == 'event', selects))
    
    events_option = [e.find_all('option') for e in events][0]
    
    event_info = zip([url + '&event=' + e['value'] for e in events_option],
                     [e['name'] for e in events_option])
    
    return(list(event_info))
    
# retrieve all links for reviewed restaurants
restaurant_bs = url_to_bs("http://madisonfishfry.com/restaurants.php")
restaurant_links_base = restaurant_bs.find_all('a', class_ = 'restaurantLink')
restaurant_name = [x.text for x in restaurant_links_base]
restaurant_links = ['http://madisonfishfry.com/' + x['href'] for x in restaurant_links_base]
rest_info = pd.DataFrame(list(zip(restaurant_name, restaurant_links)),
                         columns = ['rest_name', 'rest_link'])



# extract review links
review_information = []

for u in restaurant_links:
    review_information.append(restaurant_url_to_events(u))


review_information_flat_list = []
for sublist in review_information:
    for item in sublist:
        review_information_flat_list.append(item)

review_information_df = pd.DataFrame(review_information_flat_list,
                                     columns = ['review_link', 'review_date'])


review_information_df = review_information_df.assign(rest_link = 
                                                     [re.sub('&event.*', '',x) for x in review_information_df['review_link']])


(review_information_df
.set_index('rest_link')
.join(rest_info.set_index('rest_link'), how = 'inner')
.to_csv('fish_fry_review_info.csv'))



