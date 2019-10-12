import requests
from bs4 import BeautifulSoup
#import pandas as pd

headers = {'User-Agent': "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1",
           'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
           'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
           'Accept-Encoding': 'none',
           'Accept-Language': 'en-US,en;q=0.8',
           'Connection': 'keep-alive'}

def url_to_bs(url, decode = False):
    request_url = requests.get(url = url,
                               headers = headers)
    
    if decode:
        new_r = request_url.content.decode(errors = 'ignore')
    else:
        new_r = request_url.content
    
    return BeautifulSoup(new_r)


def restaurant_url_to_events(url, decode = True):
    
    uu = url_to_bs(url, decode)
    
    selects = uu.find_all("select")
    events = list(filter(lambda x: x['name'] == 'event', selects))
    
    events_option = [e.find_all('option') for e in events][0]
    
    event_info = zip([url + '&event=' + e['value'] for e in events_option],
                     [e['name'] for e in events_option])
    
    return(list(event_info))
    
    

restaurant_bs = url_to_bs("http://madisonfishfry.com/restaurants.php")
restaurant_links_base = restaurant_bs.find_all('a', class_ = 'restaurantLink')
restaurant_name = [x.text for x in restaurant_links_base]
restaurant_links = ['http://madisonfishfry.com/' + x['href'] for x in restaurant_links_base]

review_information = []

for u in restaurant_links:
    review_information.append(restaurant_url_to_events(u))
