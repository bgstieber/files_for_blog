import requests
from bs4 import BeautifulSoup
import pandas as pd

headers = {'User-Agent': "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1",
           'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
           'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
           'Accept-Encoding': 'none',
           'Accept-Language': 'en-US,en;q=0.8',
           'Connection': 'keep-alive'}

def url_to_bs(url):
    request_url = requests.get(url = url,
                               headers = headers)
    
    return BeautifulSoup(request_url.content)


restaurant_bs = url_to_bs("http://madisonfishfry.com/restaurants.php")
restaurant_links_base = restaurant_bs.find_all('a', class_ = 'restaurantLink')
restaurant_name = [x.text for x in restaurant_links_base]
restaurant_links = [x['href'] for x in restaurant_links_base]