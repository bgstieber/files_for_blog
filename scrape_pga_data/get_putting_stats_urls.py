# BRAD comment this code please!
import requests
from bs4 import BeautifulSoup
import pandas as pd
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

def add_year_to_url(url, year = 2019):
    
    # remove "html" from end of url
    url = url[:-4]
    # paste ".year.html" on to end of url
    return url + "y" + str(year) + ".html"

putting_url = "https://www.pgatour.com/stats/categories.RPUT_INQ.html"

putting_bs = url_to_bs(putting_url)

putting_stats = putting_bs.find("div", class_ = "table-content clearfix")

putting_stats = putting_stats.find_all("a")

putting_stats_names = [u.text for u in putting_stats]
putting_stats_url = ["https://www.pgatour.com" + u['href'] for u in putting_stats]

putting_url_df = pd.DataFrame(columns = ['year', 'name', 'url'])


for y in range(2009, 2020):
    for i in range(len(putting_stats_names)):
        putting_url_df = putting_url_df.append(
                pd.DataFrame({
                        'year': [y],
                        'name': [putting_stats_names[i]],
                        'url': [add_year_to_url(putting_stats_url[i],
                                                y)]
                        })
                )
    
putting_url_df.to_csv("putting_url_stats.csv", index = False)