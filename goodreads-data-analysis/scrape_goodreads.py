from bs4 import BeautifulSoup
import requests 
import pandas as pd

heads = {'User-Agent': "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.129 Safari/537.36",
                        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
                        'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
                        'Accept-Encoding': 'none',
                        'Accept-Language': 'en-US,en;q=0.9',
                        'Connection': 'keep-alive'}



# helper function for BeautifulSoup
def url_to_bs(url, headers = heads):
    return BeautifulSoup(requests.get(url = url, headers = headers).content)

def get_goodreads_data(book_id):
    
    url = "https://www.goodreads.com/book/show/" + book_id
    
    book_url = url_to_bs(url)
    # get description
    dscrp_div = book_url.find(id = "descriptionContainer")
    description = dscrp_div .find("span", {"style": "display:none"}).text
    
    return pd.DataFrame([[book_id, description]], 
                        columns = ['book_id', 'description'])
    
    