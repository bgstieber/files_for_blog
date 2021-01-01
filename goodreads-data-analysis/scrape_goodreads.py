from bs4 import BeautifulSoup
import requests 
import pandas as pd

heads = {'User-Agent': "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36",
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
    # minor error handling
    # some descriptions show up where they're supposed to, others don't
    if dscrp_div is None:
        description = ""
    else:
        dscrip_div2 = dscrp_div.find("span", {"style": "display:none"})
        if dscrip_div2 is None:
            description = ""
        else:
            description = dscrip_div2.text
    
    return pd.DataFrame([[book_id, description]], 
                        columns = ['book_id', 'description'])
    
    
    
goodreads_file = pd.read_csv("goodreads_library_export.csv")
books_2020 = goodreads_file[pd.to_datetime(goodreads_file['Date Read'], format = "%Y/%m/%d") >= "2020-01-01"]
books_2020 = books_2020.reset_index(drop = True)
# pull book id
book_ids = books_2020["Book Id"]

# convert to string
book_id_string = []

for i in range(len(book_ids)):
    book_id_string.append(str(book_ids[i]))
    
    
# read data
description_data = pd.DataFrame(columns = ["book_id", "description"])

for i in range(len(book_id_string)):  
    new_data = get_goodreads_data(book_id_string[i])
    
    description_data = pd.concat([description_data, new_data])
    
    
description_data.to_csv("books_2020_description.csv", index = False, encoding = "utf-8-sig")
    
    