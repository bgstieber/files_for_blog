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

url1 = "https://wsga.bluegolf.com/bluegolf/wsga15/event/wsga1556/contest/4/leaderboard.htm"
#url2 = "https://wsga.bluegolf.com/bluegolf/wsga15/event/wsga1556/contest/4/contestant/6511/scorecard.htm

def get_scorecard_links(leaderboard_url):
    
    # create beautiful soup object
    url_bs = url_to_bs(leaderboard_url)
    # find a
    url_bs_a = url_bs.find_all('a', href = True)
    # keep href (links)
    href_only = [a['href'] for a in url_bs_a]
    # remove any text not pertaining to scorecards
    remove_text = [not(h.endswith('scorecard.htm')) for h in href_only]
    # https://stackoverflow.com/a/14537238/5619526
    scorecards = [h for (h, remove) in zip(href_only, remove_text) if not remove]
    # concatenate original url
    scorecards = [leaderboard_url.replace('leaderboard.htm', '') + h for h in scorecards]
    
    return(scorecards)

sl1 = "https://wsga.bluegolf.com/bluegolf/wsga15/event/wsga1556/contest/4/contestant/6511/scorecard.htm"
    
def get_score_from_link(scorecard_url):
    
    # create beautiful soup
    url_bs = url_to_bs(scorecard_url)
    # title
    title = url_bs.find('title').text
    # get scores
    scores = url_bs.find('tr', class_ = 'scores')
    scores = [s.text for s in scores.find_all('td')]
    
    scores.remove('Round 1')
    
    scores_front = [scores[i] for i in  range(0,9)]
    scores_back = [scores[i] for i in range(10,19)]
    
    scores = [scores_front, scores_back]
    # https://stackoverflow.com/questions/952914/making-a-flat-list-out-of-list-of-lists-in-python
    scores = [float(item) for sublist in scores for item in sublist]
    
    df = pd.DataFrame(scores).T
    df.columns = ['hole_' + str(x) for x in range(1, 19)]
    df['title'] = title
    df['url'] = scorecard_url
    
    return(df)
    
ci1 = "https://wsga.bluegolf.com/bluegolf/wsga12/event/wsga1243/contest/1/course/stat/index.htm" 


def get_course_stat_url(course_url):
    
    url_bs = url_to_bs(course_url)
    # extract information
    all_a = url_bs.find_all('a', href = True)
    all_a_href = [a['href'] for a in all_a]
    all_a_text_bool = [a.text.endswith('Complete Course Stats') for a in all_a]
    
    url_sub = all_a_href[all_a_text_bool.index(True)]
    
    url_full = course_url.replace('stat/index.htm', '') + url_sub.replace('../', '')
    
    return(url_full)
    

cs1 = "https://wsga.bluegolf.com/bluegolf/wsga12/event/wsga1243/contest/1/course/oaksgc/stat/index.htm"
    
def get_course_information(course_stat_url):
    
    df = pd.read_html(course_stat_url)
    
    if df[0].columns[0] == 0:
        df = df[1]
    else:
        df = df[0]
    # https://medium.com/@chaimgluck1/working-with-pandas-fixing-messy-column-names-42a54a6659cd
    df.columns = df.columns.str.strip().str.lower().str.replace(' ', '_').str.replace('(', '').str.replace(')', '')
    
    df['url'] = course_stat_url
    
    return(df)
    
def full_wsga_scrape(leaderboard_url, course_url):
    
    # read scores and store in dataframe
    scorecard_links = get_scorecard_links(leaderboard_url)
    
    df_list = []
    fail_list = []
    
    for s in scorecard_links:
        try:
            df_list.append(get_score_from_link(s))
        except:
            fail_list.append(s)
            
    score_data = pd.concat(df_list)
    
    # get course information
    course_stat_url = get_course_stat_url(course_url)
    course_data = get_course_information(course_stat_url)
    
    return [score_data, course_data]
    
    
