import scrape_wsga_results as ws
import pandas as pd

all_urls = [
        "https://wsga.bluegolf.com/bluegolf/wsga18/event/wsga186/contest/1/leaderboard.htm",
        "https://wsga.bluegolf.com/bluegolf/wsga17/event/wsga1744/contest/1/leaderboard.htm",
        "https://wsga.bluegolf.com/bluegolf/wsga15/event/wsga1556/contest/4/leaderboard.htm",
        "https://wsga.bluegolf.com/bluegolf/wsga14/event/wsga1437/contest/1/leaderboard.htm",
        "https://wsga.bluegolf.com/bluegolf/wsga12/event/wsga1243/contest/1/leaderboard.htm"
        ]

course_urls = [t.replace('leaderboard.htm', 'course/stat/index.htm') for t in all_urls]

# get all scorecard links
get_and_write_scores = False

if get_and_write_scores:
    
    scores_url = []
    
    for u in all_urls:
        
        scores_url.append(ws.get_scorecard_links(u))    
        
    scores_url = [item for sublist in scores_url for item in sublist]
    
    # get all scores
    # very minimal error handling
    df_list = []
    fail_list = []
    
    for s in scores_url:
        try:
            df_list.append(ws.get_score_from_link(s))
        except:
            fail_list.append(s)
            
    all_data = pd.concat(df_list)
    
    write_data = False
    
    if write_data:
        all_data.to_csv(path_or_buf = "data\\all_oaks_data.csv", index = False)

get_and_write_course_stats = False

if get_and_write_course_stats:

    course_stat_urls = []
    
    for u in course_urls:
        course_stat_urls.append(ws.get_course_stat_url(u))
    
    df_list = []
    
    for c in course_stat_urls:
        df_list.append(ws.get_course_information(c))
    
    all_course_data = pd.concat(df_list)
    
    write_course_stats = False
    
    if write_course_stats:
        all_course_data.to_csv('data\\course_info.csv', index = False)
        
        
# scrape results from 1 tournament at edelweiss
t_url = "https://wsga.bluegolf.com/bluegolf/wsga17/event/wsga1754/contest/1/leaderboard.htm"
c_url = t_url.replace('leaderboard.htm', 'course/stat/index.htm')

edelweiss_data = ws.full_wsga_scrape(t_url, c_url)

edelweiss_data[0].to_csv(path_or_buf = "data\\edelweiss_data.csv", index = False)
edelweiss_data[1].to_csv('data\\edelweiss_course_data.csv', index = False)

# results from wausau country club
l_url_list = ["https://wiscpga.bluegolf.com/bluegolf/wiscpga11/event/wiscpga1128/contest/1/leaderboard.htm",
              "https://wsga.bluegolf.com/bluegolf/wsga16/event/wsga1623/contest/1/leaderboard.htm",
              "https://wsga.bluegolf.com/bluegolf/wsga14/event/wsga1441/contest/1/leaderboard.htm",
              "https://wsga.bluegolf.com/bluegolf/wsga11/event/wsga1128/contest/1/leaderboard.htm"]

c_url_list = ["https://wiscpga.bluegolf.com/bluegolf/wiscpga11/event/wiscpga1128/contest/1/course/stat/index.htm",
              "https://wsga.bluegolf.com/bluegolf/wsga16/event/wsga1623/contest/1/course/stat/index.htm",
              "https://wsga.bluegolf.com/bluegolf/wsga14/event/wsga1441/contest/1/leaderboard.htm",
              "https://wsga.bluegolf.com/bluegolf/wsga14/event/wsga1441/contest/1/leaderboard.htm"]


score_data_list = []
course_data_list = []

for i in range(0, len(l_url_list)):
    temp_results_wcc = ws.full_wsga_scrape(l_url_list[i],
                                           c_url_list[i])
    
    score_data_list.append(temp_results_wcc[0])
    course_data_list.append(temp_results_wcc[1])


#scores_url = get_scorecard_links(t_url)
#
## get all scores
## very minimal error handling
#df_list = []
#fail_list = []
#
#for s in scores_url:
#    try:
#        df_list.append(get_score_from_link(s))
#    except:
#        fail_list.append(s)
#        
#all_data = pd.concat(df_list)
#
#write_data = True
#
#if write_data:
#    all_data.to_csv(path_or_buf = "data\\edelweiss_data.csv", index = False)
#    
#    
#course_data = get_course_information(get_course_stat_url(c_url))
#course_data.to_csv('data\\edelweiss_course_data.csv', index = False)
