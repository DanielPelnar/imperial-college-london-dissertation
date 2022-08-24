### Source: Crystal Blockchain -- 
# https://crystalblockchain.com/security-breaches-and-fraud-involving-crypto/
# scrapping dates of security breaches, fraud and scams

''' 
Readme:
chromedriver.exe needs needs to be in the working directory
A appropriate version of chromedriver.exe needs to be downloaded on this website: 
https://sites.google.com/chromium.org/driver/
'''

def main():
    import os
 
    cwd = os.getcwd()
    report_url = "https://crystalblockchain.com/security-breaches-and-fraud-involving-crypto/"
    
    scaped_dates = scrape_dates(cwd, report_url)
    format_and_export(scaped_dates)
    
    
def webpage_exists(url_path):
    """
    Checks if the website status is 200
    
    Parameters
    ----------
    url_path : string
        Url path of the website to be checked.

    Returns
    -------
    Bool
        Returns true or false
    """
    import requests
    response = requests.head(url_path)
    return response.status_code == requests.codes.ok   

def scrape_dates(cwd, report_url):
    """
    Pulls out unique dates from the website
    and formats them as datetime objects
   
    Parameters
    ----------
    cwd : string
        Path of the current working directory 
        where chromedriver.exe is located
    report_url : string
        Url path of the website
        
    Returns
    -------
    unique_dates : set
        Returns a set of unique datetime dates
    """
    from selenium import webdriver
    from selenium.webdriver.chrome.service import Service
    from bs4 import BeautifulSoup
    import datetime
    import time
    import sys
    
    if not webpage_exists(report_url):
        raise Exception("The website cannot be accessed.")
    
    try:
        option = webdriver.ChromeOptions()
        driver = webdriver.Chrome(service=Service(cwd+"{}".format("\chromedriver")), options=option)
    except BaseException:
        # exception selenium.common.exceptions.WebDriverException
        sys.exit("Exiting...")
    
    driver.get(report_url)
    time.sleep(3)  # let it load the website; wait 3 seconds
    website_content = driver.page_source
    website_content = BeautifulSoup(website_content, features="lxml")
    
    unique_dates = set()
    div_content = website_content.find("div", {"class":"simplebar-content"})
    print("\nPrinting dates when an incident occured:")
    for li_row in div_content.find_all("li"):
        date_tag = li_row.find("span", {"class":"list-item__date"})
        date = date_tag.get_text() # getting innerHTML text wrapped in the span tag
        print(date)
        unique_dates.add(datetime.datetime.strptime(date, "%d/%m/%Y"))
        
    driver.quit()
    return unique_dates

def format_and_export(scaped_dates):
    """
    Creates a dummy variable which equals to 1
    if the incident occured on a given day
    and 0 otherwise. Exports as a csv file.
    
    Parameters
    ----------
    scaped_dates : set
        A set of unique datetime dates

    Returns
    -------
    None.
    """
    import pandas as pd
    import numpy as np

    all_contracts_periods = pd.date_range(start="2021-02-03", end="2022-06-24", freq='D') 
    dataset = pd.DataFrame({"time_period":all_contracts_periods}, index=all_contracts_periods) 
    dataset["incidents_dummy"] = np.where(dataset.index.get_level_values(0).isin(scaped_dates), 1, 0)
    dataset.to_csv("incidents.csv", index=False)

if __name__ == "__main__":
    main()









