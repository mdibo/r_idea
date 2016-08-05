#!/usr/bin/python
# coding: utf-8
import urllib
import pandas
from bs4 import BeautifulSoup

to_dir = "/home/zhuzhi/projects/r_idea/"
# chinext = pandas.read_csv('./CHINEXT.csv', dtype={"stock_code": "object"},
#                          names=["stock_code", "stock_name", "total_equity", "flow_equity", "industry"], header=0)

sina_default = "http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/STOCK_CODE.phtml"
sina_specific = "http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/STOCK_CODE.phtml?" \
                "year=YEAR&jidu=JIDU"

stock_list = ["601988", "601288", "601398", "601939"]


def fetch_sina_fuquan_history(stock_code):

    '''

    '''

    stock_default = sina_default.replace("STOCK_CODE", stock_code)
    stock_default_page = urllib.urlopen(stock_default).read()
    stock_default_soup = BeautifulSoup(stock_default_page, "lxml")
    year = stock_default_soup.find_all("select", {"name": "year"})
    jidu = stock_default_soup.find_all("select", {"name": "jidu"})
    year_list = year[0].find_all("option")
    year_list = [x.getText() for x in year_list]
    year_list.sort()
    jidu_list = ["1", "2", "3", "4"]
    stock_data = []
    for year_spe in year_list:
        stock_data_year = []
        for jidu_spe in jidu_list:
            print stock_code, year_spe, jidu_spe
            stock_spe_url = sina_specific.replace("YEAR", year_spe)
            stock_spe_url = stock_spe_url.replace("JIDU", jidu_spe)
            stock_spe_url = stock_spe_url.replace("STOCK_CODE", stock_code)
            stock_spe_page = urllib.urlopen(stock_spe_url).read()
            stock_spe_soup = BeautifulSoup(stock_spe_page, "lxml")
            stock_table = stock_spe_soup.find("table", {"id": "FundHoldSharesTable"})
            stock_data_raw = stock_table.find_all("tr")

            if len(stock_data_raw) == 2:
                continue
            else:
                stock_data_jidu = []
                for i in range(2,len(stock_data_raw)):
                    stock_data_day = stock_data_raw[i].find_all("td")
                    stock_data_day = [x.getText().strip() for x in stock_data_day]
                    stock_data_day = {"date": stock_data_day[0], "open": float(stock_data_day[1]),
                                      "high": float(stock_data_day[2]), "close": float(stock_data_day[3]),
                                      "low": float(stock_data_day[4]), "volume": float(stock_data_day[5]),
                                      "amount": float(stock_data_day[6]), "factor": float(stock_data_day[7])}
                    stock_data_jidu.append(stock_data_day)
                stock_data_year.extend(stock_data_jidu)
        stock_data.extend(stock_data_year)
    stock_data = pandas.DataFrame(stock_data, columns=["date", "open", "high", "low", "close", "volume", "amount", "factor"])
    stock_data = stock_data.sort_values("date")
    stock_data.to_csv(to_dir + stock_code, index = False)

for stock_code in stock_list:
    print stock_code
    fetch_sina_fuquan_history(stock_code)
