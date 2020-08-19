# -*- coding: utf-8 -*-
"""
Created on Tue Aug 18 20:48:54 2020

@author: JOAO VICTOR
"""

import yfinance as yf
import pandas_datareader.data as web
import pandas as pd
import warnings
import numpy as np

cryptocurrencies = ['BTC-USD', 'ETH-USD', 'XRP-USD']
dados_yahoo = web.get_data_yahoo(cryptocurrencies, start='2013-01-01', end='2020-01-01')['Adj Close']


indexs = ['IMOEX.ME', '^BVSP', '^MXX', '^BSESN', '^JKSE']
dados_yahoo = web.get_data_yahoo(indexs, start='2013-01-01', end='2020-01-01')['Adj Close']
