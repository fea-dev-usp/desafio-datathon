# -*- coding: utf-8 -*-
"""
Created on Tue Aug 18 20:48:54 2020

@author: JOAO VICTOR
"""

import pandas_datareader.data as web
import pandas as pd

cryptocurrencies = ['BTC-USD']
dados_crypto = web.get_data_yahoo(cryptocurrencies, start='2013-01-01', end='2020-01-01')['Adj Close']

indexs = ['IMOEX.ME', '^BVSP', '^MXX', '^BSESN', '^JKSE', 'XU100.IS']
dados_index = web.get_data_yahoo(indexs, start='2013-01-01', end='2020-01-01')['Adj Close']


dados_crypto.to_csv('dados_crypto.csv')

for i in indexs:
    dados_index = web.get_data_yahoo(i, start='2013-01-01', end='2020-01-01')['Adj Close']
    dados_index.to_csv('{}.csv'.format(i))