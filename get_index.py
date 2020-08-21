# -*- coding: utf-8 -*-
"""
Created on Tue Aug 18 20:48:54 2020

@author: JOAO VICTOR
"""

import pandas_datareader.data as web
import pandas as pd

cryptocurrencies = ['BTC-USD']
dados_crypto = web.get_data_yahoo(cryptocurrencies, start='2013-01-01', end='2020-01-01')['Adj Close']

indexs = ['IMOEX.ME', '^BVSP', '^MXX', '^BSESN', '^JKSE', 'XU100.IS', '000001.SS', '^HSI']
dados_index = web.get_data_yahoo(indexs, start='2013-01-01', end='2020-08-24')['Adj Close']


dados_crypto.to_csv('dados_crypto.csv')

for i in indexs:
    dados_index = web.get_data_yahoo(i, start='2013-01-01', end='2020-08-24')['Adj Close']
    dados_index.to_csv('{}.csv'.format(i))

benchs = ['^GSPC', '^DJI', '^IXIC']
for i in benchs:
    dados_bench = web.get_data_yahoo(i, start='2013-01-01', end='2020-08-24')['Adj Close']
    dados_index.to_csv('bench_{}.csv'.format(i))
