### Binance
# querying:
# 1) # USD(s)-M delivery BTC and ETH futures close and volume (USDT marginned and settled)
# 2) Binance SPOT close and SPOT VOLUME for BTCUSDT and ETHUSDT
# 3) BTCUSDT and ETHUSDT: PRICE INDEX and MARK PRICE 

def main():
    all_contracts_symbols = ["BTCUSDT_210326", "BTCUSDT_210625", "BTCUSDT_210924", "BTCUSDT_211231", "BTCUSDT_220325", 
                             "BTCUSDT_220624", "BTCUSDT_220930", "ETHUSDT_210326", "ETHUSDT_210625", "ETHUSDT_210924", 
                             "ETHUSDT_211231", "ETHUSDT_220325", "ETHUSDT_220624", "ETHUSDT_220930"]
    spot_and_index_symbols = ["BTCUSDT", "ETHUSDT"]
    

    # GET /fapi/v1/klines 
    request_format_create_csv(create_links(all_contracts_symbols, "/fapi/v1/klines"), "klines")
    # GET /fapi/v1/indexPriceKlines
    request_format_create_csv(create_links(spot_and_index_symbols, "/fapi/v1/indexPriceKlines"), "indexPriceKlines")
    # GET /api/v3/klines
    request_format_create_csv(create_links(spot_and_index_symbols, "/api/v3/klines"), "klines")
    # GET /fapi/v1/markPriceKlines
    request_format_create_csv(create_links(all_contracts_symbols, "/fapi/v1/markPriceKlines"), "markPriceKlines")

def create_links(symbols, your_request):
    """
    Parameters
    ----------
    symbols : list
        A list of symbols such as BTCUSDT_210625 or BTCUSDT.
    your_request : set
        A set of api request strings, a specific format is needed.
        See VALID_REQUESTS, otherwise ValueError
    Returns
    -------
    all_links : list
        A list of all available links, which can be requested from Binance.
    """
    VALID_REQUESTS = {"/fapi/v1/klines", "/api/v3/klines", "/fapi/v1/indexPriceKlines", "/fapi/v1/markPriceKlines"}
    
    if your_request not in VALID_REQUESTS:
        raise ValueError("results: your_request must be one of {}".format(VALID_REQUESTS))
        
    if your_request == "/fapi/v1/klines":
        template_link = "https://fapi.binance.com/fapi/v1/klines?symbol={}&interval=1d"
    elif your_request == "/api/v3/klines":
        template_link = "https://api.binance.com/api/v3/klines?symbol={}&limit=1000&interval=1d"
    elif your_request == "/fapi/v1/indexPriceKlines":
        template_link = "https://fapi.binance.com/fapi/v1/indexPriceKlines?pair={}&limit=1000&interval=1d"
    elif your_request == "/fapi/v1/markPriceKlines":
        template_link = "https://fapi.binance.com/fapi/v1/markPriceKlines?symbol={}&limit=1000&interval=1d"
      
    all_links = []
    for symbol in symbols:
        link = template_link.format(symbol)
        all_links.append(link)
    return all_links

def request_format_create_csv(all_links, data_type):
    """
    Parameters
    ----------
    all_links : list
        A list of all available links, which can be requested from Binance.
    data_type : set
        A set of strings, only a few strings are allowed.
        See VALID_DATA_TYPES, otherwise ValueError
    Returns
    -------
    None.
    
    This function requests data from Binance according to the links, 
    then formats the data and creates csv files.
    """
    # Imports:
    import json
    import urllib.request
    import pandas as pd
    import re
    
    VALID_DATA_TYPES = {"klines", "indexPriceKlines", "markPriceKlines"}
    
    if data_type not in VALID_DATA_TYPES:
        raise ValueError("results: data_type must be one of {}".format(VALID_DATA_TYPES))

    if data_type == "klines":
        columns_queried = ['time_period', 'open', 'high', 'low', 'close', 
                           'volume', 'close_time', 'quote_asset_volume', 'number_of_trades', 
                           'taker_buy_base_asset_volume', 'taker_buy_quote_asset_volume', 'ignore' ]

        columns_not_needed = ['open', 'high', 'low', 'close_time', 'quote_asset_volume', 'number_of_trades', 
                              'taker_buy_base_asset_volume', 'taker_buy_quote_asset_volume', 'ignore']
    elif data_type == "indexPriceKlines" or "markPriceKlines":
        columns_queried = ['time_period', 'open', 'high', 'low', 'close', 
                           'Ignore1', 'Ignore2', 'Ignore3', 'Ignore4', 
                           'Ignore5', 'Ignore6', 'Ignore7' ]
        
        columns_not_needed = ['open', 'high', 'low', 
                              'Ignore1', 'Ignore2', 'Ignore3', 'Ignore4', 
                              'Ignore5', 'Ignore6', 'Ignore7']
        
    dict_dataset = dict()
    
    for link in all_links:
        response = urllib.request.urlopen(link).read()
        data = pd.DataFrame(json.loads(response), columns = columns_queried)
        data['time_period'] = pd.to_datetime(data['time_period'], unit='ms')
        data.drop(columns_not_needed, axis=1, inplace=True)
        start_index = re.search("=", link)
        end_index = re.search("&", link)
        symbol_name = link[start_index.start()+1:end_index.start()] 
        dict_dataset[symbol_name] = data
        data.to_csv("{}{}{}.csv".format(symbol_name, "_", data_type))
        
if __name__ == "__main__":
    main()
    
    
    
    