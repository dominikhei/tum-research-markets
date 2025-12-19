import json
import requests
from datetime import datetime
import boto3
from decimal import Decimal

PAIRS = ['ETH/EUR', 'BTC/EUR', 'ALGO/EUR']
FEES = {'maker': 0.0025, 'taker': 0.004}

dynamodb = boto3.resource('dynamodb')
table = dynamodb.Table('ExchangeData')

def convert_floats_to_decimal(obj):
    if isinstance(obj, list):
        return [convert_floats_to_decimal(i) for i in obj]
    elif isinstance(obj, dict):
        return {k: convert_floats_to_decimal(v) for k, v in obj.items()}
    elif isinstance(obj, float):
        return Decimal(str(obj))  
    else:
        return obj

def get_ticker(pair):
    symbol = pair.replace('/', '')
    url = f'https://api.kraken.com/0/public/Ticker?pair={symbol}'
    res = requests.get(url).json()
    data = next(iter(res['result'].values()))
    bid, ask = float(data['b'][0]), float(data['a'][0])
    return bid, ask

def apply_fee(amount, fee_rate):
    return amount * (1 - fee_rate)

def calc_qty(euro, price, fee_rate):
    net = apply_fee(euro, fee_rate)
    return net / price

def lambda_handler(event, context):
    out = {}
    for pair in PAIRS:
        bid, ask = get_ticker(pair)
        print(bid)
        pair = pair.replace("/", "")
        out[pair] = {}
        for typ, rate in FEES.items():
            out[pair][typ] = {str(amt): calc_qty(amt, ask, rate)
                              for amt in [100,500,1000]}
    source = "kraken"
    timestamp = str(datetime.now())
    for symbol, fees in out.items():
        fees_decimals = convert_floats_to_decimal(fees)
        item = {
            'symbol': symbol,
            'datetime': timestamp,
            'source': source,
            'fees': fees_decimals,
        }
        table.put_item(Item=item)
        print(f"Inserted data for {symbol} at {timestamp}")

    return {
        'statusCode': 200,
        'body': "Inserted data in the table"
    }