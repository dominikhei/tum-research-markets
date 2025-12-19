import json

FEES = {
    "kraken": {"maker": 0.01, "taker": 0.01},       
    "coinbase": {"maker": 0.0149, "taker": 0.0149}, 
    "bitvavo": {"maker": 0.0015, "taker": 0.0025},  
}

def apply_fee(amount, source, order_type):
    """Gebühr anwenden (Betrag nach Abzug der Gebühr)"""
    fee_rate = FEES[source][order_type]
    return amount * (1 - fee_rate)

def remove_kraken_fee(amount, order_type):
    """Kraken-Gebühr rückrechnen -> ursprünglicher Bruttowert"""
    fee_rate = FEES["kraken"][order_type]
    return amount / (1 - fee_rate)

def update_entry(entry):
    source = entry["source"]

    updated_fees = {}
    price_without_fee = {}

    for order_type, amounts in entry["fees"].items():
        updated_fees[order_type] = {}
        price_without_fee[order_type] = {}

        for euro_str, kraken_value in amounts.items():
            gross = remove_kraken_fee(kraken_value, order_type)

            if source == "kraken":
                new_value = kraken_value  
            else:
                new_value = apply_fee(gross, source, order_type)

            no_fee_value = new_value * (1 - FEES[source][order_type])

            updated_fees[order_type][euro_str] = new_value
            price_without_fee[order_type][euro_str] = no_fee_value

    entry["fees"] = updated_fees
    entry["price_without_fee"] = price_without_fee
    return entry

with open("data.json", "r", encoding="utf-8") as f:
    data = json.load(f)

updated_data = [update_entry(entry) for entry in data]

with open("data_updated.json", "w", encoding="utf-8") as f:
    json.dump(updated_data, f, ensure_ascii=False, indent=2)

