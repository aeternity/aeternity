import ws_client
import logging

def get_new_block(ws, payload):
    ws_client.get_block_by_height(ws, payload["height"]) 

def print_chain_data(ws, payload):
    data_type = payload["type"]
    value = payload[data_type]
    search_key = "hash"
    if payload["height"] != None:
        search_key = "height"
    logging.info("Recieved data for " + data_type + ", requested by its " +
                search_key + " = " + str(payload[search_key]))
    logging.info(str(value))

def actions ():
    return {
        "chain": {  "requested_data": print_chain_data},
        "miner": {  "mined_block": get_new_block}
    }

