import websocket
import json
import argparse
import time
import sys
import logging
import importlib

MESSAGE_FUNCTIONS = {
        }

def error(msg):
    logging.error("[ERROR] " + msg)

def read_argv(argv):
    global trace
    parser = argparse.ArgumentParser(description='Websocket test')
    parser.add_argument('--port', type=int, default=3114,
                        help='Node\'s port number')
    parser.add_argument('--log', type=str, default="INFO",
                        help='Logging level')
    parser.add_argument('--handler', type=str, default="ws-handler",
                        help='Websocket logic handler module name')
    args = parser.parse_args()
    port = args.port
    handler = args.handler
    log_levels_dict= {"DEBUG": logging.DEBUG,
                      "INFO": logging.INFO,
                      "WARNING": logging.WARNING,
                      "ERROR": logging.ERROR}
    log_level_value = log_levels_dict.get(args.log, "INFO")
    trace = False
    if log_level_value == logging.DEBUG:
        trace = True
    logging.basicConfig(level=log_level_value, format='%(message)s', )
    return (port, handler, trace)

def parse_message(message_json):
    message = json.loads(message_json)
    status = message.get("status")
    if status:
        if status == "ok":
            result = message.get("result")
            if result:
                logging.info("Request succeded with: " + str(result))
            else:
                logging.info("Request succeded")
        if status == "error":
            error("Request failed with: " + str(message["reason"]))
        return None
    action = message.get("action")
    if action == None or not isinstance(action, basestring):
        error("Invalid action")
        return None
    payload = message.get("payload", {})
    origin = message.get("origin")
    if origin == None or not isinstance(origin, basestring):
        error("Invalid origin")
        return None
    origin_actions = MESSAGE_FUNCTIONS.get(origin)
    if origin_actions == None:
        error("Unknown origin " + origin)
        return None
    fun = origin_actions.get(action)
    if fun == None:
        error("Unknown action " + action + " for origin " + origin)
        return None
    return (fun, payload)


## websocket
def on_message(ws, message_json):
    logging.debug("\n------------------------")
    logging.debug("--- incoming message ---")
    print(">> " + str(message_json))
    act = parse_message(message_json)
    if act != None:
        (fun, payload) = act
        fun(ws, payload)

def ws_send(ws, target, action, payload={}):
    if len(payload) > 0:
        msg = {"target": target, "action": action, "payload": payload}
    else:
        msg = {"target": target, "action": action}
    msg_json = json.dumps(msg)
    logging.info("<< " + msg_json)
    ws.send(msg_json)

def on_error(ws, error):
    print(error)

def on_close(ws):
    print("### closed ###")

def on_open(ws):
    print("### opened ###")
    logging.info("Getting GenesisBlock")
    get_block_by_height(ws, 0)
    logging.info("Getting GenesisHeader")
    get_header_by_height(ws, 0)


## WS API functions
def get_block_by_height(ws, height):
    ws_send(ws, "chain", "get", {"type": "block", "height": height}) 

def get_block_by_hash(ws, hash):
    ws_send(ws, "chain", "get", {"type": "block", "hash": hash}) 

def get_header_by_height(ws, height):
    ws_send(ws, "chain", "get", {"type": "header", "height": height}) 

def get_header_by_hash(ws, hash):
    ws_send(ws, "chain", "get", {"type": "header", "hash": hash}) 



if __name__ == "__main__":
    port, handler, trace = read_argv(sys.argv)
    logic = importlib.import_module(handler)
    MESSAGE_FUNCTIONS = logic.actions()
    websocket.enableTrace(trace)
    ws = websocket.WebSocketApp("ws://127.0.0.1:" + str(port) + "/websocket",
                              on_message = on_message,
                              on_error = on_error,
                              on_close = on_close)
    ws.on_open = on_open
    ws.run_forever()
