# coding: utf-8
import json
import yaml
import argparse

from swagger_client.apis.external_api import ExternalApi
from swagger_client.api_client import ApiClient
from swagger_client.models.block import Block

def download(host, port, file_name):
    blocks = []
    api = ExternalApi(ApiClient(host= host + ':' + str(port) + '/v1'))
    top = api.get_top()
    height = top.height
    while height > -1:
        print("Downloading block with height = " + str(height))
        block = api.get_block_by_height(height)
        blocks.append(block.to_dict())
        height = height -1

    f = open(file_name, 'w')
    f.write(json.dumps(blocks))
    f.close()

def load_from_file(file_name):
    r = open(file_name, 'r')
    blocks = []
    for block_obj in json.loads(r.read()):
        blocks.append(Block(**block_obj))
    return blocks

def get_block(blocks, height=None):
    filters =[]
    if height != None:
        filters.append(lambda b: b.height == height)
    found = list(filter(lambda b: all(map(lambda f: f(b), filters)), blocks))
    if len(found) == 0:
        return None
    return found[0]

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Downloading the whole chain')
    parser.add_argument('--host', default="local",
                        help='Node to get blocks from')
    parser.add_argument('--port', type=int, default=3013,
                        help='Host\'s port number')
    parser.add_argument('--export_file', required=True, 
                        help='File to generate')
    args = parser.parse_args()
    download(args.host, args.port, args.export_file)

