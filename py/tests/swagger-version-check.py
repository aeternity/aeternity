#!/usr/bin/env python

# Check Swagger-generated files version match the epoch version in VERSION file.

import argparse, json, yaml, sys

parser = argparse.ArgumentParser(description='Checks if swagger versions match epoch version')
parser.add_argument('version_file', type=open)
parser.add_argument('yaml_file', type=open)
parser.add_argument('json_file', type=open)
args = parser.parse_args()

epoch_version = args.version_file.read().strip()
yaml_version = yaml.safe_load(args.yaml_file)['info']['version']
json_version = json.load(args.json_file)['info']['version']

print("Epoch version: %s" % epoch_version)
print("Swagger yaml version: %s" % yaml_version)
print("Swagger json version: %s" % json_version)

if yaml_version != epoch_version:
    sys.exit(1)

if json_version != epoch_version:
    sys.exit(1)
