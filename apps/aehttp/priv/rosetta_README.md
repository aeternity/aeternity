The file `rosetta.yaml` was generated as shown below and copied to the current location.

```
git clone https://github.com/coinbase/rosetta-specifications.git

cd rosetta-specifications

docker pull swaggerapi/swagger-codegen-cli-v3

docker run --rm -v ${PWD}:/local swaggerapi/swagger-codegen-cli-v3 generate -i /local/api.json -l openapi-yaml -o /local/ -DoutputFile=rosetta.yaml
```
