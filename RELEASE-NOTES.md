
Here's how you can test this release...

1. Start release using `bin/epoch start`. This will start the node in the background. 

2. Ensure release is started. `bin/epoch ping` should return `pong`.

3. You won't see anything happening. Try to `tail -f log/epoch_mining.log` to see progress.

4. Run `curl http://127.0.0.1:3013/v1/top`. You should see this until a block is successfully mined

```
{"hash":"bTmIPBP7Eet2+2G7KEVg2JLn1/Oop1O5aQPpi61k9VY=","height":0,"nonce":0,"pow":"g2sAKgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==","prev_hash":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","state_hash":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","target":553713663,"time":0,"txs_hash":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=","version":1}
```

Check the height field. You should see if increasing every few minutes. Don't worry if you see `height: 0` for a while as it takes some time to solve the Cuckoo Cycle PoW.

You should see this once a block has been successfully mined

```
{"hash":"NNHDgbZq7s7Z+x/vh5LAE5hmYcwbKcxmxPxdA1k7yYs=","height":1,"nonce":533872846,"pow":"g2wAAAAqYgAVC+5iABpRB2IAOXicYgBKDXdiANHBRGIA06PMYgEfXSNiAWhWsmIBdPYVYgHQEj5iAgQLkmICRdWBYgJjVlBiAmyW0GIC+pJYYgNDf8xiA0kd4GIDkQCoYgOgPtNiA7FZgWIDtJQkYgQUgZ1iBDf+VmIEUjXVYgRaosViBNT58mIFIDlGYgVSzk9iBVM5M2IFdYAuYgV/AkJiBa/P7WIGYayiYgZiFRxiBnSfq2IGgdFQYga0MtRiBsgJfmIG75koYgcu7L1iB4qZsmIHoKY4ag==","prev_hash":"bTmIPBP7Eet2+2G7KEVg2JLn1/Oop1O5aQPpi61k9VY=","state_hash":"EjN6dNtQ9LL+kPmtG8utQw29jGSeFUNLVgS7TI/Z6jI=","target":553713663,"time":1506427530796,"txs_hash":"QjlWz7xRNGY3CWI/LC9W7wGNo5nTg2tAL1ZUWsroSEM=","version":1}
```

4. Query your balance using `curl http://127.0.0.1:3013/v1/account/balance`. Balance should increase as you are awarded tokens for each mined block. You should see `{"balance":10}`.

5. Please [file a ticket](https://github.com/aeternity/epoch/issues) you have a problem or suggestions for improvement!
 

