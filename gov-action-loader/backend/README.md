Gov Action Loader API
===================

This repository helps in creation of  Conway era transactions containing proposals. It contains specific proposal submitter and bulk proposal submitter. The proposal json is passed from the client it just forwards the transaction to kuber and returns txid.

### Depends On
- cardano-cli (only for wallet generation)
- [kuber-server](https://github.com/dquadrant/kuber)
- Sanchonet Faucet API

### Limitations
Gov action loader backend instance uses fixed set of wallet to perform transactions. This means that gov action loader can be used by only 1 user at a time. 


## 1. Setup 

```
python3 -m venv ./venv
. ./venv/bin/activate
pip install -r requirements.txt
```

## 1. Initialize Wallets

### Prerequisite 
    - You should have sancho-node, kuber server runnning and cardano-cli available.

For bulk proposal creation , multiple wallets must be generated. Generate them with following command.

```
python3 wallets.py
```

## 2. Start the server

- Add .env file with all the keys from .env.example
- Run `uvicorn app.main:app --reload --env-file .env` in the repository to run the program