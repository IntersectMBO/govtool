GovTool backend PyTest
=================
This test is responsible for following

- Perform basic tests on GovTool backend endpoints.
- Publish the reports to metrics server for visualization.

## Installation

```shell
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```


## Wallet Setup
Test requires that certain dreps/stakes be registered in the network.

To run setup script the main wallet must have enough balance. The address for the main wallet is

> addr_test1qzc97zml9xzhm7xcsmqretkk7ztzyehj3dpd7ph7h0s40wp0ea9f3e353pmmr7yxv7m2dj09rn44m7pvd0m4cylusn8szlm75t

Run the setup script as follow.
```bash
export KUBER_API_URL="..."
export KUBER_API_KEY="..." # if not self hosting.
python3 ./setup.py
```
This will generate test_data.json that will be used to run tests.


## Run tests
In the root directory of tests/govtool-backend run the following command
```shell
export BASE_URL="url" # server's url e.g. https://staging.govtool.byron.network/api"
export METRICS_URL="url" # metrics server Url
export METRICS_API_SECRET="metrics-api-secret"
pytest -v
```
