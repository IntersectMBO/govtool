import os
import subprocess
import time

import dotenv

BUILD_ID = int(time.time())
CURRENT_GIT_HASH = str(subprocess.check_output(["git", "rev-parse", "HEAD"]), "utf-8").strip()

dotenv.load_dotenv()

METRICS_API_SECRET = os.getenv("METRICS_API_SECRET")
KUBER_API_URL = os.getenv("KUBER_URL") or f'https://{os.getenv("NETWORK","preview")}.kuber.cardanoapi.io'
KUBER_API_KEY = os.getenv("KUBER_API_KEY")
FAUCET_API_URL = os.getenv("FAUCET_API_URL") or f'https://faucet.{os.getenv("NETWORK","preview")}.world.dev.cardano.org'
FACUET_API_KEY = os.getenv("FAUCET_API_KEY")
