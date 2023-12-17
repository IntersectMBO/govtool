import os
import subprocess
import time

import dotenv

BUILD_ID = int(time.time())
CURRENT_GIT_HASH = str(subprocess.check_output(["git", "rev-parse", "HEAD"]), "utf-8").strip()

dotenv.load_dotenv()

RECORD_METRICS_API = os.getenv("RECORD_METRICS_API")
METRICS_API_SECRET= os.getenv("METRICS_API_SECRET")
KUBER_API_URL  = os.getenv("KUBER_API_URL")
KUBER_API_KEY= os.getenv("KUBER_API_KEY")

