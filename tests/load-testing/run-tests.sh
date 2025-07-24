#!/bin/bash
##
## Runs multiple stress tests with 
##
## frontend https://p80-z6b8d2f7a-ze34e4cb2-gtw.z937eb260.rustrocks.fr/
set -e
export API_URL=https://z6b8d2f7a-zca4a4c45-gtw.z937eb260.rustrocks.fr
export METADATA_VALIDATION_API_URL=https://z6b8d2f7a-z2f6a992f-gtw.z937eb260.rustrocks.fr
export PDF_API_URL=https://zdae9891f-zf09d11da-gtw.z937eb260.rustrocks.fr

export RAMP_DURATION=60 # in seconds
export STRESS_DURATION=60 # in seconds

run_load_test() {

  export PEAK_USERS=20
  (cd auth_script && npm run generate-auth)

  local peak_users=$1
  export PEAK_USERS=$peak_users
  (cd auth_script && npm run generate-auth)
  ./mvnw gatling:test

  
}

run_load_test 5
sleep 60 # wait for 60 secs
run_load_test 15
sleep 60 # wait for 60 secs
run_load_test 25
sleep 60 # wait for 60 secs
run_load_test 50
sleep 60 # wait for 60 secs
run_load_test 100
sleep 60 # wait for 60 secs
run_load_test 200
sleep 60 # wait for 60 secs
run_load_test 300
sleep 60 # wait for 60 secs
run_load_test 400