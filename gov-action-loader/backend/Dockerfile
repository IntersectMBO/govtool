FROM python:3.10 AS builder

WORKDIR /app
# Install cardano-cli

RUN  \
       wget https://github.com/input-output-hk/cardano-node/releases/download/8.5.0-pre/cardano-node-8.5.0-linux.tar.gz \
    && tar -xvzf cardano-node-8.5.0-linux.tar.gz \
    &&  mv ./cardano-cli /usr/local/bin/cardano-cli \
    && rm -rf /code/*

COPY ./requirements.txt ./requirements.txt
RUN pip install --no-cache-dir --upgrade -r ./requirements.txt
COPY . /app
EXPOSE 8000
CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000"]