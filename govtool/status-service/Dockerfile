FROM python:3.11-alpine
WORKDIR /app
CMD ["python3", "-m", "gunicorn", "--bind=0.0.0.0", "--workers=4", "--access-logfile=-", "app:app"]
ENV GRAFANA_USERNAME admin
ENV GRAFANA_PASSWORD set-me-at-runtime

COPY requirements.txt ./requirements.txt
RUN pip install -r requirements.txt
COPY . ./
