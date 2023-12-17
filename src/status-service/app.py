import requests
import json
from os import environ
from flask import Flask, render_template, request, json, Response

GRAFANA_USERNAME = environ['GRAFANA_USERNAME']
GRAFANA_PASSWORD = environ['GRAFANA_PASSWORD']

alert_health_mapping = {
    'inactive': 'healthy',
    'pending': 'warning',
    'firing': 'not healthy'
}
app = Flask(__name__)


def fetch_grafana_alert_rules(grafana_url):
    rules_endpoint = '/api/prometheus/grafana/api/v1/rules?limit_alerts=10'
    res = requests.get(grafana_url + rules_endpoint, auth=(GRAFANA_USERNAME, GRAFANA_PASSWORD))
    return res.text


@app.route('/status')
def status():
    my_host_header = request.headers['Host']
    grafana_url = 'https://' + my_host_header + '/grafana'
    app_health = {}
    rules = json.loads(fetch_grafana_alert_rules(grafana_url))
    if rules['status'] == 'success':
        for group in rules['data']['groups']:
            if group['name'] == 'status':
                for rule in group['rules']:
                    component_name = rule['labels']['app_component']
                    component_health = alert_health_mapping[rule['state']]
                    app_health[component_name] = component_health
        return render_template(
            'status.html',
            cardano_node_state=app_health['cardano-node'],
            cardano_db_sync_state=app_health['cardano-db-sync'],
            backend_state=app_health['backend'])
    else:
        return Response('failed to fetch rules from grafana', 500)
