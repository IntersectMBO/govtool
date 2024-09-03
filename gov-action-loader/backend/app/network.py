from fastapi import HTTPException
from app.settings import settings 

NETWORKS = {"preview", "preprod", "sanchonet"}

def get_api_url (network): 
    network_l_case = network.lower()
    if network_l_case == "sancho" or "sanchonet":
        return settings.kuber_api_url_sancho
    elif network_l_case == "preview": 
        return settings.kuber_api_url_preview
    elif network_l_case == "preprod":
        return settings.kuber_api_url_preprod 
    else: 
        raise HTTPException(status_code=400, detail=f"Invalid network: {network}. Expected: {NETWORKS}")