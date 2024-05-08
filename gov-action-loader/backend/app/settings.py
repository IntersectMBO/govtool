from pydantic import BaseModel


class Settings(BaseModel):
    kuber_api_url: str
    kuber_api_key: str = ""  # Default value is an empty string


settings = Settings(kuber_api_url="your_api_url_here")