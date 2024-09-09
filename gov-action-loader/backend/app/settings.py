from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    kuber_api_url_sancho: str
    kuber_api_url_preview: str
    kuber_api_url_preprod: str
    kuber_api_key: str = ''

settings = Settings()
