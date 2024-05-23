from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    kuber_api_url: str
    kuber_api_key: str = ''

settings = Settings()
