from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    kuber_api_url: str
    kuber_api_key: str

    blockfrost_api_url: str
    blockfrost_project_id: str


settings = Settings()
