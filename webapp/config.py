"""Configuration classes for the Flask application."""

import os
from typing import Type


class Config:
    """Base configuration shared by all environments."""

    # Secret key for session management and CSRF protection
    # In production, set this via environment variable
    SECRET_KEY: str = os.environ.get("SECRET_KEY", "dev-secret-key-change-in-production")

    # Web API URL (the allocation calculator API)
    API_BASE_URL: str = os.environ.get("API_BASE_URL", "http://localhost:8000")


class DevelopmentConfig(Config):
    """Configuration for local development."""

    DEBUG: bool = True


class ProductionConfig(Config):
    """Configuration for production deployment."""

    DEBUG: bool = False


# Dictionary to easily select configuration by name
config: dict[str, Type[Config]] = {
    "development": DevelopmentConfig,
    "production": ProductionConfig,
}
