"""Application factory for the Flask web application."""

from flask import Flask

from webapp.config import config


def create_app(config_name: str = "development") -> Flask:
    """Create and configure the Flask application.

    This factory pattern allows creating multiple app instances with different
    configurations, which is useful for testing and running different environments.

    Args:
        config_name: The configuration to use ("development" or "production")

    Returns:
        A configured Flask application instance
    """
    app = Flask(__name__)

    # Load configuration from the config dictionary
    app.config.from_object(config[config_name])

    # Register blueprints (routes)
    from webapp.routes import register_blueprints

    register_blueprints(app)

    return app
