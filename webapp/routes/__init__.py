"""Blueprint registration for the Flask application."""

from flask import Flask

from webapp.routes.main import main_bp


def register_blueprints(app: Flask) -> None:
    """Register all blueprints with the application.

    Args:
        app: The Flask application instance
    """
    app.register_blueprint(main_bp)
