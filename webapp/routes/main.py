"""Main routes for the chore allocation application."""

import requests
from flask import Blueprint, current_app, render_template, request

# Create the main blueprint
main_bp = Blueprint("main", __name__)


@main_bp.route("/")
def index() -> str:
    """Display the main page with agent and chore input forms.

    Returns:
        Rendered HTML template for the main page
    """
    return render_template("index.html", agents=[], chores=[], result=None)


@main_bp.route("/calculate", methods=["POST"])
def calculate() -> str:
    """Receive agents and chores lists, forward to Web API for allocation.

    The form submits two lists:
    - agents: List of agent names (people)
    - chores: List of chore names (tasks)

    Returns:
        Rendered template with the allocation result (or error)
    """
    # Get the lists from the form submission
    agents = request.form.getlist("agents")
    chores = request.form.getlist("chores")

    # Validate that at least one agent and one chore exist
    errors = []
    if not agents:
        errors.append("At least one agent is required")
    if not chores:
        errors.append("At least one chore is required")

    if errors:
        result = {
            "status": "error",
            "message": ". ".join(errors) + ".",
            "agents_count": len(agents),
            "chores_count": len(chores),
        }
        return render_template("index.html", agents=agents, chores=chores, result=result)

    # Forward to Allocation Calculator API
    api_url = current_app.config["CALCULATOR_URL"] + "/allocate"
    try:
        response = requests.post(
            api_url,
            json={"agents": agents, "chores": chores},
            timeout=30,
        )
        response.raise_for_status()
        api_result = response.json()

        result = {
            "status": api_result.get("status", "unknown"),
            "message": api_result.get("message", ""),
            "agents_count": len(agents),
            "chores_count": len(chores),
            "api_response": api_result,
        }
    except requests.exceptions.ConnectionError:
        result = {
            "status": "error",
            "message": "Could not connect to Allocation Calculator. Is it running?",
            "agents_count": len(agents),
            "chores_count": len(chores),
        }
    except requests.exceptions.Timeout:
        result = {
            "status": "error",
            "message": "Allocation Calculator request timed out.",
            "agents_count": len(agents),
            "chores_count": len(chores),
        }
    except requests.exceptions.RequestException as e:
        result = {
            "status": "error",
            "message": f"API error: {e}",
            "agents_count": len(agents),
            "chores_count": len(chores),
        }

    return render_template("index.html", agents=agents, chores=chores, result=result)
