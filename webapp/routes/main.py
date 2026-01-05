"""Main routes for the chore allocation application."""

from flask import Blueprint, render_template, request

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

    # TODO: Forward to Web API when it's implemented
    # For now, just echo back the input
    result = {
        "status": "pending",
        "message": "Web API integration coming soon. Received your input.",
        "agents_count": len(agents),
        "chores_count": len(chores),
    }

    return render_template("index.html", agents=agents, chores=chores, result=result)
