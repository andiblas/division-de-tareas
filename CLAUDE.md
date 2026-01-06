# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a chore allocation platform that finds the most fair distribution of chores among agents. The system uses mathematical allocation algorithms to optimize fairness.

## Architecture

The system consists of two layers communicating via HTTP:

```
Web App (Flask/Python) → Allocation Calculator (R/Plumber)
```

- **Web App**: Presentation layer for gathering problem instances (agents, chores, value functions)
- **Allocation Calculator**: R-based computation layer with multiple allocation strategies, exposed via Plumber HTTP API

## Technology Stack

- **Frontend/API**: Python with Flask
- **Allocation Engine**: R with Plumber for HTTP API exposure

## Development Commands

```bash
# Create and activate virtual environment
python -m venv venv
source venv/bin/activate  # On macOS/Linux

# Install dependencies
pip install -r requirements.txt

# Run the development server
python run.py
# Visit http://127.0.0.1:5000
```

## Web App Structure

```
webapp/
├── __init__.py          # Application factory (create_app)
├── config.py            # Configuration classes
├── routes/
│   ├── __init__.py      # Blueprint registration
│   └── main.py          # Main routes (/, /calculate)
├── templates/
│   ├── base.html        # Base layout template
│   └── index.html       # Main form page
└── static/
    ├── css/style.css    # Styles
    └── js/app.js        # Client-side list management
```

## Key Patterns

- **Application Factory**: `create_app()` in `webapp/__init__.py`
- **Blueprints**: Routes organized in `webapp/routes/`
- **Type hints**: Used throughout for readability
