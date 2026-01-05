#!/bin/bash

# Start script for the chore allocation project
# This script handles virtual environment setup and runs the Flask server

set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    echo "Creating virtual environment..."
    python3 -m venv venv
fi

# Activate virtual environment
source venv/bin/activate

# Install dependencies if needed
pip install -q -r requirements.txt

# Run the server
echo "Starting server at http://127.0.0.1:5001"
python run.py
