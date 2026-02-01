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

# Start R allocation calculator in background
echo "Starting R allocation calculator at http://127.0.0.1:8000"
cd calculator && Rscript run.R &
cd "$SCRIPT_DIR"

# Run the Flask server
echo "Starting Flask server at http://127.0.0.1:5001"
python run.py
