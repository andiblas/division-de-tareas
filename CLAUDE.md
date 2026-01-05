# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a chore allocation platform that finds the most fair distribution of chores among agents. The system uses mathematical allocation algorithms to optimize fairness.

## Architecture

The system consists of three layers communicating via HTTP:

```
Web App (Flask/Python) → Web API (Flask/Python) → Allocation Calculator (R/Plumber)
```

- **Web App**: Presentation layer for gathering problem instances (agents, chores, value functions)
- **Web API**: Middleware that selects allocation algorithms based on input size, parses data, and coordinates with the calculator
- **Allocation Calculator**: R-based computation layer with multiple allocation strategies, exposed via Plumber HTTP API

## Technology Stack

- **Frontend/API**: Python with Flask
- **Allocation Engine**: R with Plumber for HTTP API exposure
