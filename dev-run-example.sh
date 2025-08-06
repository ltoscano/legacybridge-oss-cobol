#!/bin/bash
set -euo pipefail

# Clean output and logs directories
rm -rf data/java-output/* data/logs/*

# Run the migration agent inside the Docker container
docker compose exec cobol-migration \
    python -m cobol_migration_agents.cli main \
        --cobol-source /app/data/cobol-source \
        --java-output /app/data/java-output \
        --verbose
