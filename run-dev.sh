#!/bin/bash

set -e

COMPOSE_FILE="./docker-compose.dev.yml"

if [ ! -f "$COMPOSE_FILE" ]; then
    echo "Error: $COMPOSE_FILE does not exist."
    exit 1
fi

echo "Starting Docker Compose services in development mode..."
docker-compose -f "$COMPOSE_FILE" up -d

if [ $? -eq 0 ]; then
    echo "Services started successfully."
else
    echo "Error starting services."
    exit 1
fi
