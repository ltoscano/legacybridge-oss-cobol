#!/bin/bash

# Start services in detached mode
echo "Starting services with docker-compose..."
docker-compose up -d

# Check if services started successfully
if [ $? -eq 0 ]; then
    echo "Services started successfully."
else
    echo "Failed to start services." >&2
    exit 1
fi
