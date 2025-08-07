#!/bin/bash

# Default container service name
DEFAULT_SERVICE="cobol-migration"

# Show help message
show_help() {
    cat << EOF
COBOL Migration Agents - Docker Runner

Usage: $0 [OPTIONS] [COMMAND]

OPTIONS:
    --help          Show this help message
    --dev           Use development configuration (docker-compose.dev.yml)

EXAMPLES:
    $0 --help                                    # Show this help
    $0 bash                                      # Open shell in container
    $0 cobol-migrate --help                      # Show migration help
    $0 cobol-migrate main \\                     # Run migration
        --cobol-source /app/data/cobol-source \\
        --java-output /app/data/java-output --verbose
    $0 --dev bash                               # Use dev config with shell

CONTAINER SERVICE:
    Uses '$DEFAULT_SERVICE' service automatically

For more information, see README.md or run:
    $0 cobol-migrate --help

EOF
}

# Check for help flag or no arguments
if [[ $# -eq 0 ]] || [[ "$1" == "--help" ]] || [[ "$1" == "-h" ]]; then
    show_help
    exit 0
fi

# Check if docker-compose is available
if command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE_CMD="docker-compose"
elif command -v docker &> /dev/null && docker compose version &> /dev/null; then
    DOCKER_COMPOSE_CMD="docker compose"
else
    echo "Error: Docker Compose is required but not found." >&2
    echo "Please install Docker Compose to use this script." >&2
    exit 1
fi

# Check for --dev flag
if [[ "$1" == "--dev" ]]; then
    COMPOSE_FILE="./docker-compose.dev.yml"
    shift  # Remove the --dev flag
    echo "Using development configuration: $COMPOSE_FILE"
else
    COMPOSE_FILE="./docker-compose.yml"
fi

# Check if compose file exists
if [[ ! -f "$COMPOSE_FILE" ]]; then
    echo "Error: Compose file '$COMPOSE_FILE' not found." >&2
    exit 1
fi

# Run the docker-compose command with default service
exec ${DOCKER_COMPOSE_CMD} -f "$COMPOSE_FILE" run --rm "$DEFAULT_SERVICE" "$@"
