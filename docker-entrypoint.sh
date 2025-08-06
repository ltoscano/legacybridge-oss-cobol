#!/bin/bash

# Docker entrypoint script for COBOL Migration Agents

set -e

# Function to log messages
log() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $1"
}

# Wait for dependencies (if any)
wait_for_dependencies() {
    log "Checking dependencies..."
    # Add any dependency checks here (databases, external services, etc.)
}

# Initialize the application
initialize_app() {
    log "Initializing COBOL Migration Agents..."
    
    # Create necessary directories
    mkdir -p /app/data/cobol-source
    mkdir -p /app/data/java-output
    mkdir -p /app/data/logs
    
    # Set proper permissions (skip read-only mounts)
    chown -R appuser:appuser /app/data/java-output 2>/dev/null || true
    chown -R appuser:appuser /app/data/logs 2>/dev/null || true
    # Skip cobol-source as it's mounted read-only
    
    # Validate configuration if not in setup mode
    if [ "$1" != "cobol-migrate-setup" ] && [ "$1" != "bash" ] && [ "$1" != "sh" ]; then
        log "Validating configuration..."
        if ! python -c "from cobol_migration_agents.config.settings import Settings; s = Settings.from_env(); missing = s.validate_required_settings(); exit(1 if missing else 0)" 2>/dev/null; then
            log "⚠️  Configuration validation failed. Run 'cobol-migrate-setup' to configure."
        else
            log "✅ Configuration is valid"
        fi
    fi
}

# Handle different run modes
handle_command() {
    case "$1" in
        "cobol-migrate")
            log "Starting COBOL migration..."
            exec "$@"
            ;;
        "cobol-migrate-setup")
            log "Starting interactive setup..."
            exec "$@"
            ;;
        "cobol-migrate-conversation")
            log "Generating conversation log..."
            exec "$@"
            ;;
        "bash"|"sh")
            log "Starting interactive shell..."
            exec "$@"
            ;;
        "python")
            log "Running Python script..."
            exec "$@"
            ;;
        *)
            if [ -z "$1" ]; then
                log "No command specified, showing help..."
                exec cobol-migrate --help
            else
                log "Running command: $*"
                exec "$@"
            fi
            ;;
    esac
}

# Main execution
main() {
    log "COBOL Migration Agents - Container Starting"
    log "=========================================="
    
    # Wait for any dependencies
    wait_for_dependencies
    
    # Initialize the application
    initialize_app "$@"
    
    # Handle the command
    handle_command "$@"
}

# Run main function with all arguments
main "$@"