#!/bin/bash

# Docker Setup Script for COBOL Migration Agents
# This script helps you set up and run the COBOL Migration system in Docker

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check if Docker is installed and running
check_docker() {
    log_info "Checking Docker installation..."
    
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed. Please install Docker first."
        exit 1
    fi
    
    if ! docker info &> /dev/null; then
        log_error "Docker is not running. Please start Docker first."
        exit 1
    fi
    
    log_success "Docker is installed and running"
}

# Check if Docker Compose is available
check_docker_compose() {
    log_info "Checking Docker Compose..."
    
    if docker compose version &> /dev/null; then
        COMPOSE_CMD="docker compose"
    elif docker-compose --version &> /dev/null; then
        COMPOSE_CMD="docker-compose"
    else
        log_error "Docker Compose is not available. Please install Docker Compose."
        exit 1
    fi
    
    log_success "Docker Compose is available: $COMPOSE_CMD"
}

# Create necessary directories
create_directories() {
    log_info "Creating necessary directories..."
    
    cd "$PROJECT_DIR"
    
    mkdir -p data/cobol-source
    mkdir -p data/java-output
    mkdir -p data/logs
    mkdir -p config
    
    log_success "Directories created"
}

# Setup environment file
setup_env_file() {
    log_info "Setting up environment file..."
    
    cd "$PROJECT_DIR"
    
    if [ ! -f .env ]; then
        if [ -f .env.example ]; then
            cp .env.example .env
            log_warning "Created .env file from .env.example"
            log_warning "Please edit .env file with your actual API credentials"
        else
            log_error ".env.example file not found"
            exit 1
        fi
    else
        log_info ".env file already exists"
    fi
}

# Setup configuration
setup_config() {
    log_info "Setting up configuration..."
    
    cd "$PROJECT_DIR"
    
    if [ ! -f config/settings.local.env ]; then
        if [ -f config/settings.env.example ]; then
            cp config/settings.env.example config/settings.local.env
            log_warning "Created config/settings.local.env from example"
            log_warning "Please edit this file with your actual API credentials"
        else
            log_warning "config/settings.env.example not found"
        fi
    else
        log_info "config/settings.local.env already exists"
    fi
}

# Build Docker images
build_images() {
    log_info "Building Docker images..."
    
    cd "$PROJECT_DIR"
    
    # Build with no cache to ensure fresh build
    $COMPOSE_CMD build --no-cache cobol-migration
    
    log_success "Docker images built successfully"
}

# Run setup wizard in container
run_setup_wizard() {
    log_info "Running setup wizard in container..."
    
    cd "$PROJECT_DIR"
    
    $COMPOSE_CMD run --rm cobol-migration cobol-migrate-setup
    
    log_success "Setup wizard completed"
}

# Validate configuration
validate_config() {
    log_info "Validating configuration..."
    
    cd "$PROJECT_DIR"
    
    if $COMPOSE_CMD run --rm cobol-migration python -m cobol_migration_agents.cli validate; then
        log_success "Configuration is valid"
    else
        log_error "Configuration validation failed"
        return 1
    fi
}

# Run system checks
run_doctor() {
    log_info "Running system diagnostics..."
    
    cd "$PROJECT_DIR"
    
    $COMPOSE_CMD run --rm cobol-migration python scripts/doctor.py check
}

# Create sample COBOL files
create_sample_files() {
    log_info "Creating sample COBOL files for testing..."
    
    cd "$PROJECT_DIR"
    
    # Create a simple COBOL program
    cat > data/cobol-source/HELLO-WORLD.cbl << 'EOF'
      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO-WORLD.
      AUTHOR. DOCKER-SETUP.
      DATE-WRITTEN. TODAY.
      
      ENVIRONMENT DIVISION.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-MESSAGE PIC X(30) VALUE 'HELLO WORLD FROM COBOL'.
      01 WS-COUNTER PIC 9(3) VALUE 0.
      
      PROCEDURE DIVISION.
      MAIN-LOGIC.
          DISPLAY 'Starting HELLO-WORLD program'.
          DISPLAY WS-MESSAGE.
          
          PERFORM VARYING WS-COUNTER FROM 1 BY 1 
                  UNTIL WS-COUNTER > 3
              DISPLAY 'Count: ' WS-COUNTER
          END-PERFORM.
          
          DISPLAY 'Program completed successfully'.
          STOP RUN.
EOF

    # Create a copybook
    cat > data/cobol-source/COMMON-DATA.cpy << 'EOF'
      * Common data definitions
      01 COMMON-CONSTANTS.
         05 MAX-RECORDS         PIC 9(5) VALUE 99999.
         05 COMPANY-NAME        PIC X(30) VALUE 'ACME CORPORATION'.
         05 VERSION-NUMBER      PIC X(10) VALUE 'V1.0.0'.
      
      01 CUSTOMER-RECORD.
         05 CUST-ID            PIC 9(10).
         05 CUST-NAME          PIC X(50).
         05 CUST-ADDRESS.
            10 STREET          PIC X(30).
            10 CITY            PIC X(20).
            10 STATE           PIC X(2).
            10 ZIP-CODE        PIC X(10).
         05 CUST-PHONE         PIC X(15).
         05 CUST-EMAIL         PIC X(50).
EOF

    log_success "Sample COBOL files created in data/cobol-source/"
}

# Print usage instructions
print_usage() {
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  setup      - Complete setup (default)"
    echo "  build      - Build Docker images only"
    echo "  validate   - Validate configuration"
    echo "  doctor     - Run system diagnostics"
    echo "  samples    - Create sample COBOL files"
    echo "  migrate    - Run a test migration"
    echo "  shell      - Open shell in container"
    echo "  logs       - Show container logs"
    echo "  clean      - Clean up containers and images"
    echo "  help       - Show this help"
}

# Run migration
run_migration() {
    log_info "Running test migration..."
    
    cd "$PROJECT_DIR"
    
    $COMPOSE_CMD run --rm cobol-migration python -m cobol_migration_agents.cli main \
        --cobol-source /app/data/cobol-source \
        --java-output /app/data/java-output \
        --verbose
}

# Open shell in container
open_shell() {
    log_info "Opening shell in container..."
    
    cd "$PROJECT_DIR"
    
    $COMPOSE_CMD run --rm cobol-migration bash
}

# Show logs
show_logs() {
    cd "$PROJECT_DIR"
    $COMPOSE_CMD logs -f cobol-migration
}

# Clean up
cleanup() {
    log_info "Cleaning up containers and images..."
    
    cd "$PROJECT_DIR"
    
    $COMPOSE_CMD down -v
    $COMPOSE_CMD rm -f
    docker system prune -f
    
    log_success "Cleanup completed"
}

# Main function
main() {
    local command="${1:-setup}"
    
    echo "🐳 COBOL Migration Agents - Docker Setup"
    echo "========================================"
    
    case "$command" in
        "setup")
            check_docker
            check_docker_compose
            create_directories
            setup_env_file
            setup_config
            build_images
            
            log_success "Setup completed!"
            log_info "Next steps:"
            echo "  1. Edit .env file with your AI service credentials"
            echo "  2. Run: $0 validate"
            echo "  3. Run: $0 samples (to create test files)"
            echo "  4. Run: $0 migrate (to test migration)"
            ;;
        "build")
            check_docker
            check_docker_compose
            build_images
            ;;
        "validate")
            check_docker
            check_docker_compose
            validate_config
            ;;
        "doctor")
            check_docker
            check_docker_compose
            run_doctor
            ;;
        "samples")
            create_sample_files
            ;;
        "migrate")
            check_docker
            check_docker_compose
            run_migration
            ;;
        "shell")
            check_docker
            check_docker_compose
            open_shell
            ;;
        "logs")
            check_docker
            check_docker_compose
            show_logs
            ;;
        "clean")
            check_docker
            check_docker_compose
            cleanup
            ;;
        "help"|"--help"|"-h")
            print_usage
            ;;
        *)
            log_error "Unknown command: $command"
            print_usage
            exit 1
            ;;
    esac
}

# Run main function
main "$@"