# Docker Guide - COBOL Migration Agents

This guide explains how to use COBOL Migration Agents in Docker containers for simple and isolated deployment.

## 🐳 Docker Configuration

### Prerequisites

- Docker 20.10+
- Docker Compose 2.0+ (or docker-compose 1.29+)
- 4GB+ available RAM
- 10GB+ free disk space

### Verify Docker Installation

```bash
# Check Docker
docker --version
docker compose version

# Test Docker
docker run hello-world
```

## 🚀 Quick Start

### 1. Automated Setup

```bash
# Make setup script executable
chmod +x scripts/docker-setup.sh

# Run complete setup
./scripts/docker-setup.sh setup
```

### 2. Manual Configuration

```bash
# Create main configuration file
cp config/settings.env.example config/settings.local.env

# Edit with your AI credentials
nano config/settings.local.env

# Optional: Create Docker-specific configuration
cp .env.example .env
nano .env
```

Example `config/settings.local.env` configuration:

```env
# Azure OpenAI
AI_SERVICE_TYPE=AzureOpenAI
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-api-key-here
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4.1
AZURE_OPENAI_MODEL_ID=gpt-4.1
AZURE_OPENAI_API_VERSION=2025-04-01-preview

# Or OpenAI
AI_SERVICE_TYPE=OpenAI
AZURE_OPENAI_API_KEY=sk-your-openai-key-here
AZURE_OPENAI_MODEL_ID=gpt-4.1

# Instructor Mode (optional)
INSTRUCTOR_MODE=instructor.Mode.OPENROUTER_STRUCTURED_OUTPUTS
```

Example `.env` configuration (Docker-specific):

```env
# Docker-specific settings
TZ=UTC
DEVELOPMENT_MODE=false
```

> 📋 **Complete Parameter Reference**: For detailed explanations of all configuration parameters, see [PARAMETERS.md](PARAMETERS.md)

### 3. Build and Start

```bash
# Build the image
docker compose build

# Start the container
docker compose up -d

# Verify it's running
docker compose ps
```

## 📁 Directory Structure

The Docker system mounts the following directories:

```
./data/
├── cobol-source/     # Input COBOL files (read-only mount)
├── java-output/      # Generated Java files (read-write mount)
└── logs/            # System logs (read-write mount)

./config/
├── settings.local.env    # Your local configuration (copy from template)
└── settings.env.example # Configuration template (do not edit)

./.env                    # Docker-specific settings (optional)
./.env.example           # Docker template (do not edit)
```

## 🐳 Docker Environments

### Production Environment (Default)
Optimized for production deployment with self-contained images:

```bash
# Build and start production containers
docker compose up --build
```

**Production Features:**
- Self-contained images (no source code mounts)
- Optimized performance (no file system overhead)
- Secure deployment ready
- Minimal attack surface

### Development Environment  
For development with hot reload capabilities:

```bash
# Build and start with development overrides
docker compose -f docker-compose.yml -f docker-compose.dev.yml up --build
```

**Development Features:**
- **Hot Reload**: Source code changes reflected immediately
- **Development Mounts**: Access to local files and scripts  
- **Debug Configuration**: Enhanced logging and debugging
- **Faster Iteration**: No container rebuild required for code changes

## 🛠️ Main Commands

### Setup and Configuration

```bash
# Interactive setup
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli setup

# Configuration validation
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli validate

# System diagnostics
docker compose run --rm cobol-migration python scripts/doctor.py check
```

### COBOL Migration

```bash
# Basic migration
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli main \
  --cobol-source /app/data/cobol-source \
  --java-output /app/data/java-output

# Migration with verbose output
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli main \
  --cobol-source /app/data/cobol-source \
  --java-output /app/data/java-output \
  --verbose

# Migration of specific folder
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli main \
  --cobol-source /app/data/cobol-source/legacy \
  --java-output /app/data/java-output/converted
```

### Log and Report Generation

```bash
# Generate conversation log
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli conversation

# Conversation log for specific session
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli conversation \
  --session-id migration_20240101_120000

# Conversation log with custom output
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli conversation \
  --output /app/data/logs
```

### Interactive Shell

```bash
# Open shell in container
docker compose run --rm cobol-migration bash

# Open shell with development mounts
docker compose -f docker-compose.yml -f docker-compose.dev.yml run --rm cobol-migration bash
```

## 📝 Helper Script

The `scripts/docker-setup.sh` script provides utility commands:

```bash
# Complete setup command
./scripts/docker-setup.sh setup

# Build images only
./scripts/docker-setup.sh build

# Configuration validation
./scripts/docker-setup.sh validate

# System diagnostics
./scripts/docker-setup.sh doctor

# Create sample COBOL files
./scripts/docker-setup.sh samples

# Test migration
./scripts/docker-setup.sh migrate

# Interactive shell
./scripts/docker-setup.sh shell

# Show logs
./scripts/docker-setup.sh logs

# Complete cleanup
./scripts/docker-setup.sh clean
```

## 💡 Usage Examples

### Complete Migration

```bash
# 1. Preparation
./scripts/docker-setup.sh setup
./scripts/docker-setup.sh samples

# 2. Configuration (edit .env with your credentials)
nano .env

# 3. Validation
./scripts/docker-setup.sh validate

# 4. Migration
./scripts/docker-setup.sh migrate

# 5. Check results
ls -la data/java-output/
cat data/java-output/migration_report_*.md
```

### Existing Project Migration

```bash
# 1. Copy your COBOL files
cp -r /path/to/your/cobol/* ./data/cobol-source/

# 2. Configure the system
./scripts/docker-setup.sh validate

# 3. Run migration
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli main \
  --cobol-source /app/data/cobol-source \
  --java-output /app/data/java-output \
  --verbose

# 4. Check results
docker compose run --rm cobol-migration find /app/data/java-output -name "*.java"
```

### Debug and Troubleshooting

```bash
# Open shell for debugging
docker compose run --rm cobol-migration bash

# Check configuration
python -m cobol_migration_agents.cli validate

# Test AI service connectivity
python -c "from cobol_migration_agents.config.settings import Settings; print(Settings.from_env().ai_settings)"

# Check detailed logs
tail -f data/logs/*.log

# Complete diagnostics
python scripts/doctor.py check
```

## 🔍 Monitoring and Logs

### Log Viewing

```bash
# Container logs
docker compose logs -f cobol-migration

# Application logs
tail -f data/logs/migration_*.log

# Conversation logs
cat data/logs/conversation_*.md
```

### Performance Metrics

```bash
# Container statistics
docker stats cobol-migration

# Disk usage
du -sh data/

# Detailed container information
docker compose run --rm cobol-migration python scripts/doctor.py info
```

## 🛡️ Security

### Credential Management

- **Never commit** the `.env` file to the repository
- Use system environment variables for CI/CD
- Consider using Docker secrets for production

### Isolation

- Container runs with non-root user (`appuser`)
- Directory mounts only where necessary
- Isolated network for containers

### Backup

```bash
# Backup configuration
cp .env .env.backup
cp config/settings.local.env config/settings.local.env.backup

# Backup data
tar -czf backup_$(date +%Y%m%d).tar.gz data/

# Backup Docker images
docker save cobol-migration-agents:latest | gzip > cobol-migration-image.tar.gz
```

## 🔧 Troubleshooting

### Common Issues

#### 1. Permission Error

```bash
# Fix permissions
sudo chown -R $USER:$USER data/
chmod -R 755 data/
```

#### 2. Container won't start

```bash
# Check logs
docker compose logs cobol-migration

# Rebuild image
docker compose build --no-cache cobol-migration
```

#### 3. Invalid configuration

```bash
# Validate configuration
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli validate

# Reset configuration
rm .env
cp .env.example .env
```

#### 4. Insufficient disk space

```bash
# Docker cleanup
docker system prune -af

# Remove unused images
docker image prune -af

# Complete project cleanup
./scripts/docker-setup.sh clean
```

### Advanced Debug

```bash
# Shell with debug mode
docker compose run --rm -e LOGGING_LEVEL=DEBUG cobol-migration bash

# Direct filesystem access
docker compose run --rm cobol-migration ls -la /app

# Test connectivity
docker compose run --rm cobol-migration ping google.com

# Check environment variables
docker compose run --rm cobol-migration env | grep AZURE
```

## 🚀 Production Deployment

### Docker Swarm

```bash
# Initialize swarm
docker swarm init

# Deploy stack
docker stack deploy -c docker-compose.yml cobol-migration

# Check services
docker service ls
```

### Kubernetes (Helm)

```bash
# Generate Kubernetes manifests
# (requires additional configuration)
helm template cobol-migration ./k8s/helm-chart/
```

### CI/CD Pipeline

Example GitHub Actions:

```yaml
name: Build and Deploy
on:
  push:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Build Docker image
        run: docker build -t cobol-migration:${{ github.sha }} .
      - name: Test container
        run: docker run --rm cobol-migration:${{ github.sha }} python -m cobol_migration_agents.cli --help
```

## 📚 Additional Resources

- [Docker Best Practices](https://docs.docker.com/develop/dev-best-practices/)
- [Docker Compose Reference](https://docs.docker.com/compose/compose-file/)
- [Container Security](https://docs.docker.com/engine/security/)

## 🆘 Support

For Docker-specific issues:

1. Check logs: `docker compose logs`
2. Verify configuration: `./scripts/docker-setup.sh validate`
3. Run diagnostics: `./scripts/docker-setup.sh doctor`
4. Consult the main project documentation

---

*Docker Guide for COBOL Migration Agents - Container-ready version of the AI-powered migration system*