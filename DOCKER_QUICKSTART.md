# 🐳 Docker Quick Start - COBOL Migration Agents

Quick guide to get started with the Docker version of the COBOL Migration system.

## ⚡ Ultra-Fast Setup

```bash
# 1. Complete automated setup
./scripts/docker-setup.sh setup

# 2. Configure AI credentials (edit .env)
nano .env

# 3. Test and migrate
./scripts/docker-setup.sh validate
./scripts/docker-setup.sh samples
./scripts/docker-setup.sh migrate
```

## 🚀 docker-setup.sh Script Commands

The `docker-setup.sh` script provides all necessary Docker operations:

### Setup Commands

```bash
# Complete initial setup (directories, config, build)
./scripts/docker-setup.sh setup

# Build Docker images only
./scripts/docker-setup.sh build

# Validate configuration
./scripts/docker-setup.sh validate

# Run system diagnostics
./scripts/docker-setup.sh doctor
```

### Migration Commands

```bash
# Create sample COBOL files for testing
./scripts/docker-setup.sh samples

# Run test migration
./scripts/docker-setup.sh migrate
```

### Development Commands

```bash
# Open interactive shell in container
./scripts/docker-setup.sh shell

# Show real-time logs
./scripts/docker-setup.sh logs

# Complete cleanup (containers, images, volumes)
./scripts/docker-setup.sh clean
```

### Help

```bash
# Show all available commands
./scripts/docker-setup.sh help
```

## ⚙️ Configuration

### Quick .env Setup

```env
# Azure OpenAI (recommended)
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
```

## 📂 Directory Structure

```
./data/
├── cobol-source/     # 📥 Input COBOL files
├── java-output/      # 📤 Generated Java files
└── logs/            # 📋 System logs

./config/
├── settings.local.env  # 🔧 Local configuration
└── settings.env.example  # 📋 Configuration template
```

## 🎯 Complete Example Workflow

```bash
# Setup in 5 minutes
git clone <repository>
cd cobol_migration_agents

# 1. Automated setup
./scripts/docker-setup.sh setup

# 2. Configure credentials (replace with your values)
cat > .env << EOF
AI_SERVICE_TYPE=AzureOpenAI
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-api-key-here
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4.1
AZURE_OPENAI_MODEL_ID=gpt-4.1
AZURE_OPENAI_API_VERSION=2025-04-01-preview
EOF

# 3. Validate and test
./scripts/docker-setup.sh validate
./scripts/docker-setup.sh samples
./scripts/docker-setup.sh migrate

# 4. Check results
ls -la data/java-output/
cat data/java-output/migration_report_*.md
```

## 🐳 Docker Environment Types

### Production (Default)
```bash
# Optimized for production
docker compose up --build
```

### Development (Hot Reload)
```bash
# Development with source code mounting
docker compose -f docker-compose.yml -f docker-compose.dev.yml up --build
```

## 🔍 Verification Commands

```bash
# Verify Docker setup
docker images | grep cobol-migration

# Test Python import
docker compose run --rm cobol-migration python -c "import cobol_migration_agents; print('OK')"

# Test CLI commands
docker compose run --rm cobol-migration python -m cobol_migration_agents.cli --help

# Verify configuration
./scripts/docker-setup.sh validate
```

## 🛟 Quick Troubleshooting

### Build Issues
```bash
# Clean rebuild
docker system prune -af
./scripts/docker-setup.sh build
```

### Permissions
```bash
# Fix data directory permissions
sudo chown -R $USER:$USER data/
chmod -R 755 data/
```

### Configuration Issues
```bash
# Reset configuration
rm .env
cp .env.example .env
# Edit .env with your credentials
```

### Debug
```bash
# Detailed logs
docker compose logs -f cobol-migration

# Interactive shell for debugging
./scripts/docker-setup.sh shell

# Test connectivity
docker compose run --rm cobol-migration ping google.com
```

## 📊 Expected Output

After successful migration, you'll see:

```
data/java-output/
├── com/example/calc/
│   ├── CalcStructures.java
│   └── SimpleCalcService.java
├── migration_report_migration_YYYYMMDD_HHMMSS.md
└── conversation_log_migration_YYYYMMDD_HHMMSS.md
```

## 🚀 Next Steps

1. **Add your COBOL files** to `data/cobol-source/`
2. **Run migration** with `./scripts/docker-setup.sh migrate`
3. **Review Java output** in `data/java-output/`
4. **Check migration reports** for detailed analysis
5. **Customize agents** for specific project needs

---

🎉 **System ready!** COBOL Migration Agents now runs perfectly in Docker with:

- ✅ Optimized and secure build
- ✅ Simplified configuration
- ✅ Automation scripts
- ✅ Debug and troubleshooting tools
- ✅ Production-ready deployment

*For detailed documentation, see DOCKER_GUIDE.md*