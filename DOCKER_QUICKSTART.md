# 🐳 Docker Quick Start - COBOL Migration Agents

Guida veloce per iniziare con la versione Docker del sistema.

## ⚡ Setup Ultra-Rapido

```bash
# 1. Setup automatico completo
./scripts/docker-setup.sh setup

# 2. Configura le credenziali AI (modifica .env)
nano .env

# 3. Test e migrazione
./scripts/docker-setup.sh validate
./scripts/docker-setup.sh samples
./scripts/docker-setup.sh migrate
```

## 🔧 Problema Risolto

**❌ Errore originale:**
```
/app/cobol_migration_agents does not contain any element
poetry install failed
```

**✅ Soluzione implementata:**
- Nuovo `Dockerfile.pip` che usa pip invece di Poetry
- `requirements.txt` per gestione dipendenze semplificata
- `setup.py` per installazione package
- `.dockerignore` ottimizzato
- Scripts di setup automatico

## 📁 File Docker Principali

```
cobol_migration_agents/
├── Dockerfile.pip              ✅ Produzione (raccomandato)
├── Dockerfile                  ⚙️  Multi-stage con Poetry (avanzato)
├── Dockerfile.dev              🔧 Development
├── docker-compose.yml          🚀 Orchestrazione
├── docker-entrypoint.sh        📋 Entrypoint intelligente
├── .env.example                🔑 Template configurazione
├── requirements.txt            📦 Dipendenze pip
├── setup.py                    ⚙️  Setup Python
└── scripts/docker-setup.sh     🛠️  Automazione completa
```

## 🚀 Utilizzo

### Comandi Base

```bash
# Build e start
docker compose build
docker compose up -d

# Migrazione diretta
docker compose run --rm cobol-migration cobol-migrate \
  --cobol-source /app/data/cobol-source \
  --java-output /app/data/java-output

# Setup interattivo
docker compose run --rm cobol-migration cobol-migrate-setup

# Shell per debug
docker compose run --rm cobol-migration bash
```

### Script Helper (Raccomandato)

```bash
./scripts/docker-setup.sh setup      # Setup completo
./scripts/docker-setup.sh build      # Solo build
./scripts/docker-setup.sh validate   # Test config
./scripts/docker-setup.sh doctor     # Diagnostica
./scripts/docker-setup.sh samples    # File test
./scripts/docker-setup.sh migrate    # Migrazione
./scripts/docker-setup.sh shell      # Shell interattiva
./scripts/docker-setup.sh logs       # Log
./scripts/docker-setup.sh clean      # Cleanup
```

## ⚙️ Configurazione .env

```env
# Azure OpenAI (raccomandato)
AI_SERVICE_TYPE=AzureOpenAI
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-api-key-here
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4
AZURE_OPENAI_MODEL_ID=gpt-4

# Oppure OpenAI
AI_SERVICE_TYPE=OpenAI
AZURE_OPENAI_API_KEY=sk-your-openai-key-here
AZURE_OPENAI_MODEL_ID=gpt-4
```

## 📂 Directory Structure

```
./data/
├── cobol-source/     # 📥 Input COBOL (mount read-only)
├── java-output/      # 📤 Output Java (mount read-write)
└── logs/            # 📋 System logs (mount read-write)

./config/
├── settings.local.env  # 🔧 Local config
└── settings.env.example  # 📋 Template
```

## 🔍 Verifiche

```bash
# Verifica build
docker images | grep cobol-migration

# Test import
docker compose run --rm cobol-migration python -c "import cobol_migration_agents; print('OK')"

# Test comandi CLI
docker compose run --rm cobol-migration cobol-migrate --help

# Verifica configurazione
docker compose run --rm cobol-migration cobol-migrate validate
```

## 🛟 Troubleshooting

### Build Fallisce
```bash
# Cleanup e rebuild
docker system prune -af
./scripts/docker-setup.sh build
```

### Permessi
```bash
# Fix permessi data
sudo chown -R $USER:$USER data/
chmod -R 755 data/
```

### Configurazione
```bash
# Reset configurazione
rm .env
cp .env.example .env
# Modifica .env con le tue credenziali
```

### Debug
```bash
# Log dettagliati
docker compose logs -f cobol-migration

# Shell per debug
./scripts/docker-setup.sh shell

# Test connettività
docker compose run --rm cobol-migration ping google.com
```

## 📚 Documentazione Completa

- `DOCKER_GUIDE.md` - Guida completa Docker
- `DOCKER_TROUBLESHOOTING.md` - Risoluzione problemi
- `README.md` - Documentazione progetto
- `PYTHON_MIGRATION_GUIDE.md` - Guida conversione da C#

## 🎯 Esempio Completo

```bash
# Setup completo in 5 minuti
git clone <repository>
cd cobol_migration_agents

# 1. Setup automatico
./scripts/docker-setup.sh setup

# 2. Configura credenziali (sostituisci con i tuoi valori)
cat > .env << EOF
AI_SERVICE_TYPE=AzureOpenAI
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
AZURE_OPENAI_API_KEY=your-api-key-here
AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4
AZURE_OPENAI_MODEL_ID=gpt-4
EOF

# 3. Valida e testa
./scripts/docker-setup.sh validate
./scripts/docker-setup.sh samples
./scripts/docker-setup.sh migrate

# 4. Controlla risultati
ls -la data/java-output/
cat data/java-output/migration_report_*.md
```

🎉 **Sistema pronto!** Il COBOL Migration Agents ora gira perfettamente in Docker con pieno supporto per:

- ✅ Build ottimizzato e sicuro
- ✅ Configurazione semplificata
- ✅ Scripts di automazione
- ✅ Debug e troubleshooting
- ✅ Produzione-ready

---

*Per supporto: consulta DOCKER_TROUBLESHOOTING.md o apri un issue*