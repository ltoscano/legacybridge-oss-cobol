# Docker Guide - COBOL Migration Agents

Questa guida spiega come utilizzare COBOL Migration Agents in container Docker per un deployment semplice e isolato.

## 🐳 Configurazione Docker

### Prerequisiti

- Docker 20.10+
- Docker Compose 2.0+ (o docker-compose 1.29+)
- 4GB+ RAM disponibile
- 10GB+ spazio disco libero

### Verifica Installazione Docker

```bash
# Verifica Docker
docker --version
docker compose version

# Test Docker
docker run hello-world
```

## 🚀 Quick Start

### 1. Setup Automatico

```bash
# Rendi eseguibile lo script di setup
chmod +x scripts/docker-setup.sh

# Esegui setup completo
./scripts/docker-setup.sh setup
```

### 2. Configurazione Manuale

```bash
# Crea file di configurazione
cp .env.example .env

# Modifica con le tue credenziali AI
nano .env
```

Esempio configurazione `.env`:

```env
# Azure OpenAI
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

### 3. Build e Start

```bash
# Build dell'immagine
docker compose build

# Avvia il container
docker compose up -d

# Verifica che sia running
docker compose ps
```

## 📁 Struttura Directory

Il sistema Docker monta le seguenti directory:

```
./data/
├── cobol-source/     # File COBOL di input (mount read-only)
├── java-output/      # File Java generati (mount read-write)
└── logs/            # Log del sistema (mount read-write)

./config/
├── settings.local.env  # Configurazione locale
└── settings.env.example  # Template configurazione
```

## 🛠️ Comandi Principali

### Setup e Configurazione

```bash
# Setup interattivo
docker compose run --rm cobol-migration cobol-migrate-setup

# Validazione configurazione
docker compose run --rm cobol-migration cobol-migrate validate

# Diagnostica sistema
docker compose run --rm cobol-migration python scripts/doctor.py check
```

### Migrazione COBOL

```bash
# Migrazione base
docker compose run --rm cobol-migration cobol-migrate \
  --cobol-source /app/data/cobol-source \
  --java-output /app/data/java-output

# Migrazione con verbose
docker compose run --rm cobol-migration cobol-migrate \
  --cobol-source /app/data/cobol-source \
  --java-output /app/data/java-output \
  --verbose

# Migrazione di una cartella specifica
docker compose run --rm cobol-migration cobol-migrate \
  --cobol-source /app/data/cobol-source/legacy \
  --java-output /app/data/java-output/converted
```

### Generazione Log e Report

```bash
# Genera conversation log
docker compose run --rm cobol-migration cobol-migrate-conversation

# Conversation log per sessione specifica
docker compose run --rm cobol-migration cobol-migrate-conversation \
  --session-id migration_20240101_120000

# Conversation log con output personalizzato
docker compose run --rm cobol-migration cobol-migrate-conversation \
  --output /app/data/logs
```

### Shell Interattiva

```bash
# Apri shell nel container
docker compose run --rm cobol-migration bash

# Apri shell con mount della directory corrente (development)
docker compose run --rm cobol-migration-dev bash
```

## 🔧 Modalità di Utilizzo

### 1. Modalità Produzione (Default)

```bash
# Container ottimizzato per produzione
docker compose up cobol-migration
```

### 2. Modalità Development

```bash
# Container con tools di sviluppo
docker compose --profile dev up cobol-migration-dev

# Shell di sviluppo
docker compose --profile dev run --rm cobol-migration-dev bash
```

### 3. Modalità Web Interface (Future)

```bash
# Web interface (quando implementata)
docker compose --profile web up cobol-migration-web

# Accesso via browser
open http://localhost:8000
```

## 📝 Script Helper

Lo script `scripts/docker-setup.sh` fornisce comandi di utilità:

```bash
# Comando completo di setup
./scripts/docker-setup.sh setup

# Solo build delle immagini
./scripts/docker-setup.sh build

# Validazione configurazione
./scripts/docker-setup.sh validate

# Diagnostica sistema
./scripts/docker-setup.sh doctor

# Creazione file COBOL di esempio
./scripts/docker-setup.sh samples

# Test di migrazione
./scripts/docker-setup.sh migrate

# Shell interattiva
./scripts/docker-setup.sh shell

# Visualizza log
./scripts/docker-setup.sh logs

# Cleanup completo
./scripts/docker-setup.sh clean
```

## 💡 Esempi di Utilizzo

### Migrazione Completa

```bash
# 1. Preparazione
./scripts/docker-setup.sh setup
./scripts/docker-setup.sh samples

# 2. Configurazione (modifica .env con le tue credenziali)
nano .env

# 3. Validazione
./scripts/docker-setup.sh validate

# 4. Migrazione
./scripts/docker-setup.sh migrate

# 5. Verifica risultati
ls -la data/java-output/
cat data/java-output/migration_report_*.md
```

### Migrazione di Progetto Esistente

```bash
# 1. Copia i tuoi file COBOL
cp -r /path/to/your/cobol/* ./data/cobol-source/

# 2. Configura il sistema
./scripts/docker-setup.sh validate

# 3. Esegui migrazione
docker compose run --rm cobol-migration cobol-migrate \
  --cobol-source /app/data/cobol-source \
  --java-output /app/data/java-output \
  --verbose

# 4. Verifica risultati
docker compose run --rm cobol-migration find /app/data/java-output -name "*.java"
```

### Debug e Troubleshooting

```bash
# Apri shell per debug
docker compose run --rm cobol-migration bash

# Verifica configurazione
cobol-migrate validate

# Test connettività AI service
python -c "from cobol_migration_agents.config.settings import Settings; print(Settings.from_env().ai_settings)"

# Controlla log dettagliati
tail -f data/logs/*.log

# Diagnostica completa
python scripts/doctor.py check
```

## 🔍 Monitoring e Log

### Visualizzazione Log

```bash
# Log del container
docker compose logs -f cobol-migration

# Log applicazione
tail -f data/logs/migration_*.log

# Log conversation
cat data/logs/conversation_*.md
```

### Metriche di Performance

```bash
# Statistiche container
docker stats cobol-migration

# Utilizzo spazio disco
du -sh data/

# Informazioni dettagliate sul container
docker compose run --rm cobol-migration python scripts/doctor.py info
```

## 🛡️ Sicurezza

### Gestione Credenziali

- **Non committare mai** il file `.env` nel repository
- Usa variabili d'ambiente del sistema per CI/CD
- Considera l'uso di Docker secrets per produzione

### Isolamento

- Il container gira con utente non-root (`appuser`)
- Directory mount solo dove necessario
- Network isolato per i container

### Backup

```bash
# Backup configuration
cp .env .env.backup
cp config/settings.local.env config/settings.local.env.backup

# Backup data
tar -czf backup_$(date +%Y%m%d).tar.gz data/

# Backup immagini Docker
docker save cobol-migration-agents:latest | gzip > cobol-migration-image.tar.gz
```

## 🔧 Troubleshooting

### Problemi Comuni

#### 1. Errore di Permission

```bash
# Fix permessi
sudo chown -R $USER:$USER data/
chmod -R 755 data/
```

#### 2. Container non si avvia

```bash
# Verifica log
docker compose logs cobol-migration

# Rebuild immagine
docker compose build --no-cache cobol-migration
```

#### 3. Configurazione non valida

```bash
# Valida configurazione
docker compose run --rm cobol-migration cobol-migrate validate

# Reset configurazione
rm .env
cp .env.example .env
```

#### 4. Spazio disco insufficiente

```bash
# Cleanup Docker
docker system prune -af

# Rimuovi immagini non utilizzate
docker image prune -af

# Cleanup completo progetto
./scripts/docker-setup.sh clean
```

### Debug Avanzato

```bash
# Shell con debug mode
docker compose run --rm -e LOGGING_LEVEL=DEBUG cobol-migration bash

# Accesso diretto al filesystem container
docker compose run --rm cobol-migration ls -la /app

# Test connettività
docker compose run --rm cobol-migration ping google.com

# Verifica variabili d'ambiente
docker compose run --rm cobol-migration env | grep AZURE
```

## 🚀 Deploy in Produzione

### Docker Swarm

```bash
# Inizializza swarm
docker swarm init

# Deploy stack
docker stack deploy -c docker-compose.yml cobol-migration

# Verifica servizi
docker service ls
```

### Kubernetes (Helm)

```bash
# Genera manifesti Kubernetes
# (richiede configurazione aggiuntiva)
helm template cobol-migration ./k8s/helm-chart/
```

### CI/CD Pipeline

Esempio GitHub Actions:

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
        run: docker run --rm cobol-migration:${{ github.sha }} cobol-migrate --help
```

## 📚 Risorse Aggiuntive

- [Docker Best Practices](https://docs.docker.com/develop/dev-best-practices/)
- [Docker Compose Reference](https://docs.docker.com/compose/compose-file/)
- [Container Security](https://docs.docker.com/engine/security/)

## 🆘 Supporto

Per problemi specifici a Docker:

1. Controlla i log: `docker compose logs`
2. Verifica la configurazione: `./scripts/docker-setup.sh validate`
3. Esegui diagnostica: `./scripts/docker-setup.sh doctor`
4. Consulta la documentazione del progetto principale

---

*Guida Docker per COBOL Migration Agents - Versione container-ready del sistema di migrazione AI-powered*