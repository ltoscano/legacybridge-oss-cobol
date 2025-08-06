# Docker Troubleshooting Guide

## Problema risolto: "does not contain any element"

### ❌ Errore
```
/app/cobol_migration_agents does not contain any element
failed to solve: process "/bin/sh -c poetry install --only=main && rm -rf $POETRY_CACHE_DIR" did not complete successfully: exit code 1
```

### ✅ Soluzione
Il problema era che Poetry tentava di installare il pacchetto locale prima che il codice sorgente fosse copiato nel container.

### 🔧 Opzioni di Fix

#### Opzione 1: Dockerfile con pip (Raccomandato)
```bash
# Usa il Dockerfile semplificato
docker compose build --no-cache
```

Il progetto ora include:
- `Dockerfile.pip` - Versione semplificata con pip
- `requirements.txt` - Dipendenze estratte da pyproject.toml  
- `setup.py` - Setup script per compatibilità

#### Opzione 2: Dockerfile multi-stage (Avanzato)
Se preferisci usare Poetry, usa `Dockerfile` che ora copia il codice sorgente prima dell'installazione.

## Altri Problemi Comuni

### 🐳 Docker non trovato
```bash
# Installa Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sh get-docker.sh

# Avvia Docker
sudo systemctl start docker
sudo systemctl enable docker
```

### 🔑 Problemi di permessi
```bash
# Aggiungi utente al gruppo docker
sudo usermod -aG docker $USER
newgrp docker

# Riavvia Docker
sudo systemctl restart docker
```

### 💾 Spazio disco insufficiente
```bash
# Cleanup Docker
docker system prune -af
docker volume prune -f

# Verifica spazio
df -h
```

### 🌐 Problemi di rete
```bash
# Test connettività
docker run --rm alpine ping -c 4 google.com

# Reset network Docker
docker network prune -f
```

### 🔧 Build cache corrotto
```bash
# Build pulito
docker compose build --no-cache --pull

# Reset completo
docker system prune -af
docker compose build --no-cache
```

### ⚙️ Variabili d'ambiente non caricate
```bash
# Verifica file .env
cat .env

# Test variabili nel container
docker compose run --rm cobol-migration env | grep AZURE
```

### 📦 Dipendenze non trovate
```bash
# Verifica requirements.txt
cat requirements.txt

# Test installazione locale
pip install -r requirements.txt
```

## Comandi di Debug

### Verifica Build
```bash
# Build con output dettagliato
docker compose build --progress=plain --no-cache

# Analizza layer
docker history cobol-migration-agents:latest
```

### Debug Container
```bash
# Shell interattiva
docker compose run --rm cobol-migration bash

# Verifica installazione
docker compose run --rm cobol-migration pip list

# Test import
docker compose run --rm cobol-migration python -c "import cobol_migration_agents; print('OK')"
```

### Log Analysis
```bash
# Log di build
docker compose build 2>&1 | tee build.log

# Log runtime
docker compose logs -f cobol-migration

# Verifica health check
docker inspect cobol-migration-agents | grep -A 10 Health
```

## Soluzioni Alternative

### Modalità Development
Se hai problemi con il build di produzione, usa la modalità development:

```bash
# Development con Poetry
docker compose --profile dev up cobol-migration-dev

# Shell development
docker compose --profile dev run --rm cobol-migration-dev bash
```

### Installazione Locale
Per test rapidi, puoi anche eseguire localmente:

```bash
# Installazione locale
pip install -e .

# Test comando
cobol-migrate --help
```

### Mount del Codice
Per sviluppo rapido, monta il codice:

```bash
# Aggiungi a docker-compose.override.yml
services:
  cobol-migration:
    volumes:
      - .:/app
    command: ["tail", "-f", "/dev/null"]
```

## Verifica Finale

Dopo aver risolto i problemi, verifica che tutto funzioni:

```bash
# 1. Build pulito
./scripts/docker-setup.sh build

# 2. Test configurazione
./scripts/docker-setup.sh validate

# 3. Test completo
./scripts/docker-setup.sh doctor

# 4. Test migrazione
./scripts/docker-setup.sh migrate
```

## Supporto

Se i problemi persistono:

1. Verifica i log: `docker compose logs`
2. Controlla lo spazio disco: `df -h`
3. Verifica la versione Docker: `docker --version`
4. Consulta la documentazione: `./scripts/docker-setup.sh help`

---

*Per altri problemi, consulta il DOCKER_GUIDE.md per la documentazione completa.*