# COBOL Migration Agents - Python Implementation Guide

This document describes the conversion of the C# COBOL Migration system to Python using the atomic-agents framework.

## Architecture Overview

The Python implementation leverages the atomic-agents framework to provide a more modular, schema-driven approach to AI agent management while maintaining all the functionality of the original C# system.

### Key Improvements

1. **Atomic Agents Framework**: Uses atomic-agents for standardized AI agent interactions with schema validation
2. **Pydantic Models**: Strong typing and validation for all data models
3. **Async/Await**: Full asynchronous support for better performance
4. **Rich CLI**: Beautiful command-line interface with progress bars and styling
5. **Modular Design**: Clear separation of concerns with service-oriented architecture

## Project Structure

```
cobol_migration_agents/
├── cobol_migration_agents/          # Main package
│   ├── __init__.py
│   ├── cli.py                       # Command-line interface
│   ├── migration_orchestrator.py    # Main orchestrator
│   ├── agents/                      # AI agents using atomic-agents
│   │   ├── __init__.py
│   │   ├── cobol_analyzer_agent.py
│   │   ├── java_converter_agent.py
│   │   └── dependency_mapper_agent.py
│   ├── config/                      # Configuration management
│   │   ├── __init__.py
│   │   └── settings.py
│   ├── models/                      # Data models
│   │   ├── __init__.py
│   │   ├── cobol_models.py
│   │   ├── java_models.py
│   │   ├── dependency_models.py
│   │   └── migration_schemas.py
│   └── services/                    # Helper services
│       ├── __init__.py
│       ├── file_manager.py
│       ├── logging_service.py
│       └── report_generator.py
├── config/                          # Configuration files
│   └── settings.env.example
├── scripts/                         # Setup and utility scripts
│   ├── setup.py
│   └── doctor.py
├── pyproject.toml                   # Poetry configuration
└── README.md
```

## Key Components

### 1. Migration Orchestrator (`migration_orchestrator.py`)

The main coordinator that manages the 6-step migration process:

- **File Discovery**: Scans for COBOL files
- **Dependency Analysis**: Maps relationships
- **COBOL Analysis**: Analyzes structure and complexity
- **Java Conversion**: Converts to Java Quarkus
- **File Generation**: Saves output files
- **Report Generation**: Creates comprehensive reports

### 2. AI Agents (`agents/`)

Three specialized agents using atomic-agents framework:

#### CobolAnalyzerAgent
- Uses atomic-agents BaseAgent with specialized system prompts
- Analyzes COBOL structure, complexity, and business logic
- Returns structured CobolAnalysis objects

#### JavaConverterAgent  
- Converts COBOL to Java Quarkus microservices
- Applies appropriate annotations (@ApplicationScoped, @Entity, etc.)
- Generates clean, maintainable Java code

#### DependencyMapperAgent
- Maps COBOL dependencies and relationships
- Generates Mermaid diagrams for visualization
- Identifies circular dependencies and migration risks

### 3. Data Models (`models/`)

Strong typing with Pydantic models:

- **CobolFile**: Represents COBOL source files
- **CobolAnalysis**: Analysis results with complexity metrics  
- **JavaFile**: Generated Java files with Quarkus metadata
- **DependencyMap**: Complete dependency relationships
- **Migration Schemas**: Input/output schemas for atomic-agents

### 4. Services (`services/`)

#### FileManager
- Async file operations for COBOL and Java files
- Directory organization and backup functionality
- File statistics and validation

#### LoggingService
- Comprehensive API call tracking
- Conversation logging for AI interactions
- Cost analysis and performance metrics
- Export capabilities for logs and statistics

#### ReportGenerator
- Markdown report generation
- Migration statistics and metrics
- File mapping and recommendations


## Configuration System

Uses Pydantic settings with environment variable support:

```python
from cobol_migration_agents.config.settings import Settings

# Load from environment variables and .env files
settings = Settings.from_env()

# Validate required settings
missing = settings.validate_required_settings()
```

### Configuration Files

- `config/settings.env.example`: Template with all options
- `config/settings.local.env`: Local overrides (not committed)

## CLI Interface

Rich command-line interface with multiple commands:

```bash
# Main migration command
cobol-migrate --cobol-source ./cobol --java-output ./java

# Interactive setup
cobol-migrate-setup

# Generate conversation logs  
cobol-migrate-conversation --session-id session_123

# Validate configuration
cobol-migrate validate

# System diagnostics
python scripts/doctor.py check
```

## Migration Process Comparison

### Original C# Process

1. Manual configuration setup
2. Semantic Kernel initialization
3. Sequential file processing
4. Basic logging
5. Simple reports

### New Python Process

1. **Enhanced Configuration**: Environment-based with validation
2. **Atomic Agents**: Schema-driven AI interactions
3. **Async Processing**: Parallel file handling where possible
4. **Rich Progress**: Real-time progress bars and status
5. **Comprehensive Logging**: API tracking, cost analysis, conversation logs
6. **Advanced Reports**: Detailed markdown with metrics and visualizations

## Key Improvements Over C# Version

### 1. Better Agent Management
- **Schema Validation**: All inputs/outputs are validated with Pydantic
- **Memory Management**: Built-in conversation tracking
- **Error Handling**: Graceful fallbacks and detailed error reporting

### 2. Enhanced Observability
- **API Call Tracking**: Detailed logs of all AI interactions
- **Cost Analysis**: Real-time cost estimation
- **Performance Metrics**: Duration and token usage tracking
- **Conversation Logs**: Human-readable AI interaction logs

### 3. Improved User Experience
- **Rich CLI**: Beautiful terminal interface with progress bars
- **Interactive Setup**: Wizard-based configuration
- **System Diagnostics**: Built-in health checks and validation
- **Better Error Messages**: Clear, actionable error reporting

### 4. Production Readiness
- **Async Support**: Non-blocking I/O operations
- **Proper Logging**: Structured logging with multiple levels
- **Configuration Management**: Environment-based configuration
- **Type Safety**: Full type hints and validation

## Installation and Setup

### Prerequisites

- Python 3.10+
- Poetry (recommended) or pip
- Azure OpenAI or OpenAI API access

### Quick Start

```bash
# Clone and install
git clone <repository>
cd cobol_migration_agents
pip install -e .

# Configure (manually edit settings)
cp config/settings.env.example config/settings.local.env
nano config/settings.local.env

# Run migration
cobol-migrate --cobol-source ./cobol-source --java-output ./java-output
```

### Development Setup

```bash
# Install with development dependencies
poetry install --with dev

# Run tests
pytest

# Format code
black .

# Type checking
mypy .
```

## Migration from C# to Python

If you're migrating from the C# version:

1. **Export Existing Configuration**: Note your Azure OpenAI settings
2. **Install Python Version**: Follow installation steps above
3. **Configure**: Use `cobol-migrate-setup` to configure settings
4. **Test**: Run `python scripts/doctor.py check` to validate setup
5. **Migrate**: Use same COBOL source files with new Python CLI

## Troubleshooting

### Common Issues

1. **Import Errors**: Run `poetry install` or `pip install -e .`
2. **Configuration Issues**: Use `cobol-migrate validate` to check settings
3. **API Errors**: Verify API keys and endpoints in configuration
4. **Permission Errors**: Check file system permissions

### Diagnostics

```bash
# Comprehensive system check
python scripts/doctor.py check

# View system information
python scripts/doctor.py info

# Test migration setup
python scripts/doctor.py test
```

## Future Enhancements

The Python implementation provides a foundation for additional features:

1. **Web Interface**: FastAPI-based web UI for migrations
2. **Batch Processing**: Queue-based processing for large codebases
3. **Plugin System**: Extensible agent system for custom analysis
4. **Integration APIs**: REST APIs for integration with other tools
5. **Advanced Analytics**: Machine learning for migration pattern recognition

## Contributing

The modular architecture makes it easy to contribute:

1. **New Agents**: Extend BaseAgent for specialized analysis
2. **Custom Models**: Add Pydantic models for new data types
3. **Additional Services**: Create services for specific functionality
4. **CLI Commands**: Add new commands to the CLI interface

See the project README for detailed contribution guidelines.