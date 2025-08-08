# 🐍 LegacyBridge OSS COBOL Migration Agents

**AI-powered COBOL to Java (Quarkus) migration framework built on Python using [`Atomic Agents`](https://github.com/BrainBlend-AI/atomic-agents) and [`Instructor`](https://github.com/567-labs/instructor) for seamless orchestration, structured validation, and automatic cost tracking.**

> **What is Quarkus?** [Quarkus](https://quarkus.io/) is a fast, cloud-native Java framework for microservices, optimized for Kubernetes and serverless. It offers quick startup, low memory use, and native compilation. This migration produces ready-to-deploy Quarkus code with annotations, dependency injection, and RESTful APIs.

**Updates** 

- 08/07/2025: Tested with **LiteLLM** — best results with OpenAI o4 via Azure, Codex via Azure, Gemini via Vertex, and Claude via Vertex.
- 08/07/2025: Tested with OpenRouter — performance on par with LiteLLM, plus support for Grok 3/4 and many more models.
- 08/07/2025: **GPT-OSS** models (both locally and via OpenRouter) proved too limited for complex input/output schemas, so I’m exploring alternative solutions.
- 08/08/2025: Tested with **GPT-5** — outstanding results!

---

## 🧭 About this Project

This repository is a **modern Python rewrite** of the original Microsoft project [Legacy Modernization Agents (C#)](https://github.com/Azure-Samples/Legacy-Modernization-Agents), which provided an AI-assisted approach to COBOL-to-Java migration using [Semantic Kernel](https://github.com/microsoft/semantic-kernel).

The Python implementation leverages the Atomic Agents framework to provide a more modular, schema-driven approach to AI agent management while maintaining all the functionality of the original C# system.

**What's new in this version:**
- Built on [`Atomic Agents`](https://github.com/BrainBlend-AI/atomic-agents) for standardized AI orchestration
- Uses [`Instructor`](https://github.com/567-labs/instructor) for structured LLM interactions with automatic validation
- **Automatic token and cost tracking** through instructor hooks
- **Docker-first** deployment with simplified setup
- **Enhanced observability** with comprehensive logging and conversation tracking

### _About the Original Microsoft Project_

_The original [Legacy Modernization Agents](https://github.com/Azure-Samples/Legacy-Modernization-Agents) represents a pioneering Microsoft initiative that emerged from a strategic collaboration with Denmark's [Bankdata](https://www.bankdata.dk/). This groundbreaking project demonstrates how AI agents can revolutionize legacy COBOL modernization at enterprise scale._

#### _Technical Foundation_

_Built on Microsoft's **Semantic Kernel framework** with .NET 8.0, the system leverages **Process Functions** to orchestrate three specialized AI agents: CobolAnalyzer, JavaConverter, and DependencyMapper. The architecture is specifically optimized for **GPT-4.1 models** running at enterprise capacity (1M tokens/minute) and integrates seamlessly with **Azure OpenAI services**. Development is streamlined through Visual Studio Code Dev Containers, ensuring consistent environments across teams._

#### _Enterprise-Grade Design_

_The project's **multi-agent architecture** embodies a clear separation of concerns, with each agent specializing in distinct phases of the migration pipeline. Configuration management follows enterprise patterns with a **dual-file system** (template + local credentials), while the included **`doctor.sh` CLI tool** provides comprehensive setup, validation, and migration management capabilities. This design philosophy prioritizes **observability, reliability, and scalability** for large-scale modernization initiatives._

#### _Industry Impact & Proven Results_

_As a **joint research initiative** between Microsoft's Global Black Belt team and Bankdata, this project has garnered significant industry attention through featured blog posts and conference presentations. The **open-source release** was strategically designed to engage the broader COBOL community, gathering real-world code samples to further refine the AI models._ 

_With **enterprise-validated results**, the system has demonstrated remarkable efficiency: processing **102 COBOL files into 99 Java files** in just **~1.2 hours** at **sub-dollar costs**, achieving an impressive **97% successful conversion rate**. This proves the practical viability of AI-assisted legacy modernization at enterprise scale._

---

## 🧩 Why Rewrite It?

While the original C# project offered a strong foundation, this Python version introduces key innovations that dramatically improve **developer experience**, **observability**, **AI reliability**, and **deployment simplicity**:

### Enhanced LLM Integration & Reliability

This Python rewrite leverages **[Instructor](https://github.com/567-labs/instructor)** library for superior LLM interactions:

- **Optimized Prompts**: All agent prompts have been refined for better accuracy and consistency
- **Structured Communication**: Instructor enforces strict input/output schemas via Pydantic, reducing AI errors
- **Automatic Retries**: Built-in retry logic with validation ensures robust responses
- **Multi-Provider Support**: Easy switching between OpenAI, Azure OpenAI, Anthropic, and other providers
- **Performance**: Structured responses eliminate manual parsing, improving speed and reliability

### Atomic Agents Framework Advantages

Built on **[Atomic Agents](https://github.com/BrainBlend-AI/atomic-agents)**, which offers significant improvements over frameworks like LangChain and CrewAI through its **IPO model** (Input-Process-Output) with Pydantic validation, **atomic components** with single responsibilities, and **transparent operations** without hidden abstractions.

This approach delivers a faster, more resilient, and scalable modernization pipeline with full developer control.

### Technical Improvements

1. **Schema-Driven Architecture**: Pydantic models ensure strong typing and validation across all data flows
2. **Asynchronous Processing**: Full async/await support for improved performance and scalability
3. **Enhanced CLI Experience**: Rich interface with progress bars, styling, and interactive feedback
4. **Service-Oriented Design**: Clear separation of concerns with modular, testable components
5. **Automated Observability**: Built-in hooks capture token usage, costs, and conversation flows automatically

### Why Use This Tool?

| Feature | Value |
|--------|-------|
| **Speed** | Migration in minutes, not months |
| **Cost** | Orders of magnitude lower than traditional manual rewrites |
| **Auditability** | Full AI trace and confidence logs |
| **Accuracy** | Enterprise-grade output with >95% correctness potential, supported by AI-driven auto-review and suggestions to speed-up human validation |
| **Scalability** | Agent-based architecture supports horizontal scale |
| **Maintainability** | Modern, clean Java output with Javadoc |
| **Compliance** | Traceability from COBOL to Java |

---

## Quick Startup

### 1. Environment Preparation
- Copy the example environment file and edit local settings:
    ```sh
    cp ./config/settings.env.example ./config/settings.local.env
    nano ./config/settings.local.env
    ```

### 2. Initial Setup
- Run the setup script to initialize Docker containers and validate the environment:
    ```sh
    ./scripts/docker-setup.sh setup
    ```
- Follow on-screen instructions to verify the setup and perform a test migration.

### 3. Default Data Directories
- COBOL source files: `./data/cobol-source`
- Java output files: `./data/java-output`
- Log files: `./data/logs`

### 4. Additional Commands
- For further options, consult the help menu:
    ```sh
    ./scripts/docker-setup.sh --help
    ```

### 5. Wizard-Based Setup
- To configure the environment interactively, use:
    ```sh
    ./run.sh cobol-migrate-setup
    ```
- For a list of available commands:
    ```sh
    ./run.sh --help
    ```

### 6. Direct Docker Compose Usage
- You may use Docker Compose commands directly as an alternative to `./run.sh`.
    Example:
    ```sh
    ./run.sh cobol-migrate-setup
    # is equivalent to:
    docker-compose run --rm cobol-migration cobol-migrate-setup
    ```

More information:
- 📘 [DOCKER_QUICKSTART.md](DOCKER_QUICKSTART.md)
- 📘 [DOCKER_GUIDE.md](DOCKER_GUIDE.md)
- 📋 [PARAMETERS.md](PARAMETERS.md) - Complete configuration reference

### Docker Configuration Advantages

The Docker-first setup ensures a consistent, isolated environment on any platform, with secure non-root containers and built-in scalability for enterprise use. Automated scripts streamline all operations, while development mode includes full debugging tools for rapid troubleshooting.

### Production-Ready Deployment

This containerized approach is ready for Docker Swarm clusters, Kubernetes with manifests, CI/CD integration (GitHub Actions, GitLab CI), and major cloud platforms like AWS ECS, Azure Container Instances, and GCP Cloud Run.

---

## 🏗️ System Architecture & Key Features

The framework is based on a modular **multi-agent architecture**, each agent specializing in one phase of the migration process. For a detailed technical diagram of the Python implementation, see the [Architecture Diagram in Appendix](#-python-implementation-architecture).

### Agent Responsibilities

| Agent | Role |
|-------|------|
| `CobolAnalyzerAgent` | Parses COBOL structure, detects business logic, calculates complexity |
| `JavaConverterAgent` | Converts COBOL to modern Java using Quarkus best practices |
| `DependencyMapperAgent` | Maps relationships between files, generates Mermaid dependency diagrams |

---

### End-to-End Migration Process

```mermaid
graph TB
    A[File Discovery] --> B[Dependency Analysis]
    B --> C[COBOL Analysis]
    C --> D[Java Conversion]
    D --> E[File Generation]
    E --> F[Report Generation]
    G[Hook Tracking] -.-> C
    G -.-> D
    G -.-> B
```

### AI Token & Cost Tracking

With **automatic Instructor hooks**, every AI call is traced and measured.

### Metrics Collected

- Tokens: prompt, completion, total
- Cost: calculated based on model used
- Latency: per-agent performance
- Hook efficiency: % of calls automatically captured

### Output Artifacts & Value

#### Migration Reports (`migration_report_*.md`)
- Conversion rate, token use, complexity score
- AI performance per file and per agent
- Code expansion ratio (COBOL → Java)

#### AI Conversation Logs (`conversation_log_*.md`)
- Timestamped agent reasoning
- Latency and confidence metrics per file
- Full visibility for debugging and auditing

This file is automatically generated after each migration. You can also regenerate it at any time using the `conversation` command with `./run.sh`.

#### Enterprise-Ready Java Code
- Fully annotated Quarkus-compatible services
- Type-safe, idiomatic Java code
- Clean microservice structure, ready for REST APIs

---

## 🧪 Development Setup (Optional, without Docker)

> Only recommended for advanced users.

### Requirements

- Python 3.12+
- pip

### Setup

```bash
pip install -e .

# Copy template to create your local configuration
cp config/settings.env.example config/settings.local.env
nano config/settings.local.env

python -m cobol_migration_agents.cli main   --cobol-source ./data/cobol-source   --java-output ./data/java-output
```

> 📋 **Configuration Help**: For detailed explanations of all configuration parameters, see [PARAMETERS.md](PARAMETERS.md)

---

## 🤝 Contributing

The modular design makes it easy to add new agents, models, services, or CLI commands. Contributions are welcome—whether it's new agent types, improved prompts, better tests, or feedback from users.

---

## 📄 License

MIT License — same as the original C# version.

### Disclaimer

This repository is an **independent creation by [Lorenzo Toscano](https://it.linkedin.com/in/lorenzotoscano)**, developed entirely separate from any professional activities or organizational affiliations. This work serves as a **technical demonstration** of how agentic AI configurations can effectively accelerate reverse engineering and code migration processes.

This project is intended for educational and research purposes, showcasing best practices in AI-assisted software migration. If you plan to use it in production environments, it is strongly recommended to seek guidance from qualified and experienced professionals.

---

## 📖 Appendix

### Python Implementation Architecture

This Python version implements a modern, modular architecture using Atomic Agents and Pydantic models:

```mermaid
graph TB
    subgraph ORCHESTRATOR ["🎯 Migration Orchestrator"]
        COORDINATOR["📋 MigrationOrchestrator<br/>• 6-Step Process<br/>• File Discovery<br/>• Workflow Management"]
    end
    
    subgraph AI_AGENTS ["🤖 Atomic Agents"]
        COBOL_AGENT["🔍 CobolAnalyzerAgent<br/>• Structure Analysis<br/>• Variable Mapping<br/>• Logic Flow Detection<br/>• Copybook References"]
        
        JAVA_AGENT["☕ JavaConverterAgent<br/>• COBOL→Java Translation<br/>• Quarkus Integration<br/>• Best Practices<br/>• Error Handling"]
        
        DEPENDENCY_AGENT["🗺️ DependencyMapperAgent<br/>• Relationship Analysis<br/>• Mermaid Diagrams<br/>• Usage Patterns<br/>• Risk Assessment"]
    end
    
    subgraph DATA_MODELS ["📊 Pydantic Models"]
        COBOL_MODELS["📄 COBOL Models<br/>• CobolFile<br/>• CobolAnalysis<br/>• Complexity Metrics"]
        
        JAVA_MODELS["☕ Java Models<br/>• JavaFile<br/>• Quarkus Metadata<br/>• Class Structures"]
        
        SCHEMA_MODELS["🔗 Schema Models<br/>• Input/Output Schemas<br/>• Migration Schemas<br/>• DependencyMap"]
    end
    
    subgraph SERVICES ["🛠️ Core Services"]
        FILE_MANAGER["📁 FileManager<br/>• Async File Operations<br/>• Directory Organization<br/>• Backup & Validation"]
        
        LOGGING_SERVICE["📊 LoggingService<br/>• API Call Tracking<br/>• Conversation Logging<br/>• Cost Analysis"]
        
        REPORT_SERVICE["📋 ReportGenerator<br/>• Markdown Reports<br/>• Migration Statistics<br/>• Recommendations"]
    end
    
    subgraph OUTPUT ["📤 Generated Artifacts"]
        JAVA_OUTPUT["☕ Java Files<br/>• Quarkus Services<br/>• Package Structure<br/>• Annotations"]
        
        REPORTS_OUTPUT["📋 Reports<br/>• Migration Report<br/>• Conversation Logs<br/>• API Statistics"]
        
        DIAGRAMS_OUTPUT["🗺️ Diagrams<br/>• Dependency Maps<br/>• Mermaid Charts<br/>• Risk Analysis"]
    end
    
    %% Main Flow
    COORDINATOR --> AI_AGENTS
    AI_AGENTS --> DATA_MODELS
    DATA_MODELS --> SERVICES
    SERVICES --> OUTPUT
    
    %% Detailed Connections
    COORDINATOR -.-> FILE_MANAGER
    COORDINATOR -.-> LOGGING_SERVICE
    COORDINATOR -.-> REPORT_SERVICE
    
    COBOL_AGENT --> COBOL_MODELS
    JAVA_AGENT --> JAVA_MODELS
    DEPENDENCY_AGENT --> SCHEMA_MODELS
    
    FILE_MANAGER --> JAVA_OUTPUT
    LOGGING_SERVICE --> REPORTS_OUTPUT
    REPORT_SERVICE --> DIAGRAMS_OUTPUT
    
    %% Styling
    classDef orchestratorStyle fill:#e3f2fd,stroke:#1976d2,stroke-width:3px,color:#0d47a1
    classDef agentStyle fill:#fff3e0,stroke:#f57c00,stroke-width:3px,color:#e65100
    classDef modelStyle fill:#f1f8e9,stroke:#689f38,stroke-width:3px,color:#1b5e20
    classDef serviceStyle fill:#f3e5f5,stroke:#7b1fa2,stroke-width:3px,color:#4a148c
    classDef outputStyle fill:#e8f5e8,stroke:#2e7d32,stroke-width:3px,color:#1b5e20
    
    class COORDINATOR orchestratorStyle
    class COBOL_AGENT,JAVA_AGENT,DEPENDENCY_AGENT agentStyle
    class COBOL_MODELS,JAVA_MODELS,SCHEMA_MODELS modelStyle
    class FILE_MANAGER,LOGGING_SERVICE,REPORT_SERVICE serviceStyle
    class JAVA_OUTPUT,REPORTS_OUTPUT,DIAGRAMS_OUTPUT outputStyle
```

---

## 🚧 Current Limitations & Roadmap

### What the Migration Produces

**Ready-to-Compile Quarkus Code:**
- Complete Java classes with proper Quarkus annotations (`@ApplicationScoped`, `@Entity`, `@Path`)
- Dependency injection setup with CDI patterns
- RESTful API structure with service/repository layers
- Package organization and imports
- Business logic preservation with type-safe conversions

**⚠️ Manual Configuration Still Required:**
- **Build Configuration**: Custom `pom.xml`/`build.gradle` for specific dependencies
- **Application Properties**: Runtime configuration (`application.properties`)
- **Database Schema**: DDL scripts for JPA entities if database integration is needed
- **Integration Testing**: End-to-end test suites for complex business scenarios
- **Deployment Manifests**: Kubernetes/Docker configurations for production

### Current Limitations

**AI Model Limitations:**
- **Complex Business Logic**: Very intricate COBOL programs may require manual review
- **Legacy Extensions**: Non-standard COBOL extensions might not be fully supported
- **Performance Optimization**: Generated code may need tuning for high-performance scenarios

**Scope Limitations:**
- **JCL Integration**: Job Control Language translation not included
- **Database Specifics**: COBOL-DB2 specific optimizations need manual attention
- **Screen Handling**: CICS/3270 screen logic requires additional conversion steps

**Infrastructure Gaps:**
- **CI/CD Pipelines**: Build and deployment automation not generated
- **Monitoring Setup**: Observability configuration for production monitoring
- **Security Configuration**: Authentication/authorization setup for enterprise deployment

### Production Readiness Assessment

When assessing production readiness, consider that the conversion of core business logic is already production-grade, with demonstrated accuracy above 95%. The Quarkus microservices architecture, Docker containerization, advanced logging, and optimized token usage are all fully implemented. However, it is essential to carefully review sections involving complex mathematical calculations, performance-critical code paths, integrations with external systems, and security-sensitive operations. Manual handling is still required for environment-specific configurations, database connection setup, authentication and authorization implementation, as well as load testing and performance tuning activities.

For optimal results, carefully follow the instructions provided below. Please note that even steps not directly managed by this tool can be efficiently automated using solutions such as GitHub Copilot and similar technologies.

### Getting Production-Ready

Copy your COBOL source files into the `data/cobol-source` directory within your cloned repository.  
By default, the migration process uses the following folders: `data/cobol-source` for input, `data/java-output` for generated Java code, and `data/logs` for migration logs.

**1. Code Review Process:**
```bash
# Generate migration
./scripts/docker-setup.sh migrate

# Review conversation logs for AI confidence scores
cat data/logs/conversation_log_*.md

# Test compilation
cd data/java-output && mvn compile
```

**2. Integration Checklist:**
- [ ] Configure `application.properties` for your environment
- [ ] Set up database connections and JPA configuration
- [ ] Implement authentication/authorization if needed
- [ ] Add integration tests for critical business flows
- [ ] Configure monitoring and alerting
- [ ] Set up CI/CD pipelines

**3. Deployment Strategy:**
- **Development**: Use generated Docker configurations
- **Staging**: Add environment-specific properties
- **Production**: Implement full observability stack

This framework provides a strong foundation for COBOL modernization while being transparent about what additional work may be needed for full production deployment.

### Roadmap & Ideas for Future Enhancements

Below are some potential enhancements. Note that advanced features are typically integrated into commercial AI solutions for migration and reverse engineering by specialized vendors.

**Short Term:**
- **Unit Test Generation**: Automated test creation for converted Java code
- **Build Configuration**: Auto-generate `pom.xml` with correct dependencies
- **Enhanced COBOL Support**: Better handling of complex nested structures
- **Performance Optimization**: AI-driven performance tuning suggestions

**Medium Term:**
- **JCL Converter Agent**: Job Control Language to Spring Batch/Quarkus Scheduler
- **Database Migration Tools**: Schema conversion and data migration scripts
- **CICS Integration**: Web service replacement for terminal-based applications
- **Advanced Dependency Analysis**: Cross-system dependency mapping

**Long Term:**
- **Enterprise Integration**: SAP, Oracle, IBM mainframe connectors
- **Visual Migration Designer**: GUI for migration planning and customization
- **Real-time Migration**: Incremental, zero-downtime migration strategies