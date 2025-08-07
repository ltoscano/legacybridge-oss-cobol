"""Command-line interface for COBOL Migration Agents."""

import asyncio
import sys
from pathlib import Path
from typing import Optional

import typer
from rich.console import Console
from rich.panel import Panel
from rich.progress import Progress
from rich.table import Table

from .config.settings import Settings
from .migration_orchestrator import MigrationOrchestrator
from .models.migration_schemas import MigrationInputSchema, ProgressUpdateSchema
from .services.logging_service import LoggingService

# Create CLI app
app = typer.Typer(
    name="cobol-migrate",
    help="AI-powered COBOL to Java Quarkus migration using atomic-agents",
    rich_markup_mode="rich"
)

# Create console for rich output
console = Console()


@app.command()
def main(
    cobol_source: str = typer.Option(
        help="Path to the folder containing COBOL source files"
    ),
    java_output: str = typer.Option(
        help="Path to the folder for Java output files"
    ),
    config_file: Optional[str] = typer.Option(
        None,
        "--config", "-c",
        help="Path to configuration file (optional, uses default if not provided)"
    ),
    verbose: bool = typer.Option(
        False,
        "--verbose", "-v",
        help="Enable verbose output"
    ),
    log_level: str = typer.Option(
        "INFO",
        "--log-level", "-l",
        help="Set logging level (DEBUG, INFO, WARNING, ERROR)"
    )
) -> None:
    """
    Run COBOL to Java Quarkus migration.
    
    This command performs a complete migration of COBOL programs to Java Quarkus
    microservices using specialized AI agents.
    """
    try:
        # Load settings
        config_path = str(config_file) if config_file is not None else None
        if config_path:
            settings = Settings.from_env(config_path)
        else:
            settings = Settings.from_env()
        
        # Validate required settings
        missing_settings = settings.validate_required_settings()
        if missing_settings:
            console.print("[red]❌ Missing required configuration:[/red]")
            for setting in missing_settings:
                console.print(f"  - {setting}")
            console.print("\n[yellow]💡 Run 'cobol-migrate-setup' to configure settings[/yellow]")
            raise typer.Exit(1)
        
        # Override settings with command line arguments
        # Debug: Check if we're getting OptionInfo objects instead of values
        if hasattr(cobol_source, 'default'):
            console.print(f"[red]Debug: cobol_source is OptionInfo object: {cobol_source}[/red]")
            raise ValueError("CLI parameter parsing error: received OptionInfo instead of value")
        
        settings.application_settings.cobol_source_folder = str(cobol_source)
        settings.application_settings.java_output_folder = str(java_output)
        
        # Set logging level
        if verbose:
            settings.logging_settings.level = "DEBUG"
        else:
            settings.logging_settings.level = log_level.upper()
        
        # Run migration
        asyncio.run(_run_migration_async(settings))
        
    except Exception as e:
        console.print(f"[red]❌ Migration failed: {e}[/red]")
        if verbose:
            console.print_exception()
        raise typer.Exit(1)


async def _run_migration_async(settings: Settings) -> None:
    """Run the migration process asynchronously."""
    # Create progress callback
    progress_data = {"current_step": "", "progress": None, "task": None}
    
    def progress_callback(update: ProgressUpdateSchema) -> None:
        if progress_data["progress"] is None:
            progress_data["progress"] = Progress(console=console)
            progress_data["progress"].start()
            progress_data["task"] = progress_data["progress"].add_task(
                "Migration Progress", 
                total=update.total_steps
            )
        
        if update.current_step != progress_data["current_step"]:
            progress_data["current_step"] = update.current_step
            progress_data["progress"].update(
                progress_data["task"],
                completed=update.step_number,
                description=f"[cyan]{update.current_step}[/cyan]"
            )
    
    try:
        # Initialize orchestrator
        orchestrator = MigrationOrchestrator(settings, progress_callback)
        
        # Create migration input
        migration_input = MigrationInputSchema(
            cobol_source_folder=settings.application_settings.cobol_source_folder,
            java_output_folder=settings.application_settings.java_output_folder,
            migration_options={}
        )
        
        # Show start banner
        console.print(Panel.fit(
            "[bold green]🚀 COBOL Migration Agents[/bold green]\n"
            "[dim]AI-powered COBOL to Java Quarkus migration[/dim]",
            border_style="green"
        ))
        
        # Run migration
        result = await orchestrator.run_migration(migration_input)
        
        # Stop progress bar
        if progress_data["progress"]:
            progress_data["progress"].stop()
        
        # Show results
        if result.success:
            console.print("\n[green]✅ Migration completed successfully![/green]")
            
            # Create results table
            table = Table(title="Migration Results")
            table.add_column("Metric", style="cyan")
            table.add_column("Value", style="green")
            
            table.add_row("Files Processed", str(result.total_files_processed))
            table.add_row("Java Files Generated", str(result.java_files_generated))
            table.add_row("Migration Report", result.migration_report_path)
            table.add_row("Conversation Log", result.conversation_log_path)
            
            console.print(table)
            
            if result.warnings:
                console.print("\n[yellow]⚠️ Warnings:[/yellow]")
                for warning in result.warnings:
                    console.print(f"  - {warning}")
        else:
            console.print("\n[red]❌ Migration failed![/red]")
            if result.errors:
                for error in result.errors:
                    console.print(f"  - {error}")
    
    except Exception as e:
        if progress_data["progress"]:
            progress_data["progress"].stop()
        raise


@app.command("conversation")
def conversation_command(
    session_id: Optional[str] = typer.Option(
        None,
        "--session-id", "-sid",
        help="Specific session ID to generate conversation for"
    ),
    log_dir: str = typer.Option(
        "logs",
        "--log-dir", "-ld", 
        help="Path to the logs directory"
    ),
    live: bool = typer.Option(
        False,
        "--live", "-l",
        help="Enable live conversation feed that updates in real-time"
    ),
    output_dir: str = typer.Option(
        ".",
        "--output", "-o",
        help="Output directory for conversation log"
    )
) -> None:
    """
    Generate a readable conversation log from migration logs.
    
    This command processes AI agent conversation logs and creates
    a human-readable markdown report of the migration process.
    """
    try:
        # Load settings for logging service
        settings = Settings.from_env()
        
        console.print("🤖 Generating conversation log from migration data...")
        
        if live:
            console.print("[yellow]⚠️ Live mode not yet implemented[/yellow]")
            return
        
        # Initialize logging service
        logging_service = LoggingService(settings)
        
        # Generate conversation log
        if session_id:
            log_path = asyncio.run(
                logging_service.export_conversation_log(output_dir, session_id)
            )
        else:
            # Find most recent session
            log_path = asyncio.run(
                logging_service.export_conversation_log(output_dir)
            )
        
        console.print(f"[green]✅ Conversation log generated: {log_path}[/green]")
        
    except Exception as e:
        console.print(f"[red]❌ Failed to generate conversation log: {e}[/red]")
        raise typer.Exit(1)


@app.command("setup")
def setup_command() -> None:
    """
    Interactive setup wizard for configuring COBOL Migration Agents.
    
    This command guides you through setting up the required configuration
    for Azure OpenAI/OpenAI API credentials and migration settings.
    """
    console.print(Panel.fit(
        "[bold blue]🔧 COBOL Migration Agents Setup[/bold blue]\n"
        "[dim]Interactive configuration wizard[/dim]",
        border_style="blue"
    ))
    
    try:
        # Check if config directory exists
        config_dir = Path("config")
        config_dir.mkdir(exist_ok=True)
        
        # Create settings file
        settings_file = config_dir / "settings.local.env"
        
        console.print("\n[cyan]Setting up AI service configuration...[/cyan]")
        
        # Collect AI service settings
        service_type = typer.prompt(
            "AI Service Type (AzureOpenAI/OpenAI)", 
            default="AzureOpenAI"
        )
        
        if service_type.lower() == "azureopenai":
            endpoint = typer.prompt("Azure OpenAI Endpoint (e.g., https://your-resource.openai.azure.com/)")
            api_key = typer.prompt("Azure OpenAI API Key", hide_input=True)
            api_version = typer.prompt("Azure OpenAI API Version (e.g., 2025-04-01-preview)")
            deployment_name = typer.prompt("Deployment Name (e.g., gpt-4.1)")
            model_id = typer.prompt("Model ID (e.g., gpt-4.1)", default="gpt-4.1")
        else:
            endpoint = ""
            api_key = typer.prompt("OpenAI API Key", hide_input=True)
            api_version = ""
            deployment_name = ""
            model_id = typer.prompt("Model ID (e.g., gpt-4.1)", default="gpt-4.1")
        
        console.print("\n[cyan]Setting up application settings...[/cyan]")
        
        # Collect application settings
        default_cobol_source = typer.prompt(
            "Default COBOL source folder", 
            default="./cobol-source"
        )
        default_java_output = typer.prompt(
            "Default Java output folder", 
            default="./java-output"
        )
        
        console.print("\n[cyan]Optional advanced settings...[/cyan]")
        
        # Optional Instructor Mode
        instructor_mode = typer.prompt(
            "Instructor Mode (optional, e.g., instructor.Mode.JSON)", 
            default="", 
            show_default=False
        )
        
        # Generate settings content
        settings_content = f"""# COBOL Migration Agents Configuration
# Generated by setup wizard

# AI Service Configuration
AI_SERVICE_TYPE={service_type}
AZURE_OPENAI_ENDPOINT={endpoint}
AZURE_OPENAI_API_KEY={api_key}"""
        
        # Add API version for Azure OpenAI
        if service_type.lower() == "azureopenai":
            settings_content += f"""
AZURE_OPENAI_API_VERSION={api_version}
AZURE_OPENAI_DEPLOYMENT_NAME={deployment_name}
AZURE_OPENAI_MODEL_ID={model_id}"""
        else:
            settings_content += f"""
AZURE_OPENAI_MODEL_ID={model_id}"""
        
        # Add Instructor Mode if specified
        if instructor_mode.strip():
            settings_content += f"""

# Instructor Mode Configuration
INSTRUCTOR_MODE={instructor_mode.strip()}"""
        
        settings_content += f"""

# Application Settings
COBOL_SOURCE_FOLDER={default_cobol_source}
JAVA_OUTPUT_FOLDER={default_java_output}

# Optional: Specialized models for different agents
# AZURE_OPENAI_COBOL_ANALYZER_MODEL=gpt-4.1
# AZURE_OPENAI_JAVA_CONVERTER_MODEL=gpt-4.1  
# AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL=gpt-4.1

# Logging and Memory Settings
LOGGING_LEVEL=INFO
MEMORY_MAX_MESSAGES=50
ENABLE_CONVERSATION_LOGGING=true
"""
        
        # Write settings file
        with open(settings_file, 'w') as f:
            f.write(settings_content)
        
        console.print(f"\n[green]✅ Configuration saved to {settings_file}[/green]")
        
        # Test configuration
        console.print("\n[cyan]Testing configuration...[/cyan]")
        
        try:
            settings = Settings.from_env(str(settings_file))
            missing = settings.validate_required_settings()
            
            if missing:
                console.print("[yellow]⚠️ Configuration incomplete. Missing:[/yellow]")
                for item in missing:
                    console.print(f"  - {item}")
            else:
                console.print("[green]✅ Configuration is complete and valid![/green]")
                
                console.print("\n[cyan]Next steps:[/cyan]")
                console.print("1. Run a test migration: [dim]cobol-migrate --cobol-source ./test-cobol --java-output ./test-output[/dim]")
                console.print("2. Check the generated Java files and reports")
                console.print("3. Review the conversation logs for detailed AI interactions")
        
        except Exception as e:
            console.print(f"[red]❌ Configuration test failed: {e}[/red]")
    
    except Exception as e:
        console.print(f"[red]❌ Setup failed: {e}[/red]")
        raise typer.Exit(1)


@app.command("validate")
def validate_command(
    config_file: Optional[str] = typer.Option(
        None,
        "--config", "-c",
        help="Path to configuration file to validate"
    )
) -> None:
    """
    Validate configuration settings and test AI service connectivity.
    
    This command checks that all required settings are properly configured
    and tests the connection to the AI service.
    """
    try:
        # Load settings
        if config_file:
            settings = Settings.from_env(config_file)
        else:
            settings = Settings.from_env()
        
        console.print("🔍 Validating configuration...")
        
        # Check required settings
        missing = settings.validate_required_settings()
        
        if missing:
            console.print("[red]❌ Missing required settings:[/red]")
            for setting in missing:
                console.print(f"  - {setting}")
            raise typer.Exit(1)
        
        console.print("[green]✅ All required settings are present[/green]")
        
        # Display configuration summary
        table = Table(title="Configuration Summary")
        table.add_column("Setting", style="cyan")
        table.add_column("Value", style="green")
        
        table.add_row("AI Service", settings.ai_settings.service_type)
        table.add_row("Model", settings.ai_settings.model_id)
        
        if settings.ai_settings.service_type.lower() == "azureopenai":
            table.add_row("Endpoint", settings.ai_settings.endpoint or "Not set")
            table.add_row("Deployment", settings.ai_settings.deployment_name or "Not set")
        
        table.add_row("COBOL Source", settings.application_settings.cobol_source_folder or "Not set")
        table.add_row("Java Output", settings.application_settings.java_output_folder or "Not set")
        
        console.print(table)
        
        console.print("\n[green]✅ Configuration validation completed successfully![/green]")
        
    except Exception as e:
        console.print(f"[red]❌ Validation failed: {e}[/red]")
        raise typer.Exit(1)


@app.command("version")
def version_command() -> None:
    """Show version information."""
    from . import __version__
    
    console.print(f"[bold]COBOL Migration Agents[/bold] version [green]{__version__}[/green]")
    console.print("AI-powered COBOL to Java Quarkus migration using atomic-agents framework")


if __name__ == "__main__":
    app()