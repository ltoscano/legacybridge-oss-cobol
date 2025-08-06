#!/usr/bin/env python3
"""Doctor script for COBOL Migration Agents - diagnostics and validation."""

import os
import sys
import asyncio
from pathlib import Path
from typing import List, Dict, Any

import typer
from rich.console import Console
from rich.table import Table
from rich.panel import Panel

# Add the parent directory to sys.path to import our modules
sys.path.insert(0, str(Path(__file__).parent.parent))

from cobol_migration_agents.config.settings import Settings

console = Console()
app = typer.Typer(name="doctor", help="COBOL Migration Agents diagnostics and validation")


@app.command()
def check() -> None:
    """
    Run comprehensive system checks.
    
    Validates configuration, dependencies, and system readiness.
    """
    console.print(Panel.fit(
        "[bold green]🩺 COBOL Migration Agents Doctor[/bold green]\n"
        "[dim]System diagnostics and validation[/dim]",
        border_style="green"
    ))
    
    checks = [
        ("Python Version", check_python_version),
        ("Dependencies", check_dependencies),
        ("Configuration", check_configuration),
        ("File Permissions", check_file_permissions),
        ("AI Service", check_ai_service),
        ("Disk Space", check_disk_space)
    ]
    
    results = {}
    
    for check_name, check_func in checks:
        console.print(f"\n[cyan]Checking {check_name}...[/cyan]")
        try:
            result = check_func()
            results[check_name] = result
            
            if result["status"] == "pass":
                console.print(f"[green]✅ {check_name}: {result['message']}[/green]")
            elif result["status"] == "warning":
                console.print(f"[yellow]⚠️ {check_name}: {result['message']}[/yellow]")
            else:
                console.print(f"[red]❌ {check_name}: {result['message']}[/red]")
                
        except Exception as e:
            console.print(f"[red]❌ {check_name}: Error during check - {e}[/red]")
            results[check_name] = {"status": "error", "message": str(e)}
    
    # Summary
    console.print("\n" + "="*50)
    _print_summary(results)
    
    # Recommendations
    _print_recommendations(results)


def check_python_version() -> Dict[str, Any]:
    """Check Python version compatibility."""
    version = sys.version_info
    
    if version >= (3, 10):
        return {
            "status": "pass",
            "message": f"Python {version.major}.{version.minor}.{version.micro}"
        }
    elif version >= (3, 8):
        return {
            "status": "warning", 
            "message": f"Python {version.major}.{version.minor}.{version.micro} (3.10+ recommended)"
        }
    else:
        return {
            "status": "fail",
            "message": f"Python {version.major}.{version.minor}.{version.micro} (3.10+ required)"
        }


def check_dependencies() -> Dict[str, Any]:
    """Check if required dependencies are installed."""
    required_packages = [
        "atomic_agents",
        "instructor", 
        "openai",
        "pydantic",
        "rich",
        "typer",
        "aiofiles"
    ]
    
    missing = []
    installed = []
    
    for package in required_packages:
        try:
            __import__(package)
            installed.append(package)
        except ImportError:
            missing.append(package)
    
    if not missing:
        return {
            "status": "pass",
            "message": f"All {len(installed)} required packages installed"
        }
    else:
        return {
            "status": "fail",
            "message": f"Missing packages: {', '.join(missing)}"
        }


def check_configuration() -> Dict[str, Any]:
    """Check configuration validity."""
    try:
        settings = Settings.from_env()
        missing = settings.validate_required_settings()
        
        if not missing:
            return {
                "status": "pass",
                "message": "All required configuration present"
            }
        else:
            return {
                "status": "fail",
                "message": f"Missing settings: {', '.join(missing)}"
            }
    
    except Exception as e:
        return {
            "status": "fail",
            "message": f"Configuration error: {e}"
        }


def check_file_permissions() -> Dict[str, Any]:
    """Check file system permissions."""
    try:
        # Test write permissions in current directory
        test_file = Path("test_permissions.tmp")
        test_file.write_text("test")
        test_file.unlink()
        
        # Check config directory
        config_dir = Path("config")
        if not config_dir.exists():
            return {
                "status": "warning",
                "message": "Config directory doesn't exist (will be created)"
            }
        
        return {
            "status": "pass",
            "message": "File permissions OK"
        }
    
    except Exception as e:
        return {
            "status": "fail",
            "message": f"Permission error: {e}"
        }


def check_ai_service() -> Dict[str, Any]:
    """Check AI service connectivity."""
    try:
        settings = Settings.from_env()
        missing = settings.validate_required_settings()
        
        if missing:
            return {
                "status": "fail",
                "message": "Cannot test - missing configuration"
            }
        
        # Basic validation - actual connectivity test would require more setup
        if settings.ai_settings.api_key and len(settings.ai_settings.api_key) > 10:
            return {
                "status": "pass",
                "message": "AI service configuration appears valid"
            }
        else:
            return {
                "status": "fail",
                "message": "Invalid API key format"
            }
    
    except Exception as e:
        return {
            "status": "fail",
            "message": f"AI service check failed: {e}"
        }


def check_disk_space() -> Dict[str, Any]:
    """Check available disk space."""
    try:
        import shutil
        
        # Check current directory disk space
        total, used, free = shutil.disk_usage(".")
        free_gb = free / (1024**3)
        
        if free_gb > 5:  # 5GB
            return {
                "status": "pass",
                "message": f"{free_gb:.1f}GB available"
            }
        elif free_gb > 1:  # 1GB
            return {
                "status": "warning",
                "message": f"{free_gb:.1f}GB available (low space)"
            }
        else:
            return {
                "status": "fail",
                "message": f"{free_gb:.1f}GB available (insufficient space)"
            }
    
    except Exception as e:
        return {
            "status": "warning",
            "message": f"Could not check disk space: {e}"
        }


def _print_summary(results: Dict[str, Dict[str, Any]]) -> None:
    """Print check results summary."""
    table = Table(title="System Check Results")
    table.add_column("Check", style="cyan")
    table.add_column("Status", style="bold")
    table.add_column("Details")
    
    for check_name, result in results.items():
        status = result["status"]
        if status == "pass":
            status_icon = "[green]✅ PASS[/green]"
        elif status == "warning":
            status_icon = "[yellow]⚠️ WARNING[/yellow]"
        else:
            status_icon = "[red]❌ FAIL[/red]"
        
        table.add_row(check_name, status_icon, result["message"])
    
    console.print(table)


def _print_recommendations(results: Dict[str, Dict[str, Any]]) -> None:
    """Print recommendations based on check results."""
    recommendations = []
    
    # Check for specific issues and provide recommendations
    if results.get("Python Version", {}).get("status") == "warning":
        recommendations.append("Consider upgrading to Python 3.10+ for best compatibility")
    
    if results.get("Dependencies", {}).get("status") == "fail":
        recommendations.append("Run 'poetry install' or 'pip install -e .' to install dependencies")
    
    if results.get("Configuration", {}).get("status") == "fail":
        recommendations.append("Run 'cobol-migrate-setup' to configure AI service credentials")
    
    if results.get("AI Service", {}).get("status") == "fail":
        recommendations.append("Verify your AI service API key and endpoint configuration")
    
    if results.get("Disk Space", {}).get("status") in ["warning", "fail"]:
        recommendations.append("Free up disk space before running large migrations")
    
    if recommendations:
        console.print("\n[yellow]📋 Recommendations:[/yellow]")
        for i, rec in enumerate(recommendations, 1):
            console.print(f"  {i}. {rec}")
    else:
        console.print("\n[green]🎉 All checks passed! System is ready for migration.[/green]")


@app.command()
def test() -> None:
    """
    Run a quick test migration with sample data.
    
    Creates test COBOL files and runs a small migration to verify the system works.
    """
    console.print("[cyan]🧪 Running test migration...[/cyan]")
    
    # Create test directories
    test_dir = Path("test_migration")
    cobol_dir = test_dir / "cobol"
    java_dir = test_dir / "java"
    
    cobol_dir.mkdir(parents=True, exist_ok=True)
    java_dir.mkdir(parents=True, exist_ok=True)
    
    # Create sample COBOL file
    sample_cobol = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO-WORLD.
      AUTHOR. TEST.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-MESSAGE PIC X(30) VALUE 'HELLO WORLD FROM COBOL'.
      
      PROCEDURE DIVISION.
      MAIN-LOGIC.
          DISPLAY WS-MESSAGE.
          STOP RUN.
"""
    
    sample_file = cobol_dir / "HELLO-WORLD.cbl"
    sample_file.write_text(sample_cobol)
    
    console.print(f"✅ Created test COBOL file: {sample_file}")
    
    # Try to run migration (would need actual implementation)
    console.print("⚠️ Test migration functionality not yet implemented")
    console.print("Use: cobol-migrate --cobol-source test_migration/cobol --java-output test_migration/java")
    
    # Clean up
    cleanup = input("Clean up test files? (Y/n): ")
    if cleanup.lower() != 'n':
        import shutil
        shutil.rmtree(test_dir)
        console.print("🧹 Test files cleaned up")


@app.command()
def info() -> None:
    """Display system and configuration information."""
    console.print(Panel.fit(
        "[bold blue]ℹ️ System Information[/bold blue]",
        border_style="blue"
    ))
    
    # System info
    info_table = Table(title="System Information")
    info_table.add_column("Property", style="cyan")
    info_table.add_column("Value", style="green")
    
    info_table.add_row("Python Version", f"{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}")
    info_table.add_row("Platform", sys.platform)
    info_table.add_row("Working Directory", str(Path.cwd()))
    
    # Virtual environment info
    venv_info = "Yes" if sys.prefix != sys.base_prefix else "No"
    info_table.add_row("Virtual Environment", venv_info)
    
    console.print(info_table)
    
    # Configuration info
    try:
        settings = Settings.from_env()
        
        config_table = Table(title="Configuration")
        config_table.add_column("Setting", style="cyan") 
        config_table.add_column("Value", style="green")
        
        config_table.add_row("AI Service", settings.ai_settings.service_type)
        config_table.add_row("Model", settings.ai_settings.model_id)
        config_table.add_row("Max Tokens", str(settings.ai_settings.max_tokens))
        config_table.add_row("Temperature", str(settings.ai_settings.temperature))
        config_table.add_row("Logs Folder", settings.application_settings.logs_folder)
        
        console.print(config_table)
        
    except Exception as e:
        console.print(f"[red]❌ Could not load configuration: {e}[/red]")


if __name__ == "__main__":
    app()