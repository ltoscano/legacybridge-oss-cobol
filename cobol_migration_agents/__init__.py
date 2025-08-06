"""
COBOL Migration Agents

AI-powered COBOL to Java Quarkus migration using atomic-agents framework.
"""

__version__ = "0.1.0"
__author__ = "Your Name"
__email__ = "your.email@example.com"

from .migration_orchestrator import MigrationOrchestrator
from .config.settings import Settings

__all__ = ["MigrationOrchestrator", "Settings"]