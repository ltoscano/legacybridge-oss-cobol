"""Data models for COBOL Migration Agents."""

from .cobol_models import CobolFile, CobolAnalysis
from .java_models import JavaFile
from .dependency_models import DependencyMap, DependencyMapMetrics
from .migration_schemas import (
    MigrationInputSchema,
    MigrationOutputSchema,
    AnalysisInputSchema,
    AnalysisOutputSchema,
    ConversionInputSchema,
    ConversionOutputSchema,
    DependencyInputSchema,
    DependencyOutputSchema
)

__all__ = [
    "CobolFile",
    "CobolAnalysis", 
    "JavaFile",
    "DependencyMap",
    "DependencyMapMetrics",
    "MigrationInputSchema",
    "MigrationOutputSchema",
    "AnalysisInputSchema",
    "AnalysisOutputSchema",
    "ConversionInputSchema",
    "ConversionOutputSchema",
    "DependencyInputSchema",
    "DependencyOutputSchema"
]