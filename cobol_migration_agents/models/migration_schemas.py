"""Input/Output schemas for migration agents using atomic-agents framework."""

from typing import List, Optional, Dict, Any
from pydantic import Field
from atomic_agents.base.base_io_schema import BaseIOSchema

from .cobol_models import CobolFile, CobolAnalysis
from .java_models import JavaFile
from .dependency_models import DependencyMap


# Migration Orchestrator Schemas
class MigrationInputSchema(BaseIOSchema):
    """Input schema for the migration orchestrator."""
    
    cobol_source_folder: str = Field(..., description="Path to COBOL source folder")
    java_output_folder: str = Field(..., description="Path for Java output")
    migration_options: Dict[str, Any] = Field(
        default_factory=dict, 
        description="Additional migration options"
    )


class MigrationOutputSchema(BaseIOSchema):
    """Output schema for the migration orchestrator."""
    
    success: bool = Field(..., description="Whether migration completed successfully")
    total_files_processed: int = Field(..., description="Total number of files processed")
    java_files_generated: int = Field(..., description="Number of Java files generated")
    migration_report_path: str = Field(..., description="Path to the migration report")
    conversation_log_path: str = Field(..., description="Path to the conversation log")
    warnings: List[str] = Field(default_factory=list, description="Migration warnings")
    errors: List[str] = Field(default_factory=list, description="Migration errors")


# COBOL Analyzer Agent Schemas
class AnalysisInputSchema(BaseIOSchema):
    """Input schema for COBOL analysis agent."""
    
    cobol_file: CobolFile = Field(..., description="COBOL file to analyze")
    analysis_depth: str = Field(default="comprehensive", description="Analysis depth level")
    focus_areas: List[str] = Field(
        default_factory=lambda: ["structure", "complexity", "business_logic"], 
        description="Areas to focus analysis on"
    )


class AnalysisOutputSchema(BaseIOSchema):
    """Output schema for COBOL analysis agent."""
    
    analysis: CobolAnalysis = Field(..., description="Complete COBOL analysis results")
    confidence_score: float = Field(..., description="Confidence in analysis results (0-1)")
    analysis_notes: List[str] = Field(default_factory=list, description="Additional analysis notes")
    recommendations: List[str] = Field(default_factory=list, description="Migration recommendations")


# Java Converter Agent Schemas
class ConversionInputSchema(BaseIOSchema):
    """Input schema for Java conversion agent."""
    
    cobol_file: CobolFile = Field(..., description="COBOL file to convert")
    cobol_analysis: CobolAnalysis = Field(..., description="Analysis results for the COBOL file")
    target_framework: str = Field(default="quarkus", description="Target Java framework")
    conversion_style: str = Field(default="microservice", description="Conversion style preference")
    include_tests: bool = Field(default=True, description="Whether to generate unit tests")


class ConversionOutputSchema(BaseIOSchema):
    """Output schema for Java conversion agent."""
    
    java_file: JavaFile = Field(..., description="Generated Java file")
    conversion_confidence: float = Field(..., description="Confidence in conversion quality (0-1)")
    conversion_notes: List[str] = Field(default_factory=list, description="Conversion process notes")
    manual_review_items: List[str] = Field(default_factory=list, description="Items requiring manual review")
    test_recommendations: List[str] = Field(default_factory=list, description="Testing recommendations")


# Dependency Mapper Agent Schemas
class DependencyInputSchema(BaseIOSchema):
    """Input schema for dependency mapping agent."""
    
    cobol_files: List[CobolFile] = Field(..., description="List of COBOL files to analyze")
    analysis_results: List[CobolAnalysis] = Field(
        default_factory=list, 
        description="Optional analysis results for enhanced mapping"
    )
    include_visualization: bool = Field(default=True, description="Whether to generate visualization diagrams")


class DependencyOutputSchema(BaseIOSchema):
    """Output schema for dependency mapping agent."""
    
    dependency_map: DependencyMap = Field(..., description="Complete dependency mapping")
    mapping_confidence: float = Field(..., description="Confidence in mapping accuracy (0-1)")
    mapping_notes: List[str] = Field(default_factory=list, description="Dependency mapping notes")
    migration_recommendations: List[str] = Field(default_factory=list, description="Migration order recommendations")


# File Processing Schemas
class FileDiscoveryInputSchema(BaseIOSchema):
    """Input schema for file discovery."""
    
    source_directory: str = Field(..., description="Directory to scan for COBOL files")
    file_patterns: List[str] = Field(
        default_factory=lambda: ["*.cbl", "*.cpy", "*.cob"], 
        description="File patterns to match"
    )
    recursive: bool = Field(default=True, description="Whether to scan recursively")


class FileDiscoveryOutputSchema(BaseIOSchema):
    """Output schema for file discovery."""
    
    cobol_files: List[CobolFile] = Field(..., description="Discovered COBOL files")
    total_files: int = Field(..., description="Total number of files found")
    file_types: Dict[str, int] = Field(..., description="Count of each file type")
    scan_summary: str = Field(..., description="Summary of the scan results")


# Report Generation Schemas
class ReportInputSchema(BaseIOSchema):
    """Input schema for report generation."""
    
    cobol_files: List[CobolFile] = Field(..., description="Original COBOL files")
    java_files: List[JavaFile] = Field(..., description="Generated Java files")
    dependency_map: DependencyMap = Field(..., description="Dependency analysis results")
    migration_start_time: str = Field(..., description="Migration start timestamp")
    migration_duration: float = Field(..., description="Migration duration in seconds")


class ReportOutputSchema(BaseIOSchema):
    """Output schema for report generation."""
    
    report_content: str = Field(..., description="Complete migration report in markdown")
    report_path: str = Field(..., description="Path where report was saved")
    summary_metrics: Dict[str, Any] = Field(..., description="Key migration metrics")
    next_steps: List[str] = Field(default_factory=list, description="Recommended next steps")


# Conversation Log Schemas
class ConversationLogInputSchema(BaseIOSchema):
    """Input schema for conversation log generation."""
    
    session_id: Optional[str] = Field(default=None, description="Specific session ID to process")
    log_directory: str = Field(default="./logs", description="Directory containing log files")
    output_format: str = Field(default="markdown", description="Output format for conversation log")
    include_metadata: bool = Field(default=True, description="Whether to include metadata")


class ConversationLogOutputSchema(BaseIOSchema):
    """Output schema for conversation log generation."""
    
    conversation_content: str = Field(..., description="Generated conversation log content")
    log_path: str = Field(..., description="Path where conversation log was saved")
    total_messages: int = Field(..., description="Total number of messages processed")
    session_summary: str = Field(..., description="Summary of the conversation session")


# Progress Tracking Schemas
class ProgressUpdateSchema(BaseIOSchema):
    """Schema for progress updates during migration."""
    
    current_step: str = Field(..., description="Current migration step")
    step_number: int = Field(..., description="Current step number")
    total_steps: int = Field(..., description="Total number of steps")
    current_file: Optional[str] = Field(default=None, description="Currently processing file")
    files_completed: int = Field(..., description="Number of files completed")
    total_files: int = Field(..., description="Total number of files to process")
    estimated_time_remaining: Optional[float] = Field(default=None, description="Estimated time remaining in seconds")
    status_message: str = Field(..., description="Human-readable status message")