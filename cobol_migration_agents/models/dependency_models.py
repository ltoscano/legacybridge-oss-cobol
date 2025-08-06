"""Dependency mapping models."""

from typing import List, Dict, Optional, Set
from datetime import datetime
from pydantic import BaseModel, Field


class DependencyRelation(BaseModel):
    """Represents a dependency relationship between files."""
    
    source_file: str = Field(..., description="Source file name")
    target_file: str = Field(..., description="Target file name")
    dependency_type: str = Field(..., description="Type of dependency (COPY, CALL, etc.)")
    line_number: Optional[int] = Field(default=None, description="Line number where dependency occurs")
    context: Optional[str] = Field(default=None, description="Context of the dependency")


class CopybookUsage(BaseModel):
    """Represents copybook usage information."""
    
    copybook_name: str = Field(..., description="Copybook name")
    used_by: List[str] = Field(default_factory=list, description="Programs that use this copybook")
    usage_count: int = Field(default=0, description="Number of times used")
    first_usage_line: Optional[int] = Field(default=None, description="First line where used")
    replacing_clauses: List[str] = Field(default_factory=list, description="REPLACING clauses used")


class CircularDependency(BaseModel):
    """Represents a circular dependency."""
    
    cycle: List[str] = Field(..., description="Files involved in the circular dependency")
    cycle_length: int = Field(..., description="Length of the dependency cycle")
    severity: str = Field(default="medium", description="Severity of the circular dependency")


class DependencyMapMetrics(BaseModel):
    """Metrics about the dependency analysis."""
    
    total_files: int = Field(..., description="Total number of files analyzed")
    total_programs: int = Field(..., description="Number of COBOL programs")
    total_copybooks: int = Field(..., description="Number of copybooks")
    total_dependencies: int = Field(..., description="Total number of dependencies")
    
    # Dependency statistics
    average_dependencies_per_program: float = Field(..., description="Average dependencies per program")
    max_dependencies_for_single_program: int = Field(..., description="Maximum dependencies for a single program")
    programs_with_no_dependencies: int = Field(..., description="Programs with no dependencies")
    
    # Copybook statistics
    most_used_copybook: Optional[str] = Field(default=None, description="Most frequently used copybook")
    most_used_copybook_count: int = Field(default=0, description="Usage count of most used copybook")
    unused_copybooks: List[str] = Field(default_factory=list, description="Copybooks that are not used")
    
    # Circular dependencies
    circular_dependencies: List[CircularDependency] = Field(default_factory=list, description="Circular dependencies found")
    has_circular_dependencies: bool = Field(default=False, description="Whether circular dependencies exist")
    
    # Complexity indicators
    dependency_complexity_score: float = Field(default=0.0, description="Overall dependency complexity score (0-1)")
    migration_risk_level: str = Field(default="medium", description="Migration risk level based on dependencies")


class DependencyMap(BaseModel):
    """Complete dependency mapping for a COBOL codebase."""
    
    analysis_date: datetime = Field(default_factory=datetime.utcnow, description="When the analysis was performed")
    source_directory: str = Field(..., description="Source directory analyzed")
    
    # Dependencies
    dependencies: List[DependencyRelation] = Field(default_factory=list, description="All dependency relationships")
    reverse_dependencies: Dict[str, List[str]] = Field(default_factory=dict, description="Reverse dependency lookup")
    
    # Copybook information
    copybook_usage: List[CopybookUsage] = Field(default_factory=list, description="Copybook usage information")
    copybook_definitions: Dict[str, str] = Field(default_factory=dict, description="Copybook definitions and paths")
    
    # Program information
    program_dependencies: Dict[str, List[str]] = Field(default_factory=dict, description="Dependencies for each program")
    program_calls: Dict[str, List[str]] = Field(default_factory=dict, description="Program call relationships")
    
    # Analysis results
    metrics: DependencyMapMetrics = Field(..., description="Dependency analysis metrics")
    
    # Visualization
    mermaid_diagram: Optional[str] = Field(default=None, description="Mermaid diagram representation")
    graphviz_dot: Optional[str] = Field(default=None, description="GraphViz DOT representation")
    
    # Migration planning
    migration_order: List[str] = Field(default_factory=list, description="Recommended migration order")
    migration_groups: List[List[str]] = Field(default_factory=list, description="Files that can be migrated together")
    high_risk_dependencies: List[DependencyRelation] = Field(default_factory=list, description="High-risk dependencies")
    
    # Metadata
    analysis_metadata: Dict[str, str] = Field(default_factory=dict, description="Additional analysis metadata")
    
    def get_dependencies_for_file(self, file_name: str) -> List[str]:
        """Get all dependencies for a specific file."""
        return self.program_dependencies.get(file_name, [])
    
    def get_dependents_of_file(self, file_name: str) -> List[str]:
        """Get all files that depend on a specific file."""
        return self.reverse_dependencies.get(file_name, [])
    
    def is_copybook(self, file_name: str) -> bool:
        """Check if a file is a copybook."""
        return file_name.lower().endswith('.cpy') or file_name in self.copybook_definitions
    
    def get_migration_complexity(self, file_name: str) -> str:
        """Get migration complexity for a specific file."""
        dependencies = self.get_dependencies_for_file(file_name)
        dependents = self.get_dependents_of_file(file_name)
        
        total_connections = len(dependencies) + len(dependents)
        
        if total_connections == 0:
            return "low"
        elif total_connections <= 3:
            return "medium"
        else:
            return "high"
    
    class Config:
        """Pydantic configuration."""
        json_encoders = {
            datetime: lambda v: v.isoformat() if v else None
        }