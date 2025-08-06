"""Java-related data models."""

from typing import List, Optional, Dict, Any
from datetime import datetime
from pydantic import BaseModel, Field


class JavaMethod(BaseModel):
    """Represents a Java method."""
    
    name: str = Field(..., description="Method name", examples=["mainProcess", "calculateValues"])
    visibility: str = Field(default="public", description="Method visibility", examples=["public", "private", "protected"])
    return_type: str = Field(..., description="Return type", examples=["void", "String", "BigDecimal", "int"])
    parameters: List[str] = Field(default_factory=list, description="Method parameters as strings", examples=[["String input", "int count"], ["BigDecimal value"]])
    annotations: List[str] = Field(default_factory=list, description="Method annotations", examples=[["@Override"], ["@PostConstruct"]])
    body: str = Field(..., description="Complete method body code", examples=["{\n    System.out.println(\"Hello\");\n    return;\n}"])
    javadoc: Optional[str] = Field(default=None, description="JavaDoc documentation")
    
    class Config:
        json_schema_extra = {
            "example": {
                "name": "calculateSum",
                "visibility": "public",
                "return_type": "BigDecimal",
                "parameters": ["BigDecimal num1", "BigDecimal num2"],
                "annotations": [],
                "body": "{\n    return num1.add(num2);\n}",
                "javadoc": "/**\n * Calculates the sum of two numbers.\n */"
            }
        }


class JavaField(BaseModel):
    """Represents a Java field."""
    
    name: str = Field(..., description="Field name")
    type: str = Field(..., description="Field type")
    visibility: str = Field(default="private", description="Field visibility")
    modifiers: List[str] = Field(default_factory=list, description="Field modifiers (static, final, etc.)")
    annotations: List[str] = Field(default_factory=list, description="Field annotations")
    initial_value: Optional[str] = Field(default=None, description="Initial value")


class JavaClass(BaseModel):
    """Represents a Java class."""
    
    class_name: str = Field(..., description="Class name")
    package_name: str = Field(..., description="Package name")
    visibility: str = Field(default="public", description="Class visibility")
    modifiers: List[str] = Field(default_factory=list, description="Class modifiers")
    extends: Optional[str] = Field(default=None, description="Parent class")
    implements: List[str] = Field(default_factory=list, description="Implemented interfaces")
    annotations: List[str] = Field(default_factory=list, description="Class annotations")
    
    # Class content
    imports: List[str] = Field(default_factory=list, description="Import statements")
    fields: List[JavaField] = Field(default_factory=list, description="Class fields")
    methods: List[JavaMethod] = Field(default_factory=list, description="Class methods")
    inner_classes: List[str] = Field(default_factory=list, description="Inner class definitions")
    
    # Documentation
    javadoc: Optional[str] = Field(default=None, description="Class JavaDoc")


class JavaFile(BaseModel):
    """Represents a generated Java file."""
    
    file_name: str = Field(..., description="Java file name")
    package_name: str = Field(..., description="Package name")
    class_name: str = Field(..., description="Main class name")
    content: str = Field(..., description="Complete Java file content")
    
    # Source information
    original_cobol_file_name: str = Field(..., description="Original COBOL file name")
    migration_date: datetime = Field(default_factory=datetime.utcnow, description="When the migration was performed")
    
    # Structure
    java_class: Optional[JavaClass] = Field(default=None, description="Parsed Java class structure")
    
    # Quarkus-specific
    is_rest_endpoint: bool = Field(default=False, description="Whether this is a REST endpoint")
    is_entity: bool = Field(default=False, description="Whether this is a JPA entity")
    is_service: bool = Field(default=False, description="Whether this is a service class")
    is_repository: bool = Field(default=False, description="Whether this is a repository class")
    is_config: bool = Field(default=False, description="Whether this is a configuration class")
    
    # Dependencies
    quarkus_extensions: List[str] = Field(default_factory=list, description="Required Quarkus extensions")
    maven_dependencies: List[str] = Field(default_factory=list, description="Maven dependencies")
    
    # Migration metadata
    migration_complexity: str = Field(default="medium", description="Migration complexity")
    conversion_notes: List[str] = Field(default_factory=list, description="Conversion notes")
    manual_review_required: bool = Field(default=False, description="Whether manual review is required")
    todo_items: List[str] = Field(default_factory=list, description="TODO items for completion")
    
    # Quality metrics
    estimated_accuracy: float = Field(default=0.8, description="Estimated conversion accuracy (0-1)")
    test_coverage_recommendation: str = Field(default="high", description="Recommended test coverage level")
    
    # File metadata
    size_bytes: int = Field(default=0, description="Generated file size in bytes")
    lines_of_code: int = Field(default=0, description="Lines of code generated")
    
    class Config:
        """Pydantic configuration."""
        json_encoders = {
            datetime: lambda v: v.isoformat() if v else None
        }


class JavaProject(BaseModel):
    """Represents a complete Java project structure."""
    
    project_name: str = Field(..., description="Project name")
    base_package: str = Field(..., description="Base package name")
    
    # Project files
    java_files: List[JavaFile] = Field(default_factory=list, description="Generated Java files")
    
    # Build configuration
    maven_pom: Optional[str] = Field(default=None, description="Maven POM content")
    gradle_build: Optional[str] = Field(default=None, description="Gradle build script")
    
    # Quarkus configuration
    application_properties: Dict[str, str] = Field(default_factory=dict, description="Application properties")
    quarkus_extensions: List[str] = Field(default_factory=list, description="Quarkus extensions used")
    
    # Documentation
    readme_content: Optional[str] = Field(default=None, description="README.md content")
    migration_report: Optional[str] = Field(default=None, description="Migration report")
    
    # Metadata
    creation_date: datetime = Field(default_factory=datetime.utcnow, description="Project creation date")
    source_cobol_files: List[str] = Field(default_factory=list, description="Source COBOL file names")
    
    class Config:
        """Pydantic configuration."""
        json_encoders = {
            datetime: lambda v: v.isoformat() if v else None
        }