"""COBOL-related data models."""

from typing import List, Optional, Dict, Any
from datetime import datetime
from pydantic import BaseModel, Field


class CobolFile(BaseModel):
    """Represents a COBOL source file."""
    
    file_name: str = Field(..., description="Name of the COBOL file")
    file_path: str = Field(..., description="Full path to the COBOL file")
    content: str = Field(..., description="Content of the COBOL file")
    file_type: str = Field(..., description="Type of COBOL file (program, copybook, etc.)")
    size_bytes: int = Field(..., description="File size in bytes")
    last_modified: Optional[datetime] = Field(default=None, description="Last modification time")
    encoding: str = Field(default="utf-8", description="File encoding")
    
    class Config:
        """Pydantic configuration."""
        json_encoders = {
            datetime: lambda v: v.isoformat() if v else None
        }


class CobolSection(BaseModel):
    """Represents a section within COBOL code."""
    
    name: str = Field(..., description="Section name", examples=["IDENTIFICATION DIVISION", "DATA DIVISION"])
    type: str = Field(..., description="Section type", examples=["DIVISION", "SECTION"])
    content: str = Field(..., description="Section content", examples=["PROGRAM-ID. SIMPLE-CALC."])
    line_start: int = Field(..., description="Starting line number", examples=[1, 5, 10])
    line_end: int = Field(..., description="Ending line number", examples=[3, 8, 15])
    
    class Config:
        json_schema_extra = {
            "example": {
                "name": "IDENTIFICATION DIVISION",
                "type": "DIVISION", 
                "content": "PROGRAM-ID. SIMPLE-CALC.\nAUTHOR. MIGRATION-TEST.",
                "line_start": 1,
                "line_end": 4
            }
        }


class CobolDataItem(BaseModel):
    """Represents a data item in COBOL."""
    
    name: str = Field(..., description="Data item name")
    level: int = Field(..., description="COBOL level number")
    picture: Optional[str] = Field(default=None, description="PICTURE clause")
    usage: Optional[str] = Field(default=None, description="USAGE clause")
    value: Optional[str] = Field(default=None, description="VALUE clause")
    occurs: Optional[str] = Field(default=None, description="OCCURS clause")
    redefines: Optional[str] = Field(default=None, description="REDEFINES clause")


class CobolParagraph(BaseModel):
    """Represents a COBOL paragraph."""
    
    name: str = Field(..., description="Paragraph name", examples=["MAIN-PROCESS", "INITIALIZE-PROGRAM"])
    content: str = Field(..., description="Paragraph content", examples=["PERFORM INITIALIZE-PROGRAM\nPERFORN CALCULATE-VALUES"])
    line_start: int = Field(..., description="Starting line number", examples=[25, 30, 45])
    line_end: int = Field(..., description="Ending line number", examples=[28, 35, 50])
    calls: List[str] = Field(default_factory=list, description="Called paragraphs/sections", examples=[["INITIALIZE-PROGRAM", "CALCULATE-VALUES"]])
    
    class Config:
        json_schema_extra = {
            "example": {
                "name": "MAIN-PROCESS",
                "content": "PERFORM INITIALIZE-PROGRAM\nPERFORM CALCULATE-VALUES\nPERFORM DISPLAY-RESULTS\nSTOP RUN.",
                "line_start": 25,
                "line_end": 30,
                "calls": ["INITIALIZE-PROGRAM", "CALCULATE-VALUES", "DISPLAY-RESULTS"]
            }
        }


class CobolProgram(BaseModel):
    """Represents a COBOL program structure."""
    
    program_id: str = Field(..., description="PROGRAM-ID")
    author: Optional[str] = Field(default=None, description="AUTHOR from IDENTIFICATION DIVISION")
    date_written: Optional[str] = Field(default=None, description="DATE-WRITTEN")
    date_compiled: Optional[str] = Field(default=None, description="DATE-COMPILED")
    
    # Divisions and sections
    sections: List[CobolSection] = Field(default_factory=list, description="Program sections")
    working_storage: List[CobolDataItem] = Field(default_factory=list, description="Working Storage items")
    linkage_section: List[CobolDataItem] = Field(default_factory=list, description="Linkage Section items")
    file_section: List[CobolDataItem] = Field(default_factory=list, description="File Section items")
    
    # Procedure division
    paragraphs: List[CobolParagraph] = Field(default_factory=list, description="Procedure Division paragraphs")
    
    # Dependencies
    copy_statements: List[str] = Field(default_factory=list, description="COPY statements used")
    call_statements: List[str] = Field(default_factory=list, description="CALL statements used")
    
    # Metrics
    total_lines: int = Field(..., description="Total lines of code")
    executable_lines: int = Field(..., description="Executable lines of code")
    comment_lines: int = Field(..., description="Comment lines")


class CobolAnalysis(BaseModel):
    """Analysis results for a COBOL file."""
    
    file_name: str = Field(..., description="Name of the analyzed COBOL file")
    analysis_date: datetime = Field(default_factory=datetime.utcnow, description="When the analysis was performed")
    
    # Structural analysis
    program: Optional[CobolProgram] = Field(default=None, description="Program structure (for .cbl files)")
    copybook_items: List[CobolDataItem] = Field(default_factory=list, description="Copybook data items (for .cpy files)")
    
    # Complexity metrics
    cyclomatic_complexity: int = Field(default=0, description="Cyclomatic complexity")
    nesting_depth: int = Field(default=0, description="Maximum nesting depth")
    
    # Code patterns
    uses_goto: bool = Field(default=False, description="Whether GOTO statements are used")
    uses_perform: bool = Field(default=False, description="Whether PERFORM statements are used")
    uses_evaluate: bool = Field(default=False, description="Whether EVALUATE statements are used")
    uses_file_io: bool = Field(default=False, description="Whether file I/O is used")
    uses_database: bool = Field(default=False, description="Whether database operations are used")
    
    # Business logic indicators
    business_rules: List[str] = Field(default_factory=list, description="Identified business rules")
    data_transformations: List[str] = Field(default_factory=list, description="Data transformation patterns")
    validation_logic: List[str] = Field(default_factory=list, description="Validation logic patterns")
    
    # Migration complexity
    migration_complexity: str = Field(default="medium", description="Estimated migration complexity (low/medium/high)")
    migration_notes: List[str] = Field(default_factory=list, description="Notes for migration")
    potential_issues: List[str] = Field(default_factory=list, description="Potential migration issues")
    
    # Metadata
    analysis_metadata: Dict[str, Any] = Field(default_factory=dict, description="Additional analysis metadata")
    
    class Config:
        """Pydantic configuration."""
        json_encoders = {
            datetime: lambda v: v.isoformat() if v else None
        }