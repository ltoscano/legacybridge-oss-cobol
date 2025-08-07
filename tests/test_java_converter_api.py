"""Test JavaConverter agent with different API providers to debug conversion issues."""

import asyncio
import pytest
from pathlib import Path

from cobol_migration_agents.config.settings import Settings
from cobol_migration_agents.agents.java_converter_agent import JavaConverterAgent
from cobol_migration_agents.services.logging_service import LoggingService
from cobol_migration_agents.models.cobol_models import CobolFile, CobolAnalysis, CobolProgram, CobolSection, CobolDataItem, CobolParagraph
from datetime import datetime


def test_java_converter_with_different_providers():
    """Test JavaConverter agent with current settings to debug conversion failures."""
    
    # Load settings from environment (will use current config)
    settings = Settings.from_env()
    
    print(f"\n=== Testing JavaConverter Agent ===")
    print(f"Service Type: {settings.ai_settings.service_type}")
    print(f"Endpoint: {settings.ai_settings.endpoint}")
    print(f"Model ID: {settings.ai_settings.model_id}")
    print(f"Instructor Mode: {settings.ai_settings.instructor_mode}")
    
    # Initialize logging service
    logging_service = LoggingService(settings)
    logging_service.start_session("test_java_converter")
    
    # Initialize JavaConverter agent
    java_converter = JavaConverterAgent(settings, logging_service)
    
    # Sample COBOL code to convert
    sample_cobol = """
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-MESSAGE PIC X(20) VALUE 'Hello, World!'.

PROCEDURE DIVISION.
MAIN-PARAGRAPH.
    DISPLAY WS-MESSAGE.
    STOP RUN.
"""
    
    # Create proper CobolFile object
    cobol_file = CobolFile(
        file_name="HELLO-WORLD.cbl",
        file_path="/app/data/cobol-source/HELLO-WORLD.cbl",
        content=sample_cobol,
        file_type="program",
        size_bytes=len(sample_cobol.encode('utf-8'))
    )
    
    # Create proper CobolAnalysis object
    cobol_analysis = CobolAnalysis(
        file_name="HELLO-WORLD.cbl",
        program=CobolProgram(
            program_id="HELLO-WORLD",
            sections=[
                CobolSection(
                    name="WORKING-STORAGE SECTION",
                    type="SECTION",
                    content="01 WS-MESSAGE PIC X(20) VALUE 'Hello, World!'.",
                    line_start=7,
                    line_end=7
                )
            ],
            working_storage=[
                CobolDataItem(
                    name="WS-MESSAGE",
                    level=1,
                    picture_clause="PIC X(20)",
                    value="'Hello, World!'",
                    line_number=7
                )
            ],
            paragraphs=[
                CobolParagraph(
                    name="MAIN-PARAGRAPH",
                    content="DISPLAY WS-MESSAGE.\nSTOP RUN.",
                    line_start=10,
                    line_end=12,
                    calls=[]
                )
            ],
            total_lines=12,
            executable_lines=2,
            comment_lines=0
        ),
        cyclomatic_complexity=1,
        nesting_depth=0,
        uses_goto=False,
        uses_perform=False,
        uses_evaluate=False,
        uses_file_io=False,
        uses_database=False
    )
    
    async def run_conversion():
        try:
            print(f"\n=== Starting Conversion ===")
            result = await java_converter.convert_to_java(
                cobol_file=cobol_file,
                cobol_analysis=cobol_analysis
            )
            
            print(f"\n=== Conversion Result ===")
            print(f"File Name: {result.file_name}")
            print(f"Class Name: {result.class_name}")
            print(f"Package Name: {result.package_name}")
            print(f"Original COBOL File: {result.original_cobol_file_name}")
            
            print(f"\n=== Generated Java Code ===")
            print(result.content[:500] + "..." if len(result.content) > 500 else result.content)
            
            return result
            
        except Exception as e:
            print(f"\n=== Exception Occurred ===")
            print(f"Exception Type: {type(e).__name__}")
            print(f"Exception Message: {str(e)}")
            
            # Print more detailed error information
            import traceback
            print(f"\n=== Full Traceback ===")
            print(traceback.format_exc())
            
            return None
    
    # Run the async conversion
    result = asyncio.run(run_conversion())
    
    # Print final statistics
    stats = logging_service.get_session_statistics()
    print(f"\n=== Session Statistics ===")
    print(f"API Calls: {stats.get('api_calls', 0)}")
    print(f"Total Tokens: {stats.get('total_tokens', 0)}")
    print(f"Total Cost: ${stats.get('total_cost', 0):.4f}")
    print(f"Errors: {stats.get('errors', 0)}")
    
    # Return result for further inspection
    return result


if __name__ == "__main__":
    # Run the test directly
    result = test_java_converter_with_different_providers()
    
    if result:
        print(f"\n=== Test Completed Successfully ===")
    else:
        print(f"\n=== Test Failed with Exception ===")