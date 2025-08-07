"""Test DependencyMapper agent with different API providers to debug validation issues."""

import asyncio
import pytest
from pathlib import Path

from cobol_migration_agents.config.settings import Settings
from cobol_migration_agents.agents.dependency_mapper_agent import DependencyMapperAgent
from cobol_migration_agents.services.logging_service import LoggingService
from cobol_migration_agents.models.cobol_models import CobolFile


def test_dependency_mapper_with_different_providers():
    """Test DependencyMapper agent with current settings to debug DependencyOutputSchema validation errors."""
    
    # Load settings from environment (will use current config)
    settings = Settings.from_env()
    
    print(f"\n=== Testing DependencyMapper Agent ===")
    print(f"Service Type: {settings.ai_settings.service_type}")
    print(f"Endpoint: {settings.ai_settings.endpoint}")
    print(f"Model ID: {settings.ai_settings.model_id}")
    print(f"Instructor Mode: {settings.ai_settings.instructor_mode}")
    
    # Initialize logging service
    logging_service = LoggingService(settings)
    logging_service.start_session("test_dependency_mapper")
    
    # Initialize DependencyMapper agent
    dependency_mapper = DependencyMapperAgent(settings, logging_service)
    
    # Sample COBOL files to analyze
    cobol_files = [
        CobolFile(
            file_name="HELLO-WORLD.cbl",
            file_path="/app/data/cobol-source/HELLO-WORLD.cbl",
            content="""
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
""",
            file_type="program",
            size_bytes=200
        ),
        CobolFile(
            file_name="COMMON-DATA.cpy",
            file_path="/app/data/cobol-source/COMMON-DATA.cpy",
            content="""
      * Common data structures
       01 COMMON-AREA.
          05 COMMON-FLAG PIC X VALUE 'N'.
          05 COMMON-COUNTER PIC 9(5) VALUE ZERO.
          05 COMMON-MESSAGE PIC X(50) VALUE SPACES.
""",
            file_type="copybook",
            size_bytes=150
        )
    ]
    
    async def run_dependency_mapping():
        try:
            print(f"\n=== Starting Dependency Mapping ===")
            result = await dependency_mapper.analyze_dependencies(cobol_files)
            
            print(f"\n=== Dependency Mapping Result (DependencyMap) ===")
            print(f"Source Directory: {result.source_directory}")
            print(f"Analysis Date: {result.analysis_date}")
            
            # Print dependency map structure - result IS the DependencyMap
            print(f"\n=== Dependency Map Details ===")
            print(f"Dependencies: {len(result.dependencies)}")
            for dep in result.dependencies[:3]:  # Show first 3
                print(f"  - {dep.source_file} -> {dep.target_file} ({dep.dependency_type})")
            
            print(f"Program Dependencies: {len(result.program_dependencies)}")
            for prog_name, deps in list(result.program_dependencies.items())[:3]:
                print(f"  - {prog_name}: {deps}")
            
            print(f"Reverse Dependencies: {len(result.reverse_dependencies)}")
            for file_name, dependents in list(result.reverse_dependencies.items())[:3]:
                print(f"  - {file_name} used by: {dependents}")
            
            print(f"Copybook Usage: {len(result.copybook_usage)}")
            for copybook in result.copybook_usage[:3]:
                print(f"  - {copybook.copybook_name}: used {copybook.usage_count} times by {copybook.used_by}")
            
            print(f"\n=== Metrics ===")
            metrics = result.metrics
            print(f"Total Files: {metrics.total_files}")
            print(f"Total Programs: {metrics.total_programs}")  
            print(f"Total Copybooks: {metrics.total_copybooks}")
            print(f"Total Dependencies: {metrics.total_dependencies}")
            print(f"Average Dependencies per Program: {metrics.average_dependencies_per_program}")
            print(f"Max Dependencies for Single Program: {metrics.max_dependencies_for_single_program}")
            print(f"Programs with No Dependencies: {metrics.programs_with_no_dependencies}")
            print(f"Circular Dependencies: {len(metrics.circular_dependencies)}")
            print(f"Dependency Complexity Score: {metrics.dependency_complexity_score}")
            print(f"Migration Risk Level: {metrics.migration_risk_level}")
            
            print(f"\n=== Migration Planning ===")
            print(f"Migration Order: {result.migration_order}")
            print(f"Migration Groups: {result.migration_groups}")
            print(f"High Risk Dependencies: {len(result.high_risk_dependencies)}")
            
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
    
    # Run the async dependency mapping
    result = asyncio.run(run_dependency_mapping())
    
    # Print final statistics
    stats = logging_service.get_session_statistics()
    print(f"\n=== Session Statistics ===")
    print(f"API Calls: {stats.get('api_calls', 0)}")
    print(f"Total Tokens: {stats.get('total_tokens', 0)}")
    print(f"Total Cost: ${stats.get('total_cost', 0):.4f}")
    print(f"Errors: {stats.get('errors', 0)}")
    
    # Check for parsing errors in logs
    if stats.get('errors', 0) > 0:
        print(f"\n=== Error Analysis ===")
        print("Check the logs for detailed parsing errors.")
        print("Common issues with DependencyOutputSchema:")
        print("- Missing 'source_directory' field")
        print("- Missing 'metrics' field")
        print("- Missing required metrics subfields")
        print("- Incorrect data types (e.g., int instead of list)")
    
    # Return result for further inspection
    return result


def test_dependency_output_schema_structure():
    """Show the expected structure of DependencyOutputSchema for debugging."""
    
    from cobol_migration_agents.models.migration_schemas import DependencyOutputSchema
    from cobol_migration_agents.models.dependency_models import DependencyMap
    
    print(f"\n=== DependencyOutputSchema Expected Structure ===")
    
    # Get the schema
    schema = DependencyOutputSchema.model_json_schema()
    
    def print_schema_structure(schema_dict, indent=0):
        """Recursively print schema structure."""
        spaces = "  " * indent
        
        if "properties" in schema_dict:
            for field_name, field_info in schema_dict["properties"].items():
                field_type = field_info.get("type", "unknown")
                required = field_name in schema_dict.get("required", [])
                required_marker = " [REQUIRED]" if required else " [OPTIONAL]"
                
                print(f"{spaces}- {field_name}: {field_type}{required_marker}")
                
                if "properties" in field_info:
                    print_schema_structure(field_info, indent + 1)
                elif field_info.get("type") == "array" and "items" in field_info:
                    items = field_info["items"]
                    if "properties" in items:
                        print(f"{spaces}  Array items:")
                        print_schema_structure(items, indent + 2)
    
    print_schema_structure(schema)
    
    # Show example of correctly structured response
    print(f"\n=== Example Correct Response Structure ===")
    example = {
        "dependency_map": {
            "source_directory": "/path/to/source",
            "programs": {
                "PROGRAM1": {
                    "dependencies": ["COPYBOOK1", "COPYBOOK2"],
                    "used_by": ["PROGRAM2"]
                }
            },
            "copybooks": {
                "COPYBOOK1": {
                    "used_by": ["PROGRAM1", "PROGRAM2"]
                }
            },
            "metrics": {
                "total_files": 3,
                "total_dependencies": 2,
                "average_dependencies_per_program": 2.0,
                "max_dependencies_for_single_program": 2,
                "programs_with_no_dependencies": 0,
                "circular_dependencies": []
            }
        },
        "source_directory": "/path/to/source",
        "analysis_date": "2025-01-01T12:00:00Z"
    }
    
    import json
    print(json.dumps(example, indent=2))


if __name__ == "__main__":
    # Show expected schema structure
    test_dependency_output_schema_structure()
    
    # Run the actual test
    result = test_dependency_mapper_with_different_providers()
    
    if result:
        print(f"\n=== Test Completed Successfully ===")
    else:
        print(f"\n=== Test Failed with Exception ===")