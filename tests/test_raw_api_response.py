"""Test to capture raw API responses before Instructor processing."""

import asyncio
import json
from openai import AsyncOpenAI
from cobol_migration_agents.config.settings import Settings


async def test_raw_dependency_mapper_response():
    """Test to see the raw response from the API before Instructor processes it."""
    
    # Load settings
    settings = Settings.from_env()
    
    print(f"\n=== Raw API Response Test ===")
    print(f"Service Type: {settings.ai_settings.service_type}")
    print(f"Endpoint: {settings.ai_settings.endpoint}")
    print(f"Model ID: {settings.ai_settings.model_id}")
    print(f"Instructor Mode: {settings.ai_settings.instructor_mode}")
    
    # Create OpenAI client directly (like the agent does)
    if settings.ai_settings.endpoint:
        client = AsyncOpenAI(
            api_key=settings.ai_settings.api_key,
            base_url=settings.ai_settings.endpoint
        )
    else:
        client = AsyncOpenAI(api_key=settings.ai_settings.api_key)
    
    # Create a prompt similar to what DependencyMapperAgent would send
    system_prompt = """You are a COBOL dependency analysis expert. Analyze the given COBOL files and return a complete dependency mapping in the exact JSON structure specified.

You must return a valid JSON object that matches this schema:

{
  "dependency_map": {
    "analysis_date": "2025-01-01T12:00:00Z",
    "source_directory": "/path/to/source",
    "dependencies": [],
    "reverse_dependencies": {},
    "copybook_usage": [],
    "copybook_definitions": {},
    "program_dependencies": {},
    "program_calls": {},
    "metrics": {
      "total_files": 0,
      "total_programs": 0,
      "total_copybooks": 0,
      "total_dependencies": 0,
      "average_dependencies_per_program": 0.0,
      "max_dependencies_for_single_program": 0,
      "programs_with_no_dependencies": 0,
      "most_used_copybook": null,
      "most_used_copybook_count": 0,
      "unused_copybooks": [],
      "circular_dependencies": [],
      "has_circular_dependencies": false,
      "dependency_complexity_score": 0.0,
      "migration_risk_level": "low"
    },
    "mermaid_diagram": null,
    "graphviz_dot": null,
    "migration_order": [],
    "migration_groups": [],
    "high_risk_dependencies": [],
    "analysis_metadata": {}
  },
  "mapping_confidence": 0.95,
  "mapping_notes": [],
  "migration_recommendations": []
}

IMPORTANT: Return ONLY valid JSON. Do not include any explanation or markdown formatting."""

    user_prompt = """Analyze these COBOL files for dependencies:

File 1: HELLO-WORLD.cbl
```
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
```

File 2: COMMON-DATA.cpy
```
      * Common data structures
       01 COMMON-AREA.
          05 COMMON-FLAG PIC X VALUE 'N'.
          05 COMMON-COUNTER PIC 9(5) VALUE ZERO.
          05 COMMON-MESSAGE PIC X(50) VALUE SPACES.
```

Return the complete dependency analysis as JSON."""

    try:
        print(f"\n=== Sending Raw API Request ===")
        
        # Make raw API call
        response = await client.chat.completions.create(
            model=settings.ai_settings.model_id,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ],
            max_tokens=settings.ai_settings.max_tokens,
            temperature=settings.ai_settings.temperature
        )
        
        # Extract raw response
        raw_content = response.choices[0].message.content
        
        print(f"\n=== Raw API Response Content ===")
        print(f"Content Length: {len(raw_content)} characters")
        print(f"Content Preview (first 500 chars):")
        print(raw_content[:500])
        print("\n" + "="*50)
        print("FULL RESPONSE:")
        print(raw_content)
        print("="*50)
        
        # Try to parse as JSON
        print(f"\n=== JSON Parsing Test ===")
        try:
            parsed_json = json.loads(raw_content)
            print("✅ Valid JSON!")
            print(f"Top-level keys: {list(parsed_json.keys())}")
            
            # Check for dependency_map
            if "dependency_map" in parsed_json:
                dep_map = parsed_json["dependency_map"]
                print(f"dependency_map keys: {list(dep_map.keys())}")
                
                # Check specific missing fields
                missing_fields = []
                if "source_directory" not in dep_map:
                    missing_fields.append("source_directory")
                if "metrics" not in dep_map:
                    missing_fields.append("metrics")
                elif isinstance(dep_map["metrics"], dict):
                    metrics = dep_map["metrics"]
                    required_metrics = [
                        "total_files", "total_dependencies", "average_dependencies_per_program",
                        "max_dependencies_for_single_program", "programs_with_no_dependencies"
                    ]
                    for metric in required_metrics:
                        if metric not in metrics:
                            missing_fields.append(f"metrics.{metric}")
                    
                    # Check circular_dependencies type
                    if "circular_dependencies" in metrics:
                        if not isinstance(metrics["circular_dependencies"], list):
                            missing_fields.append(f"metrics.circular_dependencies (wrong type: {type(metrics['circular_dependencies'])})")
                
                if missing_fields:
                    print(f"❌ Missing required fields: {missing_fields}")
                else:
                    print("✅ All required fields present!")
            else:
                print("❌ Missing 'dependency_map' key")
                
        except json.JSONDecodeError as e:
            print(f"❌ Invalid JSON: {e}")
            print(f"Error at position {e.pos}")
            if e.pos < len(raw_content):
                print(f"Context around error: '{raw_content[max(0, e.pos-20):e.pos+20]}'")
        
        # Show token usage
        print(f"\n=== Token Usage ===")
        print(f"Prompt tokens: {response.usage.prompt_tokens}")
        print(f"Completion tokens: {response.usage.completion_tokens}")
        print(f"Total tokens: {response.usage.total_tokens}")
        
        return raw_content
        
    except Exception as e:
        print(f"\n=== API Error ===")
        print(f"Error: {e}")
        import traceback
        print(traceback.format_exc())
        return None


if __name__ == "__main__":
    result = asyncio.run(test_raw_dependency_mapper_response())
    
    if result:
        print(f"\n=== Test Completed ===")
        print("Check the output above to see exactly what the API returns.")
    else:
        print(f"\n=== Test Failed ===")