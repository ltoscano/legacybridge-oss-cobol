"""Test to intercept Instructor validation errors and show raw responses."""

import asyncio
import json
from openai import AsyncOpenAI
import instructor
from cobol_migration_agents.config.settings import Settings
from cobol_migration_agents.models.migration_schemas import DependencyOutputSchema
import json
from typing import Union

async def test_intercept_instructor_error():
    """Intercept Instructor errors to see the raw response that's causing validation issues."""
    
    # Load settings
    settings = Settings.from_env()
    
    print(f"\n=== Intercepting Instructor Validation Errors ===")
    print(f"Service Type: {settings.ai_settings.service_type}")
    print(f"Endpoint: {settings.ai_settings.endpoint}")
    print(f"Model ID: {settings.ai_settings.model_id}")
    print(f"Instructor Mode: {settings.ai_settings.instructor_mode}")
    
    # Create OpenAI client
    if settings.ai_settings.endpoint:
        openai_client = AsyncOpenAI(
            api_key=settings.ai_settings.api_key,
            base_url=settings.ai_settings.endpoint
        )
    else:
        openai_client = AsyncOpenAI(api_key=settings.ai_settings.api_key)
    
    # Create instructor client with the same mode as the agent
    instructor_mode = instructor.Mode.TOOLS  # From your current config
    client = instructor.from_openai(openai_client, mode=instructor_mode)
    
    # Hook per visualizzare il prompt completo inviato all'LLM
    def debug_prompt(**kwargs):
        print(f"\n{'='*80}")
        print("PROMPT FINALE COMPLETO INVIATO ALL'LLM:")
        print(f"{'='*80}")
        
        print(f"Model: {kwargs.get('model', 'unknown')}")
        print(f"Max Tokens: {kwargs.get('max_tokens', 'unknown')}")
        print(f"Temperature: {kwargs.get('temperature', 'unknown')}")
        print(f"Top P: {kwargs.get('top_p', 'unknown')}")
        
        messages = kwargs.get('messages', [])
        for i, msg in enumerate(messages):
            print(f"\nMESSAGE {i+1} ({msg.get('role', 'unknown').upper()}):")
            print(msg.get('content', ''))
        
        print(f"\n{'='*80}")
        tools = kwargs.get('tools', [])
        if tools:
            print(f"\nSCHEMA/TOOLS:")
            print(json.dumps(tools, indent=2))
        
        print(f"\n{'='*80}")
        tool_choice = kwargs.get('tool_choice')
        if tool_choice:
            print(f"\nTOOL_CHOICE:")
            print(json.dumps(tool_choice, indent=2))
        
        print(f"{'='*80}")
    
    client.on("completion:kwargs", debug_prompt)
    
    # Simple prompt that should trigger the error
    system_prompt = """You are a COBOL dependency analysis expert.

CRITICAL: You MUST call the DependencyOutputSchema function with a valid JSON object.
DO NOT return JSON as a string. 
DO NOT add any explanatory text.
DO NOT wrap the response in markdown code blocks.

Return ONLY the function call with the properly structured object."""

    user_prompt = """Analyze these COBOL files:

1. HELLO-WORLD.cbl:
```
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-MESSAGE PIC X(20) VALUE 'Hello, World!'.
PROCEDURE DIVISION.
MAIN-PARAGRAPH.
    DISPLAY WS-MESSAGE.
    STOP RUN.
```

2. COMMON-DATA.cpy:
```
01 COMMON-AREA.
   05 COMMON-FLAG PIC X VALUE 'N'.
   05 COMMON-COUNTER PIC 9(5) VALUE ZERO.
```

Provide the dependency analysis."""

    try:
        print(f"\n=== Making Instructor Request ===")
        
        # Try to get structured response (this should fail)
        response = await client.chat.completions.create(
            model=settings.ai_settings.model_id,
            response_model=DependencyOutputSchema,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ],
            max_tokens=settings.ai_settings.max_tokens,
            temperature=settings.ai_settings.temperature,
            top_p=settings.ai_settings.top_p,
            max_retries=2
        )
        
        print(f"\n=== Instructor Response Successful ===")
        print(f"Response type: {type(response)}")
        print(f"Response: {response}")
        
        return response
        
    except Exception as e:
        print(f"\n=== Instructor Error Caught ===")
        print(f"Error Type: {type(e).__name__}")
        print(f"Error Message: {str(e)}")
        print(e)
        
        # Try to extract the raw response from the error
        if hasattr(e, 'raw_response'):
            print(f"\n=== Raw Response from Error ===")
            print(e.raw_response)
        
        if hasattr(e, 'completion'):
            print(f"\n=== Completion from Error ===")
            completion = e.completion
            if hasattr(completion, 'choices') and completion.choices:
                raw_content = completion.choices[0].message.content
                print(f"Raw Content Length: {len(raw_content)}")
                print(f"Raw Content:")
                print(raw_content)
                
                # Try to parse the raw content
                print(f"\n=== Parsing Raw Content ===")
                try:
                    parsed = json.loads(raw_content)
                    print(f"✅ Valid JSON!")
                    print(f"Top-level keys: {list(parsed.keys())}")
                    
                    # Look for the problematic structure
                    if 'call_graph' in str(parsed):
                        print(f"🔍 Found 'call_graph' in response!")
                        print(f"Full structure: {json.dumps(parsed, indent=2)}")
                    
                except json.JSONDecodeError as json_e:
                    print(f"❌ Invalid JSON: {json_e}")
                    print(f"Problematic content around position {json_e.pos}:")
                    if json_e.pos < len(raw_content):
                        start = max(0, json_e.pos - 50)
                        end = min(len(raw_content), json_e.pos + 50)
                        print(f"'{raw_content[start:end]}'")
        
        # # Also try making a raw API call to compare
        # print(f"\n=== Making Raw API Call for Comparison ===")
        # try:
        #     raw_response = await openai_client.chat.completions.create(
        #         model=settings.ai_settings.model_id,
        #         messages=[
        #             {"role": "system", "content": system_prompt},
        #             {"role": "user", "content": user_prompt}
        #         ],
        #         max_tokens=settings.ai_settings.max_tokens,
        #         temperature=settings.ai_settings.temperature,
        #         top_p=settings.ai_settings.top_p
        #     )
            
        #     raw_content = raw_response.choices[0].message.content
        #     print(f"Raw API Content Length: {len(raw_content)}")
        #     print(f"Raw API Content (first 1000 chars):")
        #     print(raw_content[:1000])
            
        #     # Check for specific patterns that cause validation errors
        #     if 'call_graph' in raw_content:
        #         print(f"🔍 Found 'call_graph' in raw response!")
        #     if 'dependency_map' not in raw_content:
        #         print(f"❌ Missing 'dependency_map' in raw response!")
        #     if 'source_directory' not in raw_content:
        #         print(f"❌ Missing 'source_directory' in raw response!")
                
        # except Exception as raw_e:
        #     print(f"Raw API call also failed: {raw_e}")
        
        return None


async def test_simple_dependency_schema():
    """Test with a very simple, explicit prompt to see what the model returns."""
    
    settings = Settings.from_env()
    
    if settings.ai_settings.endpoint:
        client = AsyncOpenAI(
            api_key=settings.ai_settings.api_key,
            base_url=settings.ai_settings.endpoint
        )
    else:
        client = AsyncOpenAI(api_key=settings.ai_settings.api_key)
    
    simple_prompt = """Return exactly this JSON structure with your analysis:

{
  "dependency_map": {
    "source_directory": "/app/data/cobol-source",
    "metrics": {
      "total_files": 2,
      "total_dependencies": 0,
      "average_dependencies_per_program": 0.0,
      "max_dependencies_for_single_program": 0,
      "programs_with_no_dependencies": 1
    }
  },
  "mapping_confidence": 0.95
}

Analyze HELLO-WORLD.cbl and COMMON-DATA.cpy. Return ONLY the JSON, no other text."""

    try:
        response = await client.chat.completions.create(
            model=settings.ai_settings.model_id,
            messages=[{"role": "user", "content": simple_prompt}],
            max_tokens=1000,
            temperature=0.1
        )
        
        content = response.choices[0].message.content
        print(f"\n=== Simple Test Response ===")
        print(content)
        
        # Try to parse
        try:
            parsed = json.loads(content)
            print(f"✅ Simple test returned valid JSON!")
            return parsed
        except:
            print(f"❌ Simple test returned invalid JSON")
            return content
            
    except Exception as e:
        print(f"Simple test failed: {e}")
        return None


if __name__ == "__main__":
    print("=== Test 1: Intercept Instructor Error ===")
    result1 = asyncio.run(test_intercept_instructor_error())
    
    # print("\n" + "="*50)
    # print("=== Test 2: Simple Prompt Test ===")
    # result2 = asyncio.run(test_simple_dependency_schema())
    
    print(f"\n=== Summary ===")
    if result1:
        print("✅ Instructor test succeeded")
    else:
        print("❌ Instructor test failed (expected)")
        
    # if result2:
    #     print("✅ Simple test succeeded")
    # else:
    #     print("❌ Simple test failed")