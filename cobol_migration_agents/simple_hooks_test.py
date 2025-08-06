"""
Simple test to understand what data we can get from Instructor hooks.

This script creates a minimal test to inspect all hook parameters and responses.
"""

import instructor
import openai
import json
import pprint
from datetime import datetime
from pathlib import Path
from pydantic import BaseModel
from dotenv import load_dotenv
import os

# Load environment variables
load_dotenv(Path("config/settings.local.env"))
load_dotenv(Path(".env"))

class SimpleModel(BaseModel):
    message: str
    word_count: int

def inspect_hook_data():
    """Simple test to inspect what data we get from each hook."""
    
    # Get API key from environment
    api_key = os.getenv("AZURE_OPENAI_API_KEY")
    model_id = os.getenv("AZURE_OPENAI_MODEL_ID", "gpt-4o-mini")
    service_type = os.getenv("AI_SERVICE_TYPE", "OpenAI")
    
    if not api_key or "your-api-key" in api_key:
        print("❌ No valid API key found. Please check your .env file")
        return
    
    print(f"🔧 Using service: {service_type}")
    print(f"🔧 Using model: {model_id}")
    print(f"🔧 API key: {api_key[:10]}...")
    
    # Configure OpenAI client
    if service_type.lower() == "openai":
        openai_client = openai.OpenAI(api_key=api_key)
    else:
        endpoint = os.getenv("AZURE_OPENAI_ENDPOINT")
        if not endpoint:
            print("❌ Azure endpoint not found for Azure OpenAI")
            return
        api_version = os.getenv("AZURE_OPENAI_API_VERSION", "2024-02-01")
        openai_client = openai.AzureOpenAI(
            api_key=api_key,
            azure_endpoint=endpoint,
            api_version=api_version
        )
    
    # Create Instructor client
    client = instructor.from_openai(openai_client)
    
    print("\n" + "="*60)
    print("🔍 INSPECTOR: Setting up hooks to examine data...")
    print("="*60)
    
    # Hook per esaminare kwargs
    def inspect_kwargs(*args, **kwargs):
        print("\n🚀 HOOK: completion:kwargs")
        print("   Args:", args)
        print("   Kwargs keys:", list(kwargs.keys()))
        
        for key, value in kwargs.items():
            print(f"   {key}: {type(value)}")
            if key == "messages":
                print(f"      Messages count: {len(value) if value else 0}")
                for i, msg in enumerate(value or []):
                    print(f"      Message {i}: {type(msg)} - {msg.get('role', 'no role') if isinstance(msg, dict) else 'not dict'}")
            elif key == "model":
                print(f"      Model: {value}")
            elif key == "tools":
                print(f"      Tools count: {len(value) if value else 0}")
                if value:
                    print(f"      First tool: {value[0].get('type', 'no type') if isinstance(value[0], dict) else 'not dict'}")
            elif key in ["temperature", "max_tokens", "timeout"]:
                print(f"      {key}: {value}")
        
        return args, kwargs
    
    # Hook per esaminare response
    def inspect_response(response):
        print("\n✅ HOOK: completion:response")
        print(f"   Response type: {type(response)}")
        print(f"   Response attributes: {dir(response)}")
        
        # Check for usage information
        if hasattr(response, 'usage'):
            usage = response.usage
            print(f"   Usage object: {type(usage)}")
            print(f"   Usage attributes: {dir(usage)}")
            if usage:
                print(f"   Prompt tokens: {getattr(usage, 'prompt_tokens', 'not available')}")
                print(f"   Completion tokens: {getattr(usage, 'completion_tokens', 'not available')}")
                print(f"   Total tokens: {getattr(usage, 'total_tokens', 'not available')}")
        else:
            print("   ❌ No usage attribute found")
        
        # Check for other useful attributes
        for attr in ['id', 'model', 'choices', 'created', 'object']:
            if hasattr(response, attr):
                value = getattr(response, attr)
                print(f"   {attr}: {type(value)} = {value}")
        
        return response
    
    # Hook per esaminare errori
    def inspect_error(error):
        print("\n❌ HOOK: completion:error")
        print(f"   Error type: {type(error)}")
        print(f"   Error message: {str(error)}")
        print(f"   Error attributes: {dir(error)}")
        return error
    
    # Hook per parsing errors
    def inspect_parse_error(error):
        print("\n🔧 HOOK: parse:error")
        print(f"   Parse error type: {type(error)}")
        print(f"   Parse error message: {str(error)}")
        return error
    
    # Hook per last attempt
    def inspect_last_attempt(error):
        print("\n🔄 HOOK: completion:last_attempt")
        print(f"   Last attempt error: {str(error)}")
        return error
    
    # Register all hooks
    client.on("completion:kwargs", inspect_kwargs)
    client.on("completion:response", inspect_response)
    client.on("completion:error", inspect_error)
    client.on("parse:error", inspect_parse_error)
    client.on("completion:last_attempt", inspect_last_attempt)
    
    print("✅ All hooks registered!")
    
    # Test 1: Simple successful call
    print("\n" + "="*60)
    print("🧪 TEST 1: Simple successful call")
    print("="*60)
    
    try:
        start_time = datetime.now()
        
        response = client.chat.completions.create(
            model=model_id,
            response_model=SimpleModel,
            messages=[
                {"role": "user", "content": "Say hello and count the words in your response"}
            ]
        )
        
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        
        print(f"\n✅ TEST 1 COMPLETED")
        print(f"   Duration: {duration:.2f} seconds")
        print(f"   Response: {response.message}")
        print(f"   Word count: {response.word_count}")
        
    except Exception as e:
        print(f"\n❌ TEST 1 FAILED: {e}")
    
    # Test 2: Call that might trigger validation error
    print("\n" + "="*60)
    print("🧪 TEST 2: Call with potential validation issues")
    print("="*60)
    
    try:
        class StrictModel(BaseModel):
            number: int  # We'll ask for something that's not a number
        
        start_time = datetime.now()
        
        response = client.chat.completions.create(
            model=model_id,
            response_model=StrictModel,
            max_retries=1,  # Limited retries
            messages=[
                {"role": "user", "content": "Please respond with the word 'hello' (not a number)"}
            ]
        )
        
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        
        print(f"\n✅ TEST 2 COMPLETED (unexpected)")
        print(f"   Duration: {duration:.2f} seconds")
        print(f"   Response: {response.number}")
        
    except Exception as e:
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()
        print(f"\n⚠️ TEST 2 FAILED AS EXPECTED: {e}")
        print(f"   Duration: {duration:.2f} seconds")
    
    print("\n" + "="*60)
    print("🔍 INSPECTION COMPLETE!")
    print("="*60)
    print("\nℹ️  Review the output above to understand:")
    print("   1. What parameters are available in completion:kwargs")
    print("   2. What token usage data is in completion:response")
    print("   3. How errors are structured")
    print("   4. Timing information you can calculate")


if __name__ == "__main__":
    print("🔍 INSTRUCTOR HOOKS INSPECTOR")
    print("Testing what data we can extract from Instructor hooks...")
    inspect_hook_data()