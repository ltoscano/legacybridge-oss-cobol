"""
Test implementation for Instructor hooks integration with LoggingService.

This script demonstrates how to extend the existing LoggingService to capture
token usage and API metrics using Instructor's hook system.
"""

import instructor
import openai
import asyncio
import json
from datetime import datetime
from typing import Dict, Any, Optional
from pathlib import Path

# Import the existing logging service
from cobol_migration_agents.services.logging_service import LoggingService
from cobol_migration_agents.config.settings import Settings


class InstructorHooksLogger:
    """
    Enhanced logger that captures token consumption and API metrics
    using Instructor's hook system.
    """
    
    def __init__(self, logging_service: LoggingService):
        """Initialize with existing logging service."""
        self.logging_service = logging_service
        self.current_request_start: Optional[datetime] = None
        self.current_agent_name: str = "unknown"
        self.hooks_data: Dict[str, Any] = {}
        
    def setup_hooks(self, client) -> None:
        """Setup Instructor hooks for token tracking."""
        
        # Hook per catturare i parametri della richiesta
        def log_completion_kwargs(*args, **kwargs):
            """Capture request parameters and start timing."""
            self.current_request_start = datetime.utcnow()
            self.hooks_data = {
                "model": kwargs.get("model", "unknown"),
                "messages": kwargs.get("messages", []),
                "tools": kwargs.get("tools", []),
                "start_time": self.current_request_start,
                "prompt_tokens": 0,
                "completion_tokens": 0,
                "total_tokens": 0
            }
            
            # Estimate prompt tokens (rough approximation)
            prompt_text = ""
            for msg in kwargs.get("messages", []):
                if isinstance(msg, dict) and "content" in msg:
                    prompt_text += str(msg["content"]) + " "
            
            # Rough token estimation (4 chars per token average)
            estimated_prompt_tokens = len(prompt_text) // 4
            self.hooks_data["estimated_prompt_tokens"] = estimated_prompt_tokens
            
            print(f"🚀 API Request starting - Model: {self.hooks_data['model']}, "
                  f"Estimated prompt tokens: {estimated_prompt_tokens}")
        
        # Hook per catturare la risposta
        def log_completion_response(response):
            """Capture response and extract token usage."""
            if hasattr(response, 'usage') and response.usage:
                usage = response.usage
                self.hooks_data.update({
                    "prompt_tokens": getattr(usage, 'prompt_tokens', 0),
                    "completion_tokens": getattr(usage, 'completion_tokens', 0),
                    "total_tokens": getattr(usage, 'total_tokens', 0)
                })
                
                # Calculate duration
                end_time = datetime.utcnow()
                duration = (end_time - self.current_request_start).total_seconds() if self.current_request_start else 0
                
                # Estimate cost
                cost = self._estimate_cost(
                    self.hooks_data["model"],
                    self.hooks_data["prompt_tokens"],
                    self.hooks_data["completion_tokens"]
                )
                
                print(f"✅ API Response received - "
                      f"Prompt: {self.hooks_data['prompt_tokens']}, "
                      f"Completion: {self.hooks_data['completion_tokens']}, "
                      f"Total: {self.hooks_data['total_tokens']} tokens, "
                      f"Duration: {duration:.2f}s, "
                      f"Cost: ${cost:.4f}")
                
                # Log to the existing logging service
                asyncio.create_task(self.logging_service.log_api_call(
                    agent_name=self.current_agent_name,
                    model=self.hooks_data["model"],
                    prompt_tokens=self.hooks_data["prompt_tokens"],
                    completion_tokens=self.hooks_data["completion_tokens"],
                    total_tokens=self.hooks_data["total_tokens"],
                    duration_seconds=duration,
                    cost_estimate=cost,
                    request_id=getattr(response, 'id', None)
                ))
                
            else:
                print("⚠️ No usage information in response")
        
        # Hook per catturare errori
        def log_completion_error(error):
            """Log API errors."""
            end_time = datetime.utcnow()
            duration = (end_time - self.current_request_start).total_seconds() if self.current_request_start else 0
            
            print(f"❌ API Error: {str(error)}")
            
            # Log error to the existing logging service
            asyncio.create_task(self.logging_service.log_api_call(
                agent_name=self.current_agent_name,
                model=self.hooks_data.get("model", "unknown"),
                prompt_tokens=self.hooks_data.get("estimated_prompt_tokens", 0),
                completion_tokens=0,
                total_tokens=self.hooks_data.get("estimated_prompt_tokens", 0),
                duration_seconds=duration,
                cost_estimate=0.0,
                error=str(error)
            ))
        
        # Hook per l'ultimo tentativo
        def log_last_attempt(error):
            """Log when we're making the last retry attempt."""
            print(f"🔄 Last retry attempt due to: {str(error)}")
        
        # Registra tutti gli hook
        client.on("completion:kwargs", log_completion_kwargs)
        client.on("completion:response", log_completion_response)
        client.on("completion:error", log_completion_error)
        client.on("completion:last_attempt", log_last_attempt)
        
        print("✅ Instructor hooks registered successfully!")
    
    def set_current_agent(self, agent_name: str) -> None:
        """Set the current agent name for logging context."""
        self.current_agent_name = agent_name
    
    def _estimate_cost(self, model: str, prompt_tokens: int, completion_tokens: int) -> float:
        """Estimate API cost based on model and token usage."""
        # Pricing per 1K tokens (aggiornato a gennaio 2025)
        pricing = {
            'gpt-4': {'prompt': 0.03, 'completion': 0.06},
            'gpt-4o': {'prompt': 0.005, 'completion': 0.015},
            'gpt-4o-mini': {'prompt': 0.00015, 'completion': 0.0006},
            'gpt-4-turbo': {'prompt': 0.01, 'completion': 0.03},
            'gpt-3.5-turbo': {'prompt': 0.001, 'completion': 0.002}
        }
        
        # Default pricing if model not found
        default_pricing = {'prompt': 0.01, 'completion': 0.03}
        model_pricing = pricing.get(model.lower(), default_pricing)
        
        prompt_cost = (prompt_tokens / 1000) * model_pricing['prompt']
        completion_cost = (completion_tokens / 1000) * model_pricing['completion']
        
        return prompt_cost + completion_cost


def test_instructor_hooks_integration():
    """Test the Instructor hooks integration."""
    
    # Load settings
    settings = Settings.from_env()
    
    # Check if we have valid API key
    if not settings.ai_settings.api_key or "your-api-key" in settings.ai_settings.api_key:
        print("❌ No valid API key found in configuration")
        print("Please configure your API key in the .env file")
        return
    
    # Initialize logging service
    logging_service = LoggingService(settings)
    logging_service.start_session("instructor_hooks_test_001")
    
    # Initialize hooks logger
    hooks_logger = InstructorHooksLogger(logging_service)
    
    # Configure OpenAI client based on settings
    if settings.ai_settings.service_type.lower() == "openai":
        # Standard OpenAI
        openai_client = openai.OpenAI(api_key=settings.ai_settings.api_key)
    else:
        # Azure OpenAI
        openai_client = openai.AzureOpenAI(
            api_key=settings.ai_settings.api_key,
            azure_endpoint=settings.ai_settings.endpoint,
            api_version=settings.ai_settings.api_version
        )
    
    # Create Instructor client with hooks
    client = instructor.from_openai(openai_client)
    hooks_logger.setup_hooks(client)
    
    print("🧪 Testing Instructor hooks integration...")
    print("=" * 60)
    
    # Test 1: Simple query to test token tracking
    print("\n📋 Test 1: Simple query")
    hooks_logger.set_current_agent("CobolAnalyzerAgent")
    
    try:
        from pydantic import BaseModel
        
        class SimpleResponse(BaseModel):
            message: str
            tokens_estimated: int
        
        response = client.chat.completions.create(
            model=settings.ai_settings.model_id,
            response_model=SimpleResponse,
            messages=[
                {"role": "user", "content": "Hello! Please respond with a simple message and estimate how many tokens this conversation uses."}
            ]
        )
        
        print(f"Response: {response.message}")
        print(f"Estimated tokens in response: {response.tokens_estimated}")
        
    except Exception as e:
        print(f"❌ Test 1 failed: {e}")
    
    # Test 2: More complex query to test higher token usage
    print("\n📋 Test 2: Complex query")
    hooks_logger.set_current_agent("JavaConverterAgent")
    
    try:
        class CodeAnalysis(BaseModel):
            complexity: str
            recommendations: list[str]
            estimated_conversion_time: str
        
        response = client.chat.completions.create(
            model=settings.ai_settings.model_id,
            response_model=CodeAnalysis,
            messages=[
                {"role": "system", "content": "You are a COBOL to Java migration expert."},
                {"role": "user", "content": """
                Analyze this COBOL code snippet and provide conversion recommendations:
                
                IDENTIFICATION DIVISION.
                PROGRAM-ID. HELLO-WORLD.
                
                DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 WS-MESSAGE PIC X(20) VALUE 'HELLO WORLD'.
                
                PROCEDURE DIVISION.
                DISPLAY WS-MESSAGE.
                STOP RUN.
                """}
            ]
        )
        
        print(f"Complexity: {response.complexity}")
        print(f"Recommendations: {response.recommendations}")
        print(f"Estimated time: {response.estimated_conversion_time}")
        
    except Exception as e:
        print(f"❌ Test 2 failed: {e}")
    
    # Test 3: Test error handling
    print("\n📋 Test 3: Error handling")
    hooks_logger.set_current_agent("TestAgent")
    
    try:
        # This should fail validation to test error hooks
        class StrictModel(BaseModel):
            number: int  # This should be a number, but we'll ask for text
        
        response = client.chat.completions.create(
            model=settings.ai_settings.model_id,
            response_model=StrictModel,
            max_retries=2,  # Limited retries to test error handling
            messages=[
                {"role": "user", "content": "Please respond with the word 'hello' (not a number)"}
            ]
        )
        
        print(f"Response: {response.number}")
        
    except Exception as e:
        print(f"Expected error (testing error handling): {e}")
    
    print("\n" + "=" * 60)
    print("🧪 Testing completed!")
    
    # Show session statistics
    stats = logging_service.get_session_statistics()
    print(f"\n📈 Session Statistics:")
    print(f"   Total API calls: {stats.get('api_calls', 0)}")
    print(f"   Total tokens: {stats.get('total_tokens', 0)}")
    print(f"   Total cost: ${stats.get('total_cost', 0):.4f}")
    print(f"   Errors: {stats.get('errors', 0)}")
    
    return logging_service


async def test_async_hooks_integration():
    """Test async integration with conversation logging."""
    
    # Load settings
    settings = Settings.from_env()
    
    if not settings.ai_settings.api_key or "your-api-key" in settings.ai_settings.api_key:
        print("❌ No valid API key found in configuration")
        return
    
    # Initialize logging service
    logging_service = LoggingService(settings)
    logging_service.start_session("async_hooks_test_001")
    
    # Initialize hooks logger
    hooks_logger = InstructorHooksLogger(logging_service)
    
    # Configure client
    if settings.ai_settings.service_type.lower() == "openai":
        openai_client = openai.OpenAI(api_key=settings.ai_settings.api_key)
    else:
        openai_client = openai.AzureOpenAI(
            api_key=settings.ai_settings.api_key,
            azure_endpoint=settings.ai_settings.endpoint,
            api_version=settings.ai_settings.api_version
        )
    
    client = instructor.from_openai(openai_client)
    hooks_logger.setup_hooks(client)
    
    print("🔄 Testing async conversation logging...")
    
    from pydantic import BaseModel
    
    class ConversationResponse(BaseModel):
        response: str
        sentiment: str
        
    # Simulate a conversation with multiple messages
    conversation_turns = [
        "Hello, I need help migrating COBOL code to Java",
        "What are the main challenges in COBOL to Java migration?",
        "How do you handle COBOL copybooks in Java?",
        "Thank you for the help!"
    ]
    
    for i, user_message in enumerate(conversation_turns):
        print(f"\n💬 Turn {i+1}: {user_message}")
        hooks_logger.set_current_agent(f"ConversationAgent_Turn_{i+1}")
        
        # Log user message
        await logging_service.log_conversation_message(
            role="user",
            agent_name=f"ConversationAgent_Turn_{i+1}",
            content=user_message,
            tokens=len(user_message) // 4,  # Rough estimate
            metadata={"turn": i+1, "conversation_id": "test_conversation"}
        )
        
        try:
            response = client.chat.completions.create(
                model=settings.ai_settings.model_id,
                response_model=ConversationResponse,
                messages=[
                    {"role": "system", "content": "You are a helpful COBOL to Java migration assistant."},
                    {"role": "user", "content": user_message}
                ]
            )
            
            # Log assistant response
            await logging_service.log_conversation_message(
                role="assistant",
                agent_name=f"ConversationAgent_Turn_{i+1}",
                content=response.response,
                tokens=len(response.response) // 4,  # Rough estimate
                metadata={"turn": i+1, "sentiment": response.sentiment, "conversation_id": "test_conversation"}
            )
            
            print(f"🤖 Response: {response.response}")
            print(f"😊 Sentiment: {response.sentiment}")
            
        except Exception as e:
            print(f"❌ Error in turn {i+1}: {e}")
    
    # Export conversation log
    try:
        log_file = await logging_service.export_conversation_log("./logs")
        print(f"\n📄 Conversation log exported to: {log_file}")
    except Exception as e:
        print(f"❌ Failed to export conversation log: {e}")
    
    # Export API statistics
    try:
        stats_file = await logging_service.export_api_statistics("./logs")
        print(f"📈 API statistics exported to: {stats_file}")
    except Exception as e:
        print(f"❌ Failed to export API statistics: {e}")


if __name__ == "__main__":
    print("🚀 Starting Instructor Hooks Integration Test")
    print("=" * 70)
    
    # Test 1: Basic synchronous hooks
    logging_service = test_instructor_hooks_integration()
    
    print("\n" + "=" * 70)
    
    # Test 2: Async conversation logging
    asyncio.run(test_async_hooks_integration())
    
    print("\n✅ All tests completed!")
    print("\nℹ️  Check the ./logs directory for exported conversation logs and API statistics.")
