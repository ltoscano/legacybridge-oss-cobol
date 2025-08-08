"""Configuration settings for COBOL Migration Agents."""

import os
from pathlib import Path
from typing import Optional
from pydantic import BaseModel, Field
from dotenv import load_dotenv


class AISettings(BaseModel):
    """AI model configuration settings."""
    
    service_type: str = Field(default="AzureOpenAI", description="AI service provider")
    endpoint: Optional[str] = Field(default=None, description="Azure OpenAI endpoint")
    api_key: Optional[str] = Field(default=None, description="API key for authentication")
    api_version: str = Field(default=None, description="Azure OpenAI API version")
    deployment_name: Optional[str] = Field(default=None, description="Azure OpenAI deployment name")
    model_id: str = Field(default="gpt-4", description="Model identifier")
    
    # Specialized model configurations
    cobol_analyzer_model_id: Optional[str] = Field(default=None, description="Model for COBOL analysis")
    java_converter_model_id: Optional[str] = Field(default=None, description="Model for Java conversion")
    dependency_mapper_model_id: Optional[str] = Field(default=None, description="Model for dependency mapping")
    unit_test_model_id: Optional[str] = Field(default=None, description="Model for unit test generation")
    
    # Instructor configuration
    instructor_mode: Optional[str] = Field(default=None, description="Instructor mode (e.g., instructor.Mode.JSON)")
    
    # API parameters - Optional, no defaults if not specified
    max_tokens: Optional[int] = Field(default=None, description="Maximum tokens per request")
    temperature: Optional[float] = Field(default=None, description="Temperature for response generation")
    top_p: Optional[float] = Field(default=None, description="Top-p sampling parameter")
    timeout_seconds: Optional[int] = Field(default=None, description="Request timeout in seconds")
    max_retries_on_validation_error: int = Field(default=1, description="Max retries on Instructor validation errors")
    
    def get_model_api_parameters(self) -> dict:
        """Get only the API parameters that are explicitly set (not None)."""
        params = {}
        
        if self.max_tokens is not None:
            params["max_tokens"] = self.max_tokens
        if self.temperature is not None:
            params["temperature"] = self.temperature
        if self.top_p is not None:
            params["top_p"] = self.top_p
        if self.timeout_seconds is not None:
            params["timeout"] = self.timeout_seconds
        
        # Always include max_retries as it has a default value
        params["max_retries"] = self.max_retries_on_validation_error
            
        return params
    
    def get_active_parameters_summary(self) -> str:
        """Get a summary of which AI parameters are active vs using defaults."""
        active_params = self.get_model_api_parameters()
        summary_lines = []
        
        summary_lines.append("AI Model Parameters:")
        summary_lines.append(f"  max_tokens: {'✓ ' + str(self.max_tokens) if self.max_tokens is not None else '○ using model default'}")
        summary_lines.append(f"  temperature: {'✓ ' + str(self.temperature) if self.temperature is not None else '○ using model default'}")
        summary_lines.append(f"  top_p: {'✓ ' + str(self.top_p) if self.top_p is not None else '○ using model default'}")
        summary_lines.append(f"  timeout: {'✓ ' + str(self.timeout_seconds) + 's' if self.timeout_seconds is not None else '○ using client default'}")
        summary_lines.append(f"  max_retries: ✓ {self.max_retries_on_validation_error}")
        
        return "\n".join(summary_lines)


class ApplicationSettings(BaseModel):
    """Application-specific settings."""
    
    cobol_source_folder: Optional[str] = Field(default=None, description="COBOL source directory")
    java_output_folder: Optional[str] = Field(default=None, description="Java output directory")
    test_output_folder: Optional[str] = Field(default="./test-output", description="Test output directory")
    logs_folder: str = Field(default="./logs", description="Logs directory")
    
    # Processing options
    max_files_per_batch: int = Field(default=10, description="Maximum files to process in a batch")
    enable_parallel_processing: bool = Field(default=True, description="Enable parallel file processing")
    backup_original_files: bool = Field(default=True, description="Create backups of original files")


class MemorySettings(BaseModel):
    """Memory and conversation settings."""
    
    max_messages: int = Field(default=50, description="Maximum messages to keep in memory")
    enable_conversation_logging: bool = Field(default=True, description="Enable conversation logging")
    auto_save_conversations: bool = Field(default=True, description="Auto-save conversation logs")


class LoggingSettings(BaseModel):
    """Logging configuration settings."""
    
    level: str = Field(default="INFO", description="Logging level")
    format: str = Field(default="%(asctime)s - %(name)s - %(levelname)s - %(message)s", description="Log format")
    enable_file_logging: bool = Field(default=True, description="Enable file logging")
    enable_console_logging: bool = Field(default=True, description="Enable console logging")
    log_api_calls: bool = Field(default=True, description="Log API calls for debugging")
    # When enabled, dump full raw LLM inputs/outputs to the log file
    show_raw_input_to_llm: bool = Field(default=False, description="Log full prompts (messages, tools, params) sent to the LLM")
    show_raw_output_from_llm: bool = Field(default=False, description="Log full raw responses returned by the LLM")


class Settings(BaseModel):
    """Main settings configuration."""
    
    ai_settings: AISettings = Field(default_factory=AISettings)
    application_settings: ApplicationSettings = Field(default_factory=ApplicationSettings)
    memory_settings: MemorySettings = Field(default_factory=MemorySettings)
    logging_settings: LoggingSettings = Field(default_factory=LoggingSettings)
    
    @classmethod
    def from_env(cls, env_file: Optional[str] = None) -> "Settings":
        """Load settings from environment variables and optional .env file."""
        if env_file:
            load_dotenv(env_file)
        else:
            # Try to load from standard locations
            config_dir = Path("config")
            for env_file_name in ["settings.local.env", "settings.env"]:
                env_path = config_dir / env_file_name
                if env_path.exists():
                    load_dotenv(env_path)
                    break
        
        # Create settings with environment variable overrides
        ai_settings = AISettings(
            service_type=os.getenv("AI_SERVICE_TYPE", "AzureOpenAI"),
            endpoint=os.getenv("AZURE_OPENAI_ENDPOINT"),
            api_key=os.getenv("AZURE_OPENAI_API_KEY"),
            api_version=os.getenv("AZURE_OPENAI_API_VERSION", "2024-02-01"),
            deployment_name=os.getenv("AZURE_OPENAI_DEPLOYMENT_NAME"),
            model_id=os.getenv("AZURE_OPENAI_MODEL_ID", "gpt-4"),
            cobol_analyzer_model_id=os.getenv("AZURE_OPENAI_COBOL_ANALYZER_MODEL"),
            java_converter_model_id=os.getenv("AZURE_OPENAI_JAVA_CONVERTER_MODEL"),
            dependency_mapper_model_id=os.getenv("AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL"),
            unit_test_model_id=os.getenv("AZURE_OPENAI_UNIT_TEST_MODEL"),
            instructor_mode=os.getenv("INSTRUCTOR_MODE"),
            # Only set AI parameters if they are explicitly provided in environment
            max_tokens=int(os.getenv("AI_MAX_TOKENS")) if os.getenv("AI_MAX_TOKENS") else None,
            temperature=float(os.getenv("AI_TEMPERATURE")) if os.getenv("AI_TEMPERATURE") else None,
            top_p=float(os.getenv("AI_TOP_P")) if os.getenv("AI_TOP_P") else None,
            timeout_seconds=int(os.getenv("AI_TIMEOUT_SECONDS")) if os.getenv("AI_TIMEOUT_SECONDS") else None,
            max_retries_on_validation_error=int(os.getenv("INSTRUCTOR_MAX_RETRIES_ON_VALIDATION_ERROR", "1"))
        )
        
        application_settings = ApplicationSettings(
            cobol_source_folder=os.getenv("COBOL_SOURCE_FOLDER"),
            java_output_folder=os.getenv("JAVA_OUTPUT_FOLDER"),
            test_output_folder=os.getenv("TEST_OUTPUT_FOLDER", "./test-output"),
            logs_folder=os.getenv("LOGS_FOLDER", "./logs"),
            max_files_per_batch=int(os.getenv("MAX_FILES_PER_BATCH", "10")),
            enable_parallel_processing=os.getenv("ENABLE_PARALLEL_PROCESSING", "true").lower() == "true",
            backup_original_files=os.getenv("BACKUP_ORIGINAL_FILES", "true").lower() == "true"
        )
        
        memory_settings = MemorySettings(
            max_messages=int(os.getenv("MEMORY_MAX_MESSAGES", "50")),
            enable_conversation_logging=os.getenv("ENABLE_CONVERSATION_LOGGING", "true").lower() == "true",
            auto_save_conversations=os.getenv("AUTO_SAVE_CONVERSATIONS", "true").lower() == "true"
        )
        
        logging_settings = LoggingSettings(
            level=os.getenv("LOGGING_LEVEL", "INFO"),
            format=os.getenv("LOGGING_FORMAT", "%(asctime)s - %(name)s - %(levelname)s - %(message)s"),
            enable_file_logging=os.getenv("ENABLE_FILE_LOGGING", "true").lower() == "true",
            enable_console_logging=os.getenv("ENABLE_CONSOLE_LOGGING", "true").lower() == "true",
            log_api_calls=os.getenv("LOG_API_CALLS", "true").lower() == "true",
            show_raw_input_to_llm=os.getenv("SHOW_RAW_INPUT_TO_LLM", "false").lower() == "true",
            show_raw_output_from_llm=os.getenv("SHOW_RAW_OUTPUT_FROM_LLM", "false").lower() == "true"
        )
        
        return cls(
            ai_settings=ai_settings,
            application_settings=application_settings,
            memory_settings=memory_settings,
            logging_settings=logging_settings
        )
    
    def validate_required_settings(self) -> list[str]:
        """Validate that all required settings are present."""
        missing = []
        
        if not self.ai_settings.api_key:
            missing.append("AZURE_OPENAI_API_KEY")
        
        # For Azure OpenAI, endpoint and deployment are required
        if self.ai_settings.service_type.lower() == "azureopenai":
            if not self.ai_settings.endpoint:
                missing.append("AZURE_OPENAI_ENDPOINT")
            if not self.ai_settings.deployment_name:
                missing.append("AZURE_OPENAI_DEPLOYMENT_NAME")
        
        # For OpenAI, only API key is required
        
        return missing