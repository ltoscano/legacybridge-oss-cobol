"""Tests for configuration management."""

import pytest
import os
from pathlib import Path
from cobol_migration_agents.config.settings import Settings, AISettings, ApplicationSettings


def test_settings_creation():
    """Test basic settings creation."""
    settings = Settings()
    assert settings.ai_settings.service_type == "AzureOpenAI"
    assert settings.ai_settings.model_id == "gpt-4"
    assert settings.application_settings.logs_folder == "./logs"


def test_ai_settings_validation():
    """Test AI settings validation."""
    ai_settings = AISettings(
        service_type="AzureOpenAI",
        endpoint="https://test.openai.azure.com/",
        api_key="test-key",
        deployment_name="gpt-4",
        model_id="gpt-4"
    )
    
    assert ai_settings.service_type == "AzureOpenAI"
    assert ai_settings.endpoint == "https://test.openai.azure.com/"


def test_application_settings():
    """Test application settings."""
    app_settings = ApplicationSettings(
        cobol_source_folder="./test-cobol",
        java_output_folder="./test-java",
        max_files_per_batch=5
    )
    
    assert app_settings.cobol_source_folder == "./test-cobol"
    assert app_settings.max_files_per_batch == 5


def test_settings_validation():
    """Test settings validation."""
    settings = Settings()
    
    # Should have missing required settings
    missing = settings.validate_required_settings()
    assert "AZURE_OPENAI_API_KEY" in missing
    assert "AZURE_OPENAI_ENDPOINT" in missing


def test_settings_from_env():
    """Test loading settings from environment variables."""
    # Set test environment variables
    test_env = {
        "AZURE_OPENAI_ENDPOINT": "https://test.openai.azure.com/",
        "AZURE_OPENAI_API_KEY": "test-key-123",
        "AZURE_OPENAI_DEPLOYMENT_NAME": "gpt-4",
        "AZURE_OPENAI_MODEL_ID": "gpt-4",
        "COBOL_SOURCE_FOLDER": "./test-cobol",
        "JAVA_OUTPUT_FOLDER": "./test-java"
    }
    
    # Temporarily set environment variables
    original_env = {}
    for key, value in test_env.items():
        original_env[key] = os.environ.get(key)
        os.environ[key] = value
    
    try:
        settings = Settings.from_env()
        
        assert settings.ai_settings.endpoint == "https://test.openai.azure.com/"
        assert settings.ai_settings.api_key == "test-key-123"
        assert settings.application_settings.cobol_source_folder == "./test-cobol"
        
        # Should have no missing settings
        missing = settings.validate_required_settings()
        assert len(missing) == 0
        
    finally:
        # Restore original environment
        for key, value in original_env.items():
            if value is None:
                if key in os.environ:
                    del os.environ[key]
            else:
                os.environ[key] = value


if __name__ == "__main__":
    pytest.main([__file__])