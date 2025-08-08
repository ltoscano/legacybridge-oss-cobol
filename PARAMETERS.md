# 📋 Configuration Parameters Reference

This document provides detailed explanations for all configuration parameters available in the COBOL Migration Agents system. These parameters are defined in `config/settings.env.example` and should be copied to `config/settings.local.env` for customization.

---

## 🤖 AI Service Configuration

### `AI_SERVICE_TYPE`
- **Type**: `string`
- **Default**: `AzureOpenAI`
- **Options**: `AzureOpenAI` | `OpenAI`
- **Description**: Determines which AI service provider to use for the migration agents.
- **Usage**: 
  - `AzureOpenAI`: Use Azure's OpenAI service (recommended for enterprise)
  - `OpenAI`: Use OpenAI's direct API service

### `AZURE_OPENAI_ENDPOINT`
- **Type**: `string` (URL)
- **Required**: Yes (for Azure OpenAI)
- **Example**: `https://your-resource.openai.azure.com/`
- **Description**: The endpoint URL for your Azure OpenAI resource.
- **Usage**: Found in your Azure Portal under your OpenAI resource's "Keys and Endpoint" section.

### `AZURE_OPENAI_API_KEY`
- **Type**: `string`
- **Required**: Yes
- **Description**: Authentication key for your AI service.
- **Usage**:
  - **Azure OpenAI**: Found in Azure Portal under your OpenAI resource
  - **OpenAI**: Your OpenAI API key starting with `sk-`
- **Security**: ⚠️ Keep this secret! Never commit to version control.

### `AZURE_OPENAI_API_VERSION`
- **Type**: `string` (date)
- **Default**: `2025-04-01-preview`
- **Required**: Yes (for Azure OpenAI)
- **Description**: Azure OpenAI API version to use.
- **Usage**: Determines available features and model capabilities. Use latest stable version for best results.

### `AZURE_OPENAI_DEPLOYMENT_NAME`
- **Type**: `string`
- **Required**: Yes (for Azure OpenAI)
- **Example**: `gpt-4.1`
- **Description**: Name of your model deployment in Azure OpenAI.
- **Usage**: Found in Azure Portal under your OpenAI resource's "Model deployments" section.

### `AZURE_OPENAI_MODEL_ID`
- **Type**: `string`
- **Default**: `gpt-4.1`
- **Description**: The model identifier to use for AI operations.
- **Usage**:
  - **Azure OpenAI**: Usually matches your deployment name
  - **OpenAI**: Model name like `gpt-4`, `gpt-4-turbo`, `gpt-3.5-turbo`

---

## 🔧 Specialized Models (Optional)

These parameters allow you to use different models for different types of analysis, enabling optimization for specific tasks.

### `AZURE_OPENAI_COBOL_ANALYZER_MODEL`
- **Type**: `string`
- **Optional**: Yes
- **Description**: Specific model for COBOL structure analysis tasks.
- **Usage**: Use a model optimized for code analysis if available. Defaults to main model if not set.

### `AZURE_OPENAI_JAVA_CONVERTER_MODEL`
- **Type**: `string`
- **Optional**: Yes
- **Description**: Specific model for COBOL-to-Java conversion tasks.
- **Usage**: Use a model with strong Java knowledge for better conversion quality.

### `AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL`
- **Type**: `string`
- **Optional**: Yes
- **Description**: Specific model for dependency analysis and relationship mapping.
- **Usage**: Use a model good at understanding complex relationships and generating diagrams.

### `AZURE_OPENAI_UNIT_TEST_MODEL`
- **Type**: `string`
- **Optional**: Yes
- **Description**: Specific model for unit test generation (future feature).
- **Usage**: Reserved for future unit test generation capabilities.

---

## 🎛️ Instructor Mode Configuration

### `INSTRUCTOR_MODE`
- **Type**: `string` (Python module path)
- **Default**: `instructor.Mode.OPENROUTER_STRUCTURED_OUTPUTS`
- **Optional**: Yes
- **Description**: Configures how the Instructor library interacts with the LLM for structured outputs.
- **Options**:
  - `instructor.Mode.JSON`: Maximum compatibility (Ollama, local models)
  - `instructor.Mode.OPENROUTER_STRUCTURED_OUTPUTS`: Optimized for OpenRouter
  - `instructor.Mode.TOOLS`: Function calling mode (OpenAI/Azure)
  - `instructor.Mode.PARALLEL_TOOLS`: Parallel function calling
- **Usage**: Choose based on your LLM provider's capabilities. See [Instructor documentation](https://python.useinstructor.com/concepts/patching/) for details.

---

## 📁 Application Configuration

### Directory Settings

#### `COBOL_SOURCE_FOLDER`
- **Type**: `string` (path)
- **Default**: `./cobol-source`
- **Description**: Directory containing input COBOL files (.cbl, .cpy).
- **Usage**: Relative or absolute path to your COBOL source files.

#### `JAVA_OUTPUT_FOLDER`
- **Type**: `string` (path)
- **Default**: `./java-output`
- **Description**: Directory where generated Java files will be written.
- **Usage**: Must be writable. Will be created if it doesn't exist.

#### `TEST_OUTPUT_FOLDER`
- **Type**: `string` (path)
- **Default**: `./test-output`
- **Description**: Directory for generated test files (future feature).
- **Usage**: Reserved for future test generation capabilities.

#### `LOGS_FOLDER`
- **Type**: `string` (path)
- **Default**: `./logs`
- **Description**: Directory where system logs and conversation logs are stored.
- **Usage**: Must be writable. Contains debug information and AI conversation traces.

---

## ⚙️ Processing Options

### `MAX_FILES_PER_BATCH`
- **Type**: `integer`
- **Default**: `10`
- **Range**: `1-100`
- **Description**: Maximum number of files to process in a single batch operation.
- **Usage**: Tune based on memory constraints and API limits. Lower values use less memory but may be slower.

### `ENABLE_PARALLEL_PROCESSING`
- **Type**: `boolean`
- **Default**: `true`
- **Description**: Enables concurrent processing of multiple files.
- **Usage**: Set to `false` for debugging or if experiencing rate limits. `true` for better performance.

### `BACKUP_ORIGINAL_FILES`
- **Type**: `boolean`
- **Default**: `true`
- **Description**: Creates backup copies of original COBOL files before processing.
- **Usage**: Recommended for safety. Backups are stored with timestamp suffixes.

---

## 🧠 AI Model Parameters

### `AI_MAX_TOKENS`
- **Type**: `integer`
- **Default**: `4000`
- **Range**: `100-32000` (model dependent)
- **Description**: Maximum tokens per AI API request.
- **Usage**: Higher values allow more complex operations but cost more. Adjust based on your model's limits.

### `AI_TEMPERATURE`
- **Type**: `float`
- **Default**: `0.1`
- **Range**: `0.0-2.0`
- **Description**: Controls randomness in AI responses.
- **Usage**: 
  - `0.0-0.3`: More deterministic, better for code generation
  - `0.4-0.7`: Balanced creativity and consistency
  - `0.8-2.0`: More creative but less predictable

### `AI_TIMEOUT_SECONDS`
- **Type**: `integer`
- **Default**: `600` (10 minutes)
- **Range**: `30-3600`
- **Description**: Maximum time to wait for AI API responses.
- **Usage**: Increase for complex files or slow networks. Decrease to fail faster on issues.

---

## 💭 Memory and Conversation Settings

### `MEMORY_MAX_MESSAGES`
- **Type**: `integer`
- **Default**: `50`
- **Range**: `5-200`
- **Description**: Maximum conversation history messages kept in memory per agent.
- **Usage**: Higher values provide more context but use more memory. Lower values save memory but may lose context.

### `ENABLE_CONVERSATION_LOGGING`
- **Type**: `boolean`
- **Default**: `true`
- **Description**: Enables logging of AI agent conversations for debugging and audit.
- **Usage**: Useful for understanding AI decisions. Set to `false` to save disk space.

### `AUTO_SAVE_CONVERSATIONS`
- **Type**: `boolean`
- **Default**: `true`
- **Description**: Automatically saves conversation logs after migration completion.
- **Usage**: Recommended for audit trails. Conversations are saved to the logs folder.

---

## 📝 Logging Configuration

### `LOGGING_LEVEL`
- **Type**: `string`
- **Default**: `INFO`
- **Options**: `DEBUG` | `INFO` | `WARNING` | `ERROR` | `CRITICAL`
- **Description**: Controls the verbosity of system logs.
- **Usage**:
  - `DEBUG`: Detailed debugging information
  - `INFO`: General information about system operation
  - `WARNING`: Warning messages about potential issues
  - `ERROR`: Error messages only
  - `CRITICAL`: Critical errors only

### `LOGGING_FORMAT`
- **Type**: `string`
- **Default**: `%(asctime)s - %(name)s - %(levelname)s - %(message)s`
- **Description**: Python logging format string for log messages.
- **Usage**: Customize log message format. See [Python logging documentation](https://docs.python.org/3/library/logging.html#logrecord-attributes) for format options.

### `ENABLE_FILE_LOGGING`
- **Type**: `boolean`
- **Default**: `true`
- **Description**: Enables writing logs to files in the logs directory.
- **Usage**: Recommended for persistent debugging. Set to `false` to only log to console.

### `ENABLE_CONSOLE_LOGGING`
- **Type**: `boolean`
- **Default**: `true`
- **Description**: Enables displaying logs in the console/terminal.
- **Usage**: Useful for real-time monitoring. Can be disabled for headless operation.

### `LOG_API_CALLS`
- **Type**: `boolean`
- **Default**: `true`
- **Description**: Enables detailed logging of AI API calls including tokens and costs.
- **Usage**: Essential for monitoring usage and costs. Disable only if storage is a concern.

### `SHOW_RAW_INPUT_TO_LLM`
- **Type**: `boolean`
- **Default**: `false`
- **Description**: When `true`, logs the full raw prompt sent to the LLM including system prompt, messages, tools and key parameters.
- **Usage**: Useful for debugging prompt construction. May include sensitive data, enable only in secure environments. Large payloads are truncated (~50 KB).

### `SHOW_RAW_OUTPUT_FROM_LLM`
- **Type**: `boolean`
- **Default**: `false`
- **Description**: When `true`, logs the full raw response payload returned by the LLM.
- **Usage**: Helpful for troubleshooting provider responses. May include sensitive data, enable only in secure environments. Large payloads are truncated (~50 KB).

---

## 🔧 Configuration Best Practices

### Security
- ✅ **Never commit API keys** to version control
- ✅ **Use environment variables** in production
- ✅ **Restrict file permissions** on configuration files
- ✅ **Rotate API keys** regularly

### Performance
- 🚀 **Enable parallel processing** for better throughput
- 🚀 **Tune batch size** based on memory and API limits
- 🚀 **Use appropriate temperature** for your use case
- 🚀 **Monitor token usage** to optimize costs

### Debugging
- 🐛 **Enable conversation logging** for AI debugging
- 🐛 **Use DEBUG level** when troubleshooting
- 🐛 **Keep backups enabled** during testing
- 🐛 **Monitor log files** for errors and warnings

### Scaling
- 📈 **Use specialized models** for different tasks
- 📈 **Adjust timeout values** for complex operations
- 📈 **Monitor memory usage** with large batches
- 📈 **Consider API rate limits** when scaling

---

## 🔗 Related Documentation

- 📘 [Docker Quick Start](DOCKER_QUICKSTART.md)
- 📘 [Docker Guide](DOCKER_GUIDE.md)
- 🔗 [Instructor Documentation](https://python.useinstructor.com/)
- 🔗 [Azure OpenAI Documentation](https://docs.microsoft.com/en-us/azure/cognitive-services/openai/)
- 🔗 [OpenAI API Documentation](https://platform.openai.com/docs/)