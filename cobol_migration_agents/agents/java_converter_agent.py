"""Java Converter Agent using atomic-agents framework."""

from openai import AsyncAzureOpenAI, AsyncOpenAI
from typing import Optional
from atomic_agents.context.chat_history import ChatHistory
from atomic_agents.context.system_prompt_generator import SystemPromptGenerator
from atomic_agents.agents.atomic_agent import AtomicAgent, AgentConfig

from ..config.settings import Settings
from ..config.instructor_config import create_instructor_client
from ..models.migration_schemas import ConversionInputSchema, ConversionOutputSchema
from ..models.cobol_models import CobolFile, CobolAnalysis
from ..models.java_models import JavaFile
from ..services.logging_service import LoggingService


class JavaConverterAgent:
    """
    Specialized AI agent for converting COBOL code to Java Quarkus microservices.
    
    This agent uses atomic-agents framework to provide consistent, schema-driven
    conversion of COBOL programs to modern Java code with Quarkus framework patterns.
    """
    
    def __init__(self, settings: Settings, logging_service: Optional[LoggingService] = None):
        """Initialize the Java Converter Agent."""
        self.settings = settings
        self.agent: Optional[AtomicAgent] = None
        self.logging_service = logging_service
        self.instructor_client = None
        
    async def initialize(self) -> None:
        """Initialize the atomic agent with system prompt and configuration."""
        # Create memory for conversation tracking
        memory = ChatHistory(max_messages=self.settings.memory_settings.max_messages)
        
        # Create system prompt generator with Java/Quarkus expertise
        system_prompt_generator = SystemPromptGenerator(
            background=[
                "You are an expert Java developer and COBOL-to-Java migration specialist with extensive experience in Quarkus microservices.",
                "You have deep knowledge of Java best practices, Quarkus framework, JPA, CDI, RESTEasy, and modern microservice patterns.",
                "Your role is to convert COBOL business logic into clean, maintainable Java code that follows modern enterprise patterns.",
                "You understand both legacy COBOL patterns and modern Java/Quarkus equivalents, ensuring accurate business logic preservation."
            ],
            steps=[
                "1. Analyze the COBOL file structure and the provided analysis results",
                "2. Identify the primary business function and determine appropriate Java class structure",
                "3. Map COBOL data items to Java POJOs, DTOs, or JPA entities as appropriate",
                "4. Convert COBOL PROCEDURE DIVISION logic to Java methods with proper error handling",
                "5. Apply appropriate Quarkus annotations (@ApplicationScoped, @Entity, @Path, etc.)",
                "6. Implement proper separation of concerns (service layer, repository layer, REST endpoints)",
                "7. Add appropriate imports, package declarations, and dependency configurations",
                "8. Include comprehensive JavaDoc and inline comments for complex business logic",
                "9. Generate accompanying unit test recommendations and TODO items for manual review"
            ],
            output_instructions=[
                "CRITICAL: Return properly structured JSON with all required fields:",
                "- methods: Array of objects with name, visibility, return_type, parameters, body",
                "- Each method MUST include a complete 'body' field with actual Java code (e.g., '{ return value; }')",
                "- parameters: Array of STRINGS (e.g., ['String input', 'int count'], NOT objects)",
                "- Generate complete, compilable Java code with proper package structure",
                "- Use appropriate Quarkus annotations and patterns for the identified use case",
                "- Preserve all business logic from the original COBOL program",
                "- Follow Java naming conventions and coding standards",
                "- ALWAYS generate complete getters and setters for ALL fields with proper method bodies",
                "- Ensure all generated Java code compiles without errors",
                "- Provide accurate conversion confidence score and detailed notes"
            ]
        )
        
        # Setup AI client based on configuration (async clients)
        if self.settings.ai_settings.service_type.lower() == "azureopenai":
            openai_client = AsyncAzureOpenAI(
                api_key=self.settings.ai_settings.api_key,
                api_version=self.settings.ai_settings.api_version,
                azure_endpoint=self.settings.ai_settings.endpoint
            )
            model = self.settings.ai_settings.deployment_name
        else:
            # For OpenAI, support custom base_url if endpoint is provided (for proxies, self-hosted, etc.)
            if self.settings.ai_settings.endpoint:
                openai_client = AsyncOpenAI(
                    api_key=self.settings.ai_settings.api_key,
                    base_url=self.settings.ai_settings.endpoint
                )
            else:
                openai_client = AsyncOpenAI(api_key=self.settings.ai_settings.api_key)
            model = self.settings.ai_settings.model_id
        
        # Create instructor client with dynamic mode configuration
        client = create_instructor_client(openai_client, self.settings.ai_settings.instructor_mode)
        
        # Store client reference for hook setup
        self.instructor_client = client
        
        # Setup Instructor hooks for automatic token tracking if logging service is available
        if self.logging_service:
            self.logging_service.setup_instructor_hooks(client)
            self.logging_service.set_current_agent("JavaConverterAgent")
        
        # Use specialized model if configured
        if self.settings.ai_settings.java_converter_model_id:
            model = self.settings.ai_settings.java_converter_model_id
        
        # Create agent configuration
        config = AgentConfig(
            client=client,
            model=model,
            memory=memory,
            system_prompt_generator=system_prompt_generator,
            model_api_parameters={
                "max_tokens": self.settings.ai_settings.max_tokens,
                "temperature": self.settings.ai_settings.temperature,
                "top_p": self.settings.ai_settings.top_p
            }
        )
        
        # Initialize the base agent with type parameters
        self.agent = AtomicAgent[ConversionInputSchema, ConversionOutputSchema](config=config)
        
        # Add initial context to memory
        initial_context = ConversionOutputSchema(
            java_file=JavaFile(
                file_name="Initialization.java",
                package_name="com.example.initialization",
                class_name="Initialization",
                content="// Java Converter Agent initialized",
                original_cobol_file_name="initialization"
            ),
            conversion_confidence=1.0,
            conversion_notes=["Java Converter Agent initialized and ready for COBOL to Java conversion"],
            manual_review_items=[],
            test_recommendations=["Ensure proper unit test coverage for all converted business logic"]
        )
        memory.add_message("assistant", initial_context)
    
    async def convert_to_java(
        self, 
        cobol_file: CobolFile, 
        cobol_analysis: CobolAnalysis
    ) -> JavaFile:
        """
        Convert a COBOL file to Java Quarkus code.
        
        Args:
            cobol_file: The COBOL file to convert
            cobol_analysis: Analysis results for the COBOL file
            
        Returns:
            Generated Java file with Quarkus patterns
        """
        if not self.agent:
            await self.initialize()
        
        # Create conversion input
        conversion_input = ConversionInputSchema(
            cobol_file=cobol_file,
            cobol_analysis=cobol_analysis,
            target_framework="quarkus",
            conversion_style="microservice",
            include_tests=True
        )
        
        # Run conversion through the agent
        try:
            response = await self.agent.run_async(conversion_input)
            
            # Extract the Java file from the typed response
            return response.java_file
                
        except Exception as e:
            # Return basic Java file on failure
            class_name = self._derive_class_name(cobol_file.file_name)
            package_name = "com.example.converted"
            
            return JavaFile(
                file_name=f"{class_name}.java",
                package_name=package_name,
                class_name=class_name,
                content=self._generate_fallback_java_content(class_name, package_name, str(e)),
                original_cobol_file_name=cobol_file.file_name,
                conversion_notes=[f"Conversion failed: {str(e)}"],
                manual_review_required=True,
                todo_items=["Complete conversion manually - automated conversion failed"],
                estimated_accuracy=0.0
            )
    
    async def batch_convert_files(
        self, 
        cobol_files: list[CobolFile], 
        cobol_analyses: list[CobolAnalysis]
    ) -> list[JavaFile]:
        """
        Convert multiple COBOL files to Java in batch.
        
        Args:
            cobol_files: List of COBOL files to convert
            cobol_analyses: Corresponding analysis results
            
        Returns:
            List of generated Java files
        """
        java_files = []
        
        for cobol_file, analysis in zip(cobol_files, cobol_analyses):
            java_file = await self.convert_to_java(cobol_file, analysis)
            java_files.append(java_file)
        
        return java_files
    
    def _derive_class_name(self, cobol_file_name: str) -> str:
        """Derive a Java class name from COBOL file name."""
        # Remove extension and convert to PascalCase
        base_name = cobol_file_name.replace('.cbl', '').replace('.cpy', '').replace('.cob', '')
        
        # Convert to PascalCase
        parts = base_name.replace('-', '_').split('_')
        class_name = ''.join(word.capitalize() for word in parts if word)
        
        # Ensure it starts with a capital letter and is valid Java identifier
        if not class_name:
            class_name = "ConvertedProgram"
        elif not class_name[0].isupper():
            class_name = class_name.capitalize()
        
        return class_name
    
    def _generate_fallback_java_content(
        self, 
        class_name: str, 
        package_name: str, 
        error_msg: str
    ) -> str:
        """Generate fallback Java content when conversion fails."""
        return f"""package {package_name};

import jakarta.enterprise.context.ApplicationScoped;

/**
 * Converted COBOL program - CONVERSION FAILED
 * 
 * Error: {error_msg}
 * 
 * TODO: Complete conversion manually
 */
@ApplicationScoped
public class {class_name} {{
    
    /**
     * Placeholder method - implement business logic from original COBOL program
     * 
     * TODO: Analyze original COBOL file and implement corresponding business logic
     */
    public void processBusinessLogic() {{
        throw new UnsupportedOperationException("Conversion failed - manual implementation required");
    }}
}}"""
    
    def get_agent_memory(self) -> ChatHistory:
        """Get the agent's conversation memory."""
        if self.agent:
            return self.agent.memory
        return ChatHistory()
    
    def reset_agent_memory(self) -> None:
        """Reset the agent's conversation memory."""
        if self.agent:
            self.agent.reset_memory()
    
    async def get_conversion_statistics(self) -> dict:
        """Get statistics about conversions performed by this agent."""
        if not self.agent or not self.agent.memory:
            return {"total_conversions": 0, "successful_conversions": 0}
        
        messages = self.agent.memory.get_history()
        conversions = [msg for msg in messages if msg.get("role") == "assistant"]
        
        return {
            "total_conversions": len(conversions),
            "successful_conversions": len([c for c in conversions if "error" not in str(c.get("content", ""))]),
            "memory_usage": len(messages)
        }