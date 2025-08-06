"""COBOL Analyzer Agent using atomic-agents framework."""

import instructor
import openai
from openai import AsyncAzureOpenAI, AsyncOpenAI
from typing import Optional
from atomic_agents.context.chat_history import ChatHistory
from atomic_agents.context.system_prompt_generator import SystemPromptGenerator
from atomic_agents.agents.atomic_agent import AtomicAgent, AgentConfig

from ..config.settings import Settings
from ..models.migration_schemas import AnalysisInputSchema, AnalysisOutputSchema
from ..models.cobol_models import CobolFile, CobolAnalysis
from ..services.logging_service import LoggingService


class CobolAnalyzerAgent:
    """
    Specialized AI agent for analyzing COBOL code structure and patterns.
    
    This agent uses atomic-agents framework to provide consistent, schema-driven
    analysis of COBOL programs and copybooks with comprehensive business logic
    and complexity analysis.
    """
    
    def __init__(self, settings: Settings, logging_service: Optional[LoggingService] = None):
        """Initialize the COBOL Analyzer Agent."""
        self.settings = settings
        self.agent: Optional[AtomicAgent] = None
        self.logging_service = logging_service
        self.instructor_client = None
        
    async def initialize(self) -> None:
        """Initialize the atomic agent with system prompt and configuration."""
        # Create memory for conversation tracking
        memory = ChatHistory(max_messages=self.settings.memory_settings.max_messages)
        
        # Create system prompt generator with COBOL analysis expertise
        system_prompt_generator = SystemPromptGenerator(
            background=[
                "You are an expert COBOL code analyzer with decades of experience in legacy system modernization.",
                "You have deep knowledge of COBOL syntax, COBOL-85 standards, copybooks, and business logic patterns.",
                "Your role is to analyze COBOL programs and copybooks to understand their structure, complexity, and business purpose.",
                "You provide detailed technical analysis that helps in accurate migration to modern Java frameworks."
            ],
            steps=[
                "1. Parse the COBOL file structure (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions)",
                "2. Identify and catalog all data items, their levels, PICTURE clauses, and usage patterns",
                "3. Analyze PROCEDURE DIVISION for paragraphs, sections, and control flow",
                "4. Examine COPY statements and external dependencies",
                "5. Identify business logic patterns, validation rules, and data transformations",
                "6. Calculate complexity metrics including cyclomatic complexity and nesting depth",
                "7. Assess migration complexity and identify potential conversion challenges",
                "8. Generate comprehensive analysis with migration recommendations"
            ],
            output_instructions=[
                "CRITICAL: Return properly structured JSON with all required fields:",
                "- sections: Array of objects with name, type, content, line_start, line_end",
                "- paragraphs: Array of objects with name, content, line_start, line_end, calls",
                "- Each section must be an OBJECT, not a string (e.g., {'name': 'IDENTIFICATION DIVISION', 'type': 'DIVISION', 'content': '...', 'line_start': 1, 'line_end': 4})",
                "- Each paragraph must be an OBJECT with all fields including 'content' and line numbers",
                "- Provide complete structural analysis with all divisions, sections, and data items",
                "- Include detailed complexity metrics and business logic identification",
                "- Rate migration complexity as 'low', 'medium', or 'high' with clear justification",
                "- Ensure all analysis is technically accurate and migration-focused"
            ]
        )
        
        # Setup AI client based on configuration (async clients)
        if self.settings.ai_settings.service_type.lower() == "azureopenai":
            client = instructor.from_openai(
                AsyncAzureOpenAI(
                    api_key=self.settings.ai_settings.api_key,
                    api_version=self.settings.ai_settings.api_version,
                    azure_endpoint=self.settings.ai_settings.endpoint
                )
            )
            model = self.settings.ai_settings.deployment_name
        else:
            client = instructor.from_openai(
                AsyncOpenAI(api_key=self.settings.ai_settings.api_key)
            )
            model = self.settings.ai_settings.model_id
        
        # Store client reference for hook setup
        self.instructor_client = client
        
        # Setup Instructor hooks for automatic token tracking if logging service is available
        if self.logging_service:
            self.logging_service.setup_instructor_hooks(client)
            self.logging_service.set_current_agent("CobolAnalyzerAgent")
        
        # Use specialized model if configured
        if self.settings.ai_settings.cobol_analyzer_model_id:
            model = self.settings.ai_settings.cobol_analyzer_model_id
        
        # Create agent configuration
        config = AgentConfig(
            client=client,
            model=model,
            memory=memory,
            system_prompt_generator=system_prompt_generator,
            model_api_parameters={
                "max_tokens": self.settings.ai_settings.max_tokens,
                "temperature": self.settings.ai_settings.temperature
            }
        )
        
        # Initialize the base agent with type parameters
        self.agent = AtomicAgent[AnalysisInputSchema, AnalysisOutputSchema](config=config)
        
        # Add initial context to memory
        initial_context = AnalysisOutputSchema(
            analysis=CobolAnalysis(
                file_name="initialization",
                analysis_metadata={"status": "agent_initialized"}
            ),
            confidence_score=1.0,
            analysis_notes=["COBOL Analyzer Agent initialized and ready for file analysis"],
            recommendations=["Provide COBOL files for comprehensive structural and business logic analysis"]
        )
        memory.add_message("assistant", initial_context)
    
    async def analyze_cobol_file(self, cobol_file: CobolFile) -> CobolAnalysis:
        """
        Analyze a COBOL file and return comprehensive analysis results.
        
        Args:
            cobol_file: The COBOL file to analyze
            
        Returns:
            Detailed analysis results including structure, complexity, and business logic
        """
        if not self.agent:
            await self.initialize()
        
        # Create analysis input
        analysis_input = AnalysisInputSchema(
            cobol_file=cobol_file,
            analysis_depth="comprehensive",
            focus_areas=["structure", "complexity", "business_logic", "dependencies"]
        )
        
        # Run analysis through the agent
        try:
            response = await self.agent.run_async(analysis_input)
            
            # Extract the analysis from the typed response
            return response.analysis
                
        except Exception as e:
            # Return basic analysis on failure
            return CobolAnalysis(
                file_name=cobol_file.file_name,
                migration_complexity="high",
                migration_notes=[f"Analysis failed: {str(e)}"],
                potential_issues=["Automated analysis failed - manual review required"],
                analysis_metadata={"error": str(e), "status": "failed"}
            )
    
    async def batch_analyze_files(self, cobol_files: list[CobolFile]) -> list[CobolAnalysis]:
        """
        Analyze multiple COBOL files in batch.
        
        Args:
            cobol_files: List of COBOL files to analyze
            
        Returns:
            List of analysis results
        """
        analyses = []
        
        for cobol_file in cobol_files:
            analysis = await self.analyze_cobol_file(cobol_file)
            analyses.append(analysis)
        
        return analyses
    
    def get_agent_memory(self) -> ChatHistory:
        """Get the agent's conversation memory."""
        if self.agent:
            return self.agent.memory
        return ChatHistory()
    
    def reset_agent_memory(self) -> None:
        """Reset the agent's conversation memory."""
        if self.agent:
            self.agent.reset_memory()
    
    async def get_analysis_statistics(self) -> dict:
        """Get statistics about analyses performed by this agent."""
        if not self.agent or not self.agent.memory:
            return {"total_analyses": 0, "successful_analyses": 0}
        
        messages = self.agent.memory.get_history()
        analyses = [msg for msg in messages if msg.get("role") == "assistant"]
        
        return {
            "total_analyses": len(analyses),
            "successful_analyses": len([a for a in analyses if "error" not in str(a.get("content", ""))]),
            "memory_usage": len(messages)
        }