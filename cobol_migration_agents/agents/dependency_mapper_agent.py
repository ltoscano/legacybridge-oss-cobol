"""Dependency Mapper Agent using atomic-agents framework."""

import instructor
import openai
from openai import AsyncAzureOpenAI, AsyncOpenAI
from typing import Optional, List
from atomic_agents.context.chat_history import ChatHistory
from atomic_agents.context.system_prompt_generator import SystemPromptGenerator
from atomic_agents.agents.atomic_agent import AtomicAgent, AgentConfig

from ..config.settings import Settings
from ..config.instructor_config import create_instructor_client
from ..models.migration_schemas import DependencyInputSchema, DependencyOutputSchema
from ..models.cobol_models import CobolFile, CobolAnalysis
from ..models.dependency_models import DependencyMap, DependencyMapMetrics, DependencyRelation
from ..services.logging_service import LoggingService


class DependencyMapperAgent:
    """
    Specialized AI agent for mapping COBOL dependencies and relationships.
    
    This agent uses atomic-agents framework to provide consistent, schema-driven
    analysis of COBOL program dependencies, copybook usage, and call relationships
    with visualization diagram generation.
    """
    
    def __init__(self, settings: Settings, logging_service: Optional[LoggingService] = None):
        """Initialize the Dependency Mapper Agent."""
        self.settings = settings
        self.agent: Optional[AtomicAgent] = None
        self.logging_service = logging_service
        self.instructor_client = None
        
    async def initialize(self) -> None:
        """Initialize the atomic agent with system prompt and configuration."""
        # Create memory for conversation tracking
        memory = ChatHistory(max_messages=self.settings.memory_settings.max_messages)
        
        # Create system prompt generator with dependency analysis expertise
        system_prompt_generator = SystemPromptGenerator(
            background=[
                "You are an expert COBOL dependency analyst with deep knowledge of legacy system architecture patterns.",
                "You specialize in analyzing COBOL program relationships, copybook usage, CALL statements, and data flow patterns.",
                "Your role is to create comprehensive dependency maps that help in understanding system architecture and migration planning.",
                "You have expertise in generating Mermaid diagrams and identifying circular dependencies and migration risks."
            ],
            steps=[
                "1. Scan all COBOL files for COPY statements, CALL statements, and program references",
                "2. Identify copybook usage patterns and which programs use which copybooks",
                "3. Map program-to-program call relationships and data flow",
                "4. Detect circular dependencies and potential migration bottlenecks",
                "5. Calculate dependency metrics and complexity scores",
                "6. Determine optimal migration order based on dependency analysis",
                "7. Generate Mermaid diagram visualization of the dependency structure",
                "8. Identify high-risk dependencies that may complicate migration",
                "9. Provide migration recommendations based on dependency patterns"
            ],
            output_instructions=[
                "Create complete dependency mapping with all relationships clearly identified",
                "Include detailed metrics about dependency complexity and migration risks",
                "Generate accurate Mermaid diagram syntax for dependency visualization",
                "Identify and flag all circular dependencies with severity assessment",
                "Provide clear migration order recommendations based on dependency hierarchy",
                "Include copybook usage statistics and most/least used copybooks",
                "Calculate dependency complexity scores to guide migration planning",
                "Highlight any unusual or high-risk dependency patterns that need attention"
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
            openai_client = AsyncOpenAI(api_key=self.settings.ai_settings.api_key)
            model = self.settings.ai_settings.model_id
        
        # Create instructor client with dynamic mode configuration
        client = create_instructor_client(openai_client, self.settings.ai_settings.instructor_mode)
        
        # Store client reference for hook setup
        self.instructor_client = client
        
        # Setup Instructor hooks for automatic token tracking if logging service is available
        if self.logging_service:
            self.logging_service.setup_instructor_hooks(client)
            self.logging_service.set_current_agent("DependencyMapperAgent")
        
        # Use specialized model if configured
        if self.settings.ai_settings.dependency_mapper_model_id:
            model = self.settings.ai_settings.dependency_mapper_model_id
        
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
        self.agent = AtomicAgent[DependencyInputSchema, DependencyOutputSchema](config=config)
        
        # Add initial context to memory
        initial_context = DependencyOutputSchema(
            dependency_map=DependencyMap(
                source_directory="initialization",
                metrics=DependencyMapMetrics(
                    total_files=0,
                    total_programs=0,
                    total_copybooks=0,
                    total_dependencies=0,
                    average_dependencies_per_program=0.0,
                    max_dependencies_for_single_program=0,
                    programs_with_no_dependencies=0
                ),
                mermaid_diagram="graph TD\n    A[Dependency Mapper Agent] --> B[Ready for Analysis]"
            ),
            mapping_confidence=1.0,
            mapping_notes=["Dependency Mapper Agent initialized and ready for COBOL dependency analysis"],
            migration_recommendations=["Provide COBOL files for comprehensive dependency mapping and migration planning"]
        )
        memory.add_message("assistant", initial_context)
    
    async def analyze_dependencies(
        self, 
        cobol_files: List[CobolFile],
        analysis_results: Optional[List[CobolAnalysis]] = None
    ) -> DependencyMap:
        """
        Analyze dependencies across all COBOL files.
        
        Args:
            cobol_files: List of COBOL files to analyze
            analysis_results: Optional analysis results for enhanced mapping
            
        Returns:
            Complete dependency mapping with metrics and visualization
        """
        if not self.agent:
            await self.initialize()
        
        # Create dependency analysis input
        dependency_input = DependencyInputSchema(
            cobol_files=cobol_files,
            analysis_results=analysis_results or [],
            include_visualization=True
        )
        
        # Run dependency analysis through the agent
        try:
            response = await self.agent.run_async(dependency_input)
            
            # Extract the dependency map from the typed response
            return response.dependency_map
                
        except Exception as e:
            # Return basic dependency map on failure
            return self._create_fallback_dependency_map(cobol_files, str(e))
    
    def _create_fallback_dependency_map(
        self, 
        cobol_files: List[CobolFile], 
        error_msg: str
    ) -> DependencyMap:
        """Create a fallback dependency map when analysis fails."""
        return DependencyMap(
            source_directory="analysis_failed",
            dependencies=[],
            reverse_dependencies={},
            copybook_usage=[],
            copybook_definitions={},
            program_dependencies={},
            program_calls={},
            metrics=DependencyMapMetrics(
                total_files=len(cobol_files),
                total_programs=len([f for f in cobol_files if f.file_name.endswith('.cbl')]),
                total_copybooks=len([f for f in cobol_files if f.file_name.endswith('.cpy')]),
                total_dependencies=0,
                average_dependencies_per_program=0.0,
                max_dependencies_for_single_program=0,
                programs_with_no_dependencies=len(cobol_files),
                migration_risk_level="high"
            ),
            mermaid_diagram=f"graph TD\n    Error[\"Analysis Failed: {error_msg}\"]",
            migration_order=[f.file_name for f in cobol_files],
            migration_groups=[[f.file_name for f in cobol_files]],
            high_risk_dependencies=[],
            analysis_metadata={"error": error_msg, "status": "failed"}
        )
    
    async def analyze_single_file_dependencies(
        self, 
        cobol_file: CobolFile,
        all_files: List[CobolFile]
    ) -> List[DependencyRelation]:
        """
        Analyze dependencies for a single COBOL file.
        
        Args:
            cobol_file: The COBOL file to analyze
            all_files: All available COBOL files for reference
            
        Returns:
            List of dependency relationships for the file
        """
        # This could be implemented as a simplified version of the full analysis
        # For now, we'll use basic parsing to identify COPY and CALL statements
        dependencies = []
        
        try:
            lines = cobol_file.content.split('\n')
            for line_num, line in enumerate(lines, 1):
                line_upper = line.upper().strip()
                
                # Look for COPY statements
                if 'COPY ' in line_upper:
                    parts = line_upper.split('COPY ')
                    if len(parts) > 1:
                        copybook_name = parts[1].split()[0].strip('.')
                        dependencies.append(DependencyRelation(
                            source_file=cobol_file.file_name,
                            target_file=f"{copybook_name}.cpy",
                            dependency_type="COPY",
                            line_number=line_num,
                            context=line.strip()
                        ))
                
                # Look for CALL statements
                if 'CALL ' in line_upper:
                    parts = line_upper.split('CALL ')
                    if len(parts) > 1:
                        called_program = parts[1].split()[0].strip('\'".')
                        dependencies.append(DependencyRelation(
                            source_file=cobol_file.file_name,
                            target_file=f"{called_program}.cbl",
                            dependency_type="CALL",
                            line_number=line_num,
                            context=line.strip()
                        ))
        
        except Exception as e:
            # Return empty list on parsing failure
            pass
        
        return dependencies
    
    def get_agent_memory(self) -> ChatHistory:
        """Get the agent's conversation memory."""
        if self.agent:
            return self.agent.memory
        return ChatHistory()
    
    def reset_agent_memory(self) -> None:
        """Reset the agent's conversation memory."""
        if self.agent:
            self.agent.reset_memory()
    
    async def get_mapping_statistics(self) -> dict:
        """Get statistics about dependency mappings performed by this agent."""
        if not self.agent or not self.agent.memory:
            return {"total_mappings": 0, "successful_mappings": 0}
        
        messages = self.agent.memory.get_history()
        mappings = [msg for msg in messages if msg.get("role") == "assistant"]
        
        return {
            "total_mappings": len(mappings),
            "successful_mappings": len([m for m in mappings if "error" not in str(m.get("content", ""))]),
            "memory_usage": len(messages)
        }