"""Main migration orchestrator using atomic-agents framework."""

import asyncio
import logging
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Callable, Dict, Any

from rich.console import Console
from rich.progress import Progress, TaskID

from .config.settings import Settings
from .models.migration_schemas import (
    MigrationInputSchema,
    MigrationOutputSchema,
    ProgressUpdateSchema
)
from .agents.cobol_analyzer_agent import CobolAnalyzerAgent
from .agents.java_converter_agent import JavaConverterAgent  
from .agents.dependency_mapper_agent import DependencyMapperAgent
from .services.file_manager import FileManager
from .services.logging_service import LoggingService
from .services.report_generator import ReportGenerator


class MigrationOrchestrator:
    """
    Main orchestrator for COBOL to Java migration using atomic-agents.
    
    This class coordinates the entire migration process through specialized
    AI agents and provides comprehensive logging and progress tracking.
    """
    
    def __init__(
        self,
        settings: Settings,
        progress_callback: Optional[Callable[[ProgressUpdateSchema], None]] = None
    ):
        """
        Initialize the migration orchestrator.
        
        Args:
            settings: Application settings configuration
            progress_callback: Optional callback for progress updates
        """
        self.settings = settings
        self.progress_callback = progress_callback
        self.console = Console()
        
        # Initialize services
        self.file_manager = FileManager(settings)
        self.logging_service = LoggingService(settings)
        self.report_generator = ReportGenerator(settings)
        
        # Initialize agents
        self.cobol_analyzer_agent: Optional[CobolAnalyzerAgent] = None
        self.java_converter_agent: Optional[JavaConverterAgent] = None
        self.dependency_mapper_agent: Optional[DependencyMapperAgent] = None
        
        # Migration state
        self.migration_session_id: Optional[str] = None
        self.start_time: Optional[datetime] = None
        
        # Setup logging
        self.logger = logging.getLogger(__name__)
        
    async def initialize_agents(self) -> None:
        """Initialize all AI agents."""
        self.console.print("🤖 Initializing AI agents...")
        
        try:
            # Initialize COBOL Analyzer Agent with logging service
            self.cobol_analyzer_agent = CobolAnalyzerAgent(self.settings, self.logging_service)
            await self.cobol_analyzer_agent.initialize()
            
            # Initialize Java Converter Agent with logging service
            self.java_converter_agent = JavaConverterAgent(self.settings, self.logging_service)
            await self.java_converter_agent.initialize()
            
            # Initialize Dependency Mapper Agent with logging service
            self.dependency_mapper_agent = DependencyMapperAgent(self.settings, self.logging_service)
            await self.dependency_mapper_agent.initialize()
            
            self.console.print("✅ All agents initialized successfully")
            self.logger.info("AI agents initialized successfully")
            
        except Exception as e:
            self.console.print(f"❌ Failed to initialize agents: {e}")
            self.logger.error(f"Failed to initialize agents: {e}")
            raise
    
    async def run_migration(
        self, 
        migration_input: MigrationInputSchema
    ) -> MigrationOutputSchema:
        """
        Run the complete COBOL to Java migration process.
        
        Args:
            migration_input: Migration input parameters
            
        Returns:
            Migration results and output information
        """
        self.start_time = datetime.utcnow()
        self.migration_session_id = f"migration_{self.start_time.strftime('%Y%m%d_%H%M%S')}"
        
        # Start logging session
        self.logging_service.start_session(self.migration_session_id)
        
        self.console.print("🚀 Starting COBOL to Java Quarkus migration...")
        self.logger.info(f"Starting migration session: {self.migration_session_id}")
        
        try:
            # Validate input
            await self._validate_migration_input(migration_input)
            
            # Initialize agents if not already done
            if not self.cobol_analyzer_agent:
                await self.initialize_agents()
            
            # Step 1: File Discovery
            await self._update_progress("Discovering COBOL files", 1, 6)
            cobol_files = await self.file_manager.discover_cobol_files(
                migration_input.cobol_source_folder
            )
            
            if not cobol_files:
                return MigrationOutputSchema(
                    success=False,
                    total_files_processed=0,
                    java_files_generated=0,
                    migration_report_path="",
                    conversation_log_path="",
                    errors=[f"No COBOL files found in {migration_input.cobol_source_folder}"]
                )
            
            self.console.print(f"📁 Found {len(cobol_files)} COBOL files")
            
            # Step 2: Dependency Analysis
            await self._update_progress("Analyzing dependencies", 2, 6)
            start_time = datetime.utcnow()
            
            # Log conversation message for dependency analysis start
            await self.logging_service.log_conversation_message(
                role="user",
                agent_name="DependencyMapperAgent",
                content=f"Analyze dependencies for {len(cobol_files)} COBOL files",
                tokens=len(cobol_files) * 30,  # Estimated input tokens
                metadata={
                    "operation": "dependency_analysis_start",
                    "files_count": len(cobol_files),
                    "file_names": [f.file_name for f in cobol_files]
                }
            )
            
            dependency_map = await self.dependency_mapper_agent.analyze_dependencies(
                cobol_files
            )
            
            end_time = datetime.utcnow()
            duration = (end_time - start_time).total_seconds()
            estimated_tokens = len(cobol_files) * 80
            
            # Log API call for dependency analysis
            await self.logging_service.log_api_call(
                agent_name="DependencyMapperAgent",
                model=self.settings.ai_settings.model_id,
                prompt_tokens=estimated_tokens // 2,
                completion_tokens=estimated_tokens // 2,
                total_tokens=estimated_tokens,
                duration_seconds=duration,
                cost_estimate=self.logging_service._estimate_cost(
                    self.settings.ai_settings.model_id,
                    estimated_tokens // 2,
                    estimated_tokens // 2
                )
            )
            
            # Log conversation message for dependency analysis completion
            await self.logging_service.log_conversation_message(
                role="assistant",
                agent_name="DependencyMapperAgent",
                content=f"Completed dependency analysis: Found {dependency_map.metrics.total_dependencies if hasattr(dependency_map, 'metrics') else 0} dependencies",
                tokens=estimated_tokens // 2,
                metadata={
                    "operation": "dependency_analysis_complete",
                    "duration_seconds": duration,
                    "total_dependencies": dependency_map.metrics.total_dependencies if hasattr(dependency_map, 'metrics') else 0,
                    "migration_risk": dependency_map.metrics.migration_risk_level if hasattr(dependency_map, 'metrics') else "unknown"
                }
            )
            
            
            # Step 3: COBOL Analysis
            await self._update_progress("Analyzing COBOL structure", 3, 6)
            cobol_analyses = await self._analyze_cobol_files(cobol_files)
            
            # Step 4: Java Conversion
            await self._update_progress("Converting to Java", 4, 6)
            java_files = await self._convert_to_java(cobol_files, cobol_analyses)
            
            # Step 5: File Generation
            await self._update_progress("Generating output files", 5, 6)
            await self._save_java_files(java_files, migration_input.java_output_folder)
            
            # Step 6: Report Generation
            await self._update_progress("Generating reports", 6, 6)
            report_path = await self._generate_migration_report(
                cobol_files, java_files, dependency_map, migration_input.java_output_folder
            )
            
            # Generate conversation log
            conversation_log_path = await self._generate_conversation_log(
                migration_input.java_output_folder
            )
            
            # Final success message with AI usage statistics
            duration = (datetime.utcnow() - self.start_time).total_seconds()
            
            # Get final logging statistics
            final_stats = self.logging_service.get_session_statistics()
            
            self.console.print(f"✅ Migration completed successfully in {duration:.1f}s")
            self.console.print(f"🤖 AI Usage Summary:")
            self.console.print(f"   • Total API calls: {final_stats.get('api_calls', 0)}")
            self.console.print(f"   • Total tokens: {final_stats.get('total_tokens', 0):,}")
            self.console.print(f"   • Prompt tokens: {final_stats.get('total_prompt_tokens', 0):,}")
            self.console.print(f"   • Completion tokens: {final_stats.get('total_completion_tokens', 0):,}")
            self.console.print(f"   • Estimated cost: ${final_stats.get('total_cost', 0):.4f}")
            
            if final_stats.get('hooks_enabled', False):
                self.console.print(f"   • ✅ Instructor hooks enabled (automatic tracking)")
            else:
                self.console.print(f"   • ⚠️ Manual logging (hooks not enabled)")
            
            return MigrationOutputSchema(
                success=True,
                total_files_processed=len(cobol_files),
                java_files_generated=len(java_files),
                migration_report_path=report_path,
                conversation_log_path=conversation_log_path,
                warnings=[],
                errors=[]
            )
            
        except Exception as e:
            error_msg = f"Migration failed: {str(e)}"
            self.console.print(f"❌ {error_msg}")
            self.logger.error(error_msg, exc_info=True)
            
            return MigrationOutputSchema(
                success=False,
                total_files_processed=0,
                java_files_generated=0,
                migration_report_path="",
                conversation_log_path="",
                errors=[error_msg]
            )
    
    async def _validate_migration_input(self, migration_input: MigrationInputSchema) -> None:
        """Validate migration input parameters."""
        source_path = Path(migration_input.cobol_source_folder)
        if not source_path.exists():
            raise ValueError(f"COBOL source folder does not exist: {migration_input.cobol_source_folder}")
        
        output_path = Path(migration_input.java_output_folder)
        output_path.mkdir(parents=True, exist_ok=True)
    
    async def _analyze_cobol_files(self, cobol_files) -> List:
        """Analyze COBOL files using the analyzer agent."""
        analyses = []
        
        with Progress(console=self.console) as progress:
            task = progress.add_task("Analyzing COBOL files...", total=len(cobol_files))
            
            for i, cobol_file in enumerate(cobol_files):
                try:
                    start_time = datetime.utcnow()
                    estimated_input_tokens = len(cobol_file.content) // 4  # Rough estimate
                    
                    # Log conversation start for this file
                    await self.logging_service.log_conversation_message(
                        role="user",
                        agent_name="CobolAnalyzerAgent",
                        content=f"Analyze COBOL file: {cobol_file.file_name}",
                        tokens=estimated_input_tokens,
                        metadata={
                            "operation": "cobol_analysis_start",
                            "file_name": cobol_file.file_name,
                            "file_size": cobol_file.size_bytes,
                            "file_type": cobol_file.file_type,
                            "content_preview": cobol_file.content[:200] + "..." if len(cobol_file.content) > 200 else cobol_file.content
                        }
                    )
                    
                    analysis = await self.cobol_analyzer_agent.analyze_cobol_file(cobol_file)
                    analyses.append(analysis)
                    
                    end_time = datetime.utcnow()
                    duration = (end_time - start_time).total_seconds()
                    estimated_output_tokens = estimated_input_tokens // 3
                    total_tokens = estimated_input_tokens + estimated_output_tokens
                    
                    # Log API call for this file analysis
                    await self.logging_service.log_api_call(
                        agent_name="CobolAnalyzerAgent",
                        model=self.settings.ai_settings.model_id,
                        prompt_tokens=estimated_input_tokens,
                        completion_tokens=estimated_output_tokens,
                        total_tokens=total_tokens,
                        duration_seconds=duration,
                        cost_estimate=self.logging_service._estimate_cost(
                            self.settings.ai_settings.model_id,
                            estimated_input_tokens,
                            estimated_output_tokens
                        )
                    )
                    
                    # Log conversation completion for this file
                    await self.logging_service.log_conversation_message(
                        role="assistant",
                        agent_name="CobolAnalyzerAgent",
                        content=f"Analysis complete for {cobol_file.file_name}: {analysis.migration_complexity if hasattr(analysis, 'migration_complexity') else 'unknown'} complexity",
                        tokens=estimated_output_tokens,
                        metadata={
                            "operation": "cobol_analysis_complete",
                            "file_name": cobol_file.file_name,
                            "duration_seconds": duration,
                            "migration_complexity": analysis.migration_complexity if hasattr(analysis, 'migration_complexity') else "unknown",
                            "potential_issues_count": len(analysis.potential_issues) if hasattr(analysis, 'potential_issues') and analysis.potential_issues else 0,
                            "analysis_confidence": getattr(analysis, 'confidence_score', 0.8)
                        }
                    )
                    
                    progress.update(task, advance=1)
                    await self._update_progress(
                        f"Analyzing COBOL files ({i+1}/{len(cobol_files)})", 
                        3, 6,
                        current_file=cobol_file.file_name
                    )
                    
                except Exception as e:
                    self.logger.error(f"Failed to analyze {cobol_file.file_name}: {e}")
                    
                    # Log the error
                    await self.logging_service.log_conversation_message(
                        role="assistant",
                        agent_name="CobolAnalyzerAgent",
                        content=f"Failed to analyze {cobol_file.file_name}: {str(e)}",
                        tokens=0,
                        metadata={
                            "operation": "cobol_analysis_error",
                            "file_name": cobol_file.file_name,
                            "error": str(e)
                        }
                    )
                    # Continue with other files
        
        return analyses
    
    async def _convert_to_java(self, cobol_files, cobol_analyses) -> List:
        """Convert COBOL files to Java using the converter agent."""
        java_files = []
        
        with Progress(console=self.console) as progress:
            task = progress.add_task("Converting to Java...", total=len(cobol_files))
            
            for i, (cobol_file, analysis) in enumerate(zip(cobol_files, cobol_analyses)):
                try:
                    start_time = datetime.utcnow()
                    estimated_input_tokens = len(cobol_file.content) // 3  # More complex than analysis
                    
                    # Log conversation start for this conversion
                    await self.logging_service.log_conversation_message(
                        role="user",
                        agent_name="JavaConverterAgent",
                        content=f"Convert COBOL file to Java: {cobol_file.file_name}",
                        tokens=estimated_input_tokens,
                        metadata={
                            "operation": "java_conversion_start",
                            "file_name": cobol_file.file_name,
                            "file_size": cobol_file.size_bytes,
                            "file_type": cobol_file.file_type,
                            "migration_complexity": analysis.migration_complexity if hasattr(analysis, 'migration_complexity') else "unknown",
                            "content_preview": cobol_file.content[:200] + "..." if len(cobol_file.content) > 200 else cobol_file.content
                        }
                    )
                    
                    java_file = await self.java_converter_agent.convert_to_java(
                        cobol_file, analysis
                    )
                    java_files.append(java_file)
                    
                    end_time = datetime.utcnow()
                    duration = (end_time - start_time).total_seconds()
                    estimated_output_tokens = estimated_input_tokens // 2  # Conversion generates significant output
                    total_tokens = estimated_input_tokens + estimated_output_tokens
                    
                    # Log API call for this conversion
                    await self.logging_service.log_api_call(
                        agent_name="JavaConverterAgent",
                        model=self.settings.ai_settings.model_id,
                        prompt_tokens=estimated_input_tokens,
                        completion_tokens=estimated_output_tokens,
                        total_tokens=total_tokens,
                        duration_seconds=duration,
                        cost_estimate=self.logging_service._estimate_cost(
                            self.settings.ai_settings.model_id,
                            estimated_input_tokens,
                            estimated_output_tokens
                        )
                    )
                    
                    # Log conversation completion for this conversion
                    await self.logging_service.log_conversation_message(
                        role="assistant",
                        agent_name="JavaConverterAgent",
                        content=f"Conversion complete for {cobol_file.file_name}: Generated {java_file.class_name if hasattr(java_file, 'class_name') else 'Java class'}",
                        tokens=estimated_output_tokens,
                        metadata={
                            "operation": "java_conversion_complete",
                            "file_name": cobol_file.file_name,
                            "duration_seconds": duration,
                            "java_class_name": java_file.class_name if hasattr(java_file, 'class_name') else "unknown",
                            "java_package": java_file.package_name if hasattr(java_file, 'package_name') else "unknown",
                            "lines_of_code": len(java_file.content.split('\n')) if hasattr(java_file, 'content') else 0,
                            "conversion_confidence": getattr(java_file, 'confidence_score', 0.8)
                        }
                    )
                    
                    progress.update(task, advance=1)
                    await self._update_progress(
                        f"Converting to Java ({i+1}/{len(cobol_files)})", 
                        4, 6,
                        current_file=cobol_file.file_name
                    )
                    
                except Exception as e:
                    self.logger.error(f"Failed to convert {cobol_file.file_name}: {e}")
                    
                    # Log the error
                    await self.logging_service.log_conversation_message(
                        role="assistant",
                        agent_name="JavaConverterAgent",
                        content=f"Failed to convert {cobol_file.file_name}: {str(e)}",
                        tokens=0,
                        metadata={
                            "operation": "java_conversion_error",
                            "file_name": cobol_file.file_name,
                            "error": str(e)
                        }
                    )
                    # Continue with other files
        
        return java_files
    
    async def _save_java_files(self, java_files, output_folder: str) -> None:
        """Save generated Java files to the output folder."""
        for java_file in java_files:
            await self.file_manager.save_java_file(java_file, output_folder)
    
    async def _generate_migration_report(
        self, cobol_files, java_files, dependency_map, output_folder: str
    ) -> str:
        """Generate comprehensive migration report with AI usage statistics."""
        # Get logging statistics to include in the report
        logging_stats = self.logging_service.get_session_statistics()
        
        return await self.report_generator.generate_migration_report(
            cobol_files=cobol_files,
            java_files=java_files,
            dependency_map=dependency_map,
            output_folder=output_folder,
            session_id=self.migration_session_id,
            start_time=self.start_time,
            logging_stats=logging_stats
        )
    
    async def _generate_conversation_log(self, output_folder: str) -> str:
        """Generate conversation log from AI interactions."""
        return await self.logging_service.export_conversation_log(
            output_folder=output_folder,
            session_id=self.migration_session_id
        )
    
    async def _update_progress(
        self, 
        status: str, 
        step: int, 
        total_steps: int,
        current_file: Optional[str] = None
    ) -> None:
        """Update migration progress."""
        if self.progress_callback:
            progress_update = ProgressUpdateSchema(
                current_step=status,
                step_number=step,
                total_steps=total_steps,
                current_file=current_file,
                files_completed=0,  # Will be updated by specific steps
                total_files=0,      # Will be updated by specific steps
                status_message=status
            )
            self.progress_callback(progress_update)
    
    def get_migration_statistics(self) -> Dict[str, Any]:
        """Get current migration statistics."""
        if not self.start_time:
            return {}
        
        duration = (datetime.utcnow() - self.start_time).total_seconds()
        
        return {
            "session_id": self.migration_session_id,
            "start_time": self.start_time.isoformat(),
            "duration_seconds": duration,
            "status": "running" if duration > 0 else "completed"
        }