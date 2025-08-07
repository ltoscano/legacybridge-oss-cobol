"""Logging service for COBOL Migration Agents."""

import asyncio
import aiofiles
import json
import logging
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, asdict

from ..config.settings import Settings


@dataclass
class APICall:
    """Represents an API call for logging purposes."""
    timestamp: datetime
    agent_name: str
    model: str
    prompt_tokens: int
    completion_tokens: int
    total_tokens: int
    duration_seconds: float
    cost_estimate: float
    request_id: Optional[str] = None
    error: Optional[str] = None


@dataclass
class ConversationMessage:
    """Represents a conversation message for logging."""
    timestamp: datetime
    role: str  # 'user', 'assistant', 'system'
    agent_name: str
    content: str
    tokens: int
    metadata: Dict[str, Any]


class LoggingService:
    """
    Service for comprehensive logging of AI conversations and API calls.
    
    Provides tracking of API usage, costs, conversation history,
    and performance metrics with export capabilities.
    """
    
    def __init__(self, settings: Settings):
        """Initialize the logging service."""
        self.settings = settings
        self.logger = logging.getLogger(__name__)
        
        # Create logs directory
        self.logs_dir = Path(settings.application_settings.logs_folder)
        self.logs_dir.mkdir(parents=True, exist_ok=True)
        
        # Storage for session data
        self.api_calls: List[APICall] = []
        self.conversation_messages: List[ConversationMessage] = []
        self.session_id: Optional[str] = None
        self.session_start_time: Optional[datetime] = None
        
        # Performance tracking
        self.performance_metrics: Dict[str, Any] = {}
        
        # Instructor hooks state tracking
        self._current_request_start: Optional[datetime] = None
        self._current_agent_name: str = "unknown"
        self._current_request_data: Dict[str, Any] = {}
        self._hooks_enabled: bool = False
        
        # Setup file logging
        self._setup_file_logging()
    
    def _setup_file_logging(self) -> None:
        """Setup file-based logging configuration."""
        if not self.settings.logging_settings.enable_file_logging:
            return
        
        # Create log file path
        log_file = self.logs_dir / f"migration_{datetime.utcnow().strftime('%Y%m%d')}.log"
        
        # Configure file handler
        file_handler = logging.FileHandler(log_file, encoding='utf-8')
        file_handler.setLevel(getattr(logging, self.settings.logging_settings.level))
        
        # Create formatter
        formatter = logging.Formatter(self.settings.logging_settings.format)
        file_handler.setFormatter(formatter)
        
        # Add to logger and set logger level
        self.logger.addHandler(file_handler)
        self.logger.setLevel(getattr(logging, self.settings.logging_settings.level))
    
    def start_session(self, session_id: str) -> None:
        """
        Start a new logging session.
        
        Args:
            session_id: Unique identifier for the session
        """
        self.session_id = session_id
        self.session_start_time = datetime.utcnow()
        self.api_calls.clear()
        self.conversation_messages.clear()
        self.performance_metrics.clear()
        
        self.logger.info(f"Started logging session: {session_id}")
        self.logger.info(f"Logging configuration - Level: {self.settings.logging_settings.level}, File: {self.settings.logging_settings.enable_file_logging}, Console: {self.settings.logging_settings.enable_console_logging}")
    
    async def log_api_call(
        self,
        agent_name: str,
        model: str,
        prompt_tokens: int,
        completion_tokens: int,
        total_tokens: int,
        duration_seconds: float,
        cost_estimate: float,
        request_id: Optional[str] = None,
        error: Optional[str] = None
    ) -> None:
        """
        Log an API call for tracking and cost analysis.
        
        Args:
            agent_name: Name of the agent making the call
            model: Model used for the call
            prompt_tokens: Number of prompt tokens
            completion_tokens: Number of completion tokens
            total_tokens: Total tokens used
            duration_seconds: Call duration in seconds
            cost_estimate: Estimated cost of the call
            request_id: Optional request identifier
            error: Optional error message
        """
        if not self.settings.logging_settings.log_api_calls:
            return
        
        api_call = APICall(
            timestamp=datetime.utcnow(),
            agent_name=agent_name,
            model=model,
            prompt_tokens=prompt_tokens,
            completion_tokens=completion_tokens,
            total_tokens=total_tokens,
            duration_seconds=duration_seconds,
            cost_estimate=cost_estimate,
            request_id=request_id,
            error=error
        )
        
        self.api_calls.append(api_call)
        
        # Log the call
        if error:
            self.logger.error(f"API call failed - Agent: {agent_name}, Model: {model}, Error: {error}")
        else:
            self.logger.info(f"API call - Agent: {agent_name}, Model: {model}, Tokens: {total_tokens}, Duration: {duration_seconds:.2f}s, Cost: ${cost_estimate:.4f}")
    
    async def log_conversation_message(
        self,
        role: str,
        agent_name: str,
        content: str,
        tokens: int,
        metadata: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Log a conversation message.
        
        Args:
            role: Message role (user, assistant, system)
            agent_name: Name of the agent
            content: Message content
            tokens: Number of tokens in the message
            metadata: Additional metadata
        """
        if not self.settings.memory_settings.enable_conversation_logging:
            return
        
        message = ConversationMessage(
            timestamp=datetime.utcnow(),
            role=role,
            agent_name=agent_name,
            content=content,
            tokens=tokens,
            metadata=metadata or {}
        )
        
        self.conversation_messages.append(message)
        
        # Auto-save if enabled
        if self.settings.memory_settings.auto_save_conversations:
            asyncio.create_task(self._auto_save_conversation())
    
    def _estimate_cost(self, model: str, prompt_tokens: int, completion_tokens: int) -> float:
        """
        Estimate the cost of an API call based on model and token usage.
        
        Args:
            model: Model name
            prompt_tokens: Number of prompt tokens
            completion_tokens: Number of completion tokens
            
        Returns:
            Estimated cost in USD
        """
        # Pricing per 1K tokens (approximate as of 2024)
        pricing = {
            'gpt-4': {'prompt': 0.03, 'completion': 0.06},
            'gpt-4-turbo': {'prompt': 0.01, 'completion': 0.03},
            'gpt-4o': {'prompt': 0.005, 'completion': 0.015},
            'gpt-4o-mini': {'prompt': 0.00015, 'completion': 0.0006},
            'gpt-3.5-turbo': {'prompt': 0.001, 'completion': 0.002}
        }
        
        # Default pricing if model not found
        default_pricing = {'prompt': 0.01, 'completion': 0.03}
        
        model_pricing = pricing.get(model.lower(), default_pricing)
        
        prompt_cost = (prompt_tokens / 1000) * model_pricing['prompt']
        completion_cost = (completion_tokens / 1000) * model_pricing['completion']
        
        return prompt_cost + completion_cost
    
    async def _auto_save_conversation(self) -> None:
        """Auto-save conversation data to prevent loss."""
        if not self.session_id:
            return
        
        try:
            auto_save_path = self.logs_dir / f"conversation_{self.session_id}_autosave.json"
            await self._save_conversation_data(auto_save_path)
        except Exception as e:
            self.logger.error(f"Auto-save failed: {e}")
    
    async def export_conversation_log(
        self,
        output_folder: str,
        session_id: Optional[str] = None
    ) -> str:
        """
        Export conversation log as a readable markdown file.
        
        Args:
            output_folder: Directory to save the log
            session_id: Session ID to export (current session if None)
            
        Returns:
            Path to the exported log file
        """
        target_session = session_id or self.session_id
        if not target_session:
            raise ValueError("No session ID provided and no active session")
        
        output_path = Path(output_folder)
        output_path.mkdir(parents=True, exist_ok=True)
        
        log_file = output_path / f"conversation_log_{target_session}.md"
        
        # Generate markdown content
        markdown_content = await self._generate_conversation_markdown(target_session)
        
        # Save to file
        async with aiofiles.open(log_file, 'w', encoding='utf-8') as f:
            await f.write(markdown_content)
        
        self.logger.info(f"Exported conversation log: {log_file}")
        return str(log_file)
    
    async def _generate_conversation_markdown(self, session_id: str) -> str:
        """
        Generate markdown content for conversation log following C# LogCombiner pattern.
        
        Args:
            session_id: Session ID for the log
            
        Returns:
            Markdown content
        """
        # Load session data from autosave file if conversation_messages is empty
        messages_to_use = self.conversation_messages
        api_calls_to_use = self.api_calls
        session_start = self.session_start_time
        
        if not messages_to_use:
            # Try to load from autosave file
            autosave_path = self.logs_dir / f"conversation_{session_id}_autosave.json"
            # Also try alternative paths in case the file is in a different location
            alt_paths = [
                Path(f"/app/data/logs/conversation_{session_id}_autosave.json"),
                Path(f"./data/logs/conversation_{session_id}_autosave.json"),
                autosave_path
            ]
            
            found_path = None
            for path in alt_paths:
                if path.exists():
                    found_path = path
                    break
            
            
            if found_path:
                try:
                    import json
                    async with aiofiles.open(found_path, 'r', encoding='utf-8') as f:
                        data = json.loads(await f.read())
                    
                    # Load conversation messages
                    messages_to_use = []
                    for msg_data in data.get('conversation_messages', []):
                        msg = ConversationMessage(
                            timestamp=datetime.fromisoformat(msg_data['timestamp']),
                            role=msg_data['role'],
                            agent_name=msg_data['agent_name'],
                            content=msg_data['content'],
                            tokens=msg_data['tokens'],
                            metadata=msg_data.get('metadata', {})
                        )
                        messages_to_use.append(msg)
                    
                    # Load API calls
                    api_calls_to_use = []
                    for call_data in data.get('api_calls', []):
                        call = APICall(
                            timestamp=datetime.fromisoformat(call_data['timestamp']),
                            agent_name=call_data['agent_name'],
                            model=call_data['model'],
                            prompt_tokens=call_data['prompt_tokens'],
                            completion_tokens=call_data['completion_tokens'],
                            total_tokens=call_data['total_tokens'],
                            duration_seconds=call_data['duration_seconds'],
                            cost_estimate=call_data['cost_estimate'],
                            request_id=call_data.get('request_id'),
                            error=call_data.get('error')
                        )
                        api_calls_to_use.append(call)
                    
                    # Load session start time
                    if 'session_start' in data:
                        session_start = datetime.fromisoformat(data['session_start'])
                        
                except Exception as e:
                    self.logger.warning(f"Failed to load autosave data: {e}")
        
        content = [
            f"# 🤖 COBOL to Java Migration: Agent Conversation Log",
            f"## Session: {session_id}",
            f"## Generated: {datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S')}",
            "",
            "---",
            ""
        ]
        
        if session_start:
            content.extend([
                "🎬 **MIGRATION SESSION STARTED**",
                f"*{session_start.strftime('%H:%M:%S')}* - System initializing comprehensive logging...",
                ""
            ])
        
        # Group messages by agent for narrative flow
        current_agent = ""
        sorted_messages = sorted(messages_to_use, key=lambda m: m.timestamp)
        
        for message in sorted_messages:
            agent_category = self._get_agent_category(message.agent_name)
            
            if current_agent != agent_category:
                current_agent = agent_category
                content.extend([
                    "---",
                    "",
                    f"## {self._get_agent_emoji(message.agent_name)} **{message.agent_name.upper()}** speaking:",
                    ""
                ])
            
            # Add agent message in narrative format
            emoji = "👤" if message.role == "user" else "🤖"
            time_str = message.timestamp.strftime('%H:%M:%S')
            
            content.append(f"{emoji} *{time_str}*: \"{message.content}\"")
            
            # Add metadata insights if available
            if message.metadata:
                self._add_metadata_insights(content, message.metadata)
            
            content.append("")
        
        # Add session summary in C# style
        content.extend([
            "---",
            "",
            "## 📈 **SESSION SUMMARY**",
            ""
        ])
        
        self._add_session_summary(content, messages_to_use, api_calls_to_use, session_start)
        
        return "\n".join(content)
    
    def _get_agent_category(self, agent_name: str) -> str:
        """Get agent category for grouping."""
        agent_name_lower = agent_name.lower()
        if "analyzer" in agent_name_lower or "cobol" in agent_name_lower:
            return "CobolAnalyzer"
        elif "converter" in agent_name_lower or "java" in agent_name_lower:
            return "JavaConverter"
        elif "dependency" in agent_name_lower or "mapper" in agent_name_lower:
            return "DependencyMapper"
        else:
            return "MigrationOrchestrator"
    
    def _get_agent_emoji(self, agent_name: str) -> str:
        """Get emoji for agent type."""
        agent_name_lower = agent_name.lower()
        if "analyzer" in agent_name_lower:
            return "🔍"
        elif "converter" in agent_name_lower:
            return "☕"
        elif "dependency" in agent_name_lower:
            return "🗺️"
        else:
            return "🎯"
    
    def _add_metadata_insights(self, content: List[str], metadata: Dict[str, Any]) -> None:
        """Add metadata insights in C# LogCombiner style."""
        if "file_name" in metadata:
            content.append(f"   📁 Working on file: `{metadata['file_name']}`")
        
        if "file_size" in metadata:
            content.append(f"   📏 File size: {metadata['file_size']} bytes")
        
        if "operation" in metadata:
            status_emoji = {
                "start": "⏳",
                "complete": "✅", 
                "error": "❌"
            }.get(metadata["operation"].split("_")[-1], "ℹ️")
            content.append(f"   {status_emoji} Operation: {metadata['operation']}")
        
        if "duration_seconds" in metadata:
            content.append(f"   ⏱️ Duration: {metadata['duration_seconds']:.2f}s")
        
        if "migration_complexity" in metadata:
            content.append(f"   🎯 Complexity: {metadata['migration_complexity']}")
    
    def _add_session_summary(self, content: List[str], messages=None, api_calls=None, session_start=None) -> None:
        """Add session summary following C# pattern."""
        # Use provided data or fallback to instance data for backward compatibility
        messages_data = messages if messages is not None else self.conversation_messages
        api_calls_data = api_calls if api_calls is not None else self.api_calls
        session_start_data = session_start if session_start is not None else self.session_start_time
        
        if session_start_data:
            duration = datetime.utcnow() - session_start_data
            content.append(f"⏱️ **Session Duration**: {duration.total_seconds() / 60:.1f} minutes")
        
        content.append(f"🔧 **Total AI Calls**: {len(api_calls_data)}")
        
        # Count files analyzed
        analyzer_messages = [m for m in messages_data if "analyzer" in m.agent_name.lower()]
        content.append(f"📁 **Files Analyzed**: {len(analyzer_messages)}")
        
        # Count conversion operations (actual COBOL->Java transformations)
        conversion_messages = [m for m in messages_data if m.metadata.get('operation', '') in ['java_conversion_complete', 'conversion_complete']]
        content.append(f"🎯 **Migration Operations**: {len(conversion_messages)}")
        
        # Error count
        error_count = len([call for call in api_calls_data if call.error])
        if error_count > 0:
            content.append(f"❌ **Errors Encountered**: {error_count}")
        else:
            content.append("✅ **Session Completed Successfully!**")
        
        content.extend([
            "",
            "*End of conversation log*"
        ])
    
    async def export_api_statistics(self, output_folder: str) -> str:
        """
        Export detailed API statistics as JSON.
        
        Args:
            output_folder: Directory to save the statistics
            
        Returns:
            Path to the exported statistics file
        """
        output_path = Path(output_folder)
        output_path.mkdir(parents=True, exist_ok=True)
        
        stats_file = output_path / f"api_statistics_{self.session_id}.json"
        
        # Calculate statistics
        statistics = self._calculate_api_statistics()
        
        # Save to file
        async with aiofiles.open(stats_file, 'w', encoding='utf-8') as f:
            await f.write(json.dumps(statistics, indent=2, default=str))
        
        self.logger.info(f"Exported API statistics: {stats_file}")
        return str(stats_file)
    
    def _calculate_api_statistics(self) -> Dict[str, Any]:
        """Calculate comprehensive API usage statistics."""
        if not self.api_calls:
            return {'error': 'No API calls recorded'}
        
        total_calls = len(self.api_calls)
        successful_calls = len([call for call in self.api_calls if not call.error])
        failed_calls = total_calls - successful_calls
        
        total_tokens = sum(call.total_tokens for call in self.api_calls)
        total_cost = sum(call.cost_estimate for call in self.api_calls)
        total_duration = sum(call.duration_seconds for call in self.api_calls)
        
        # Agent breakdown
        agent_stats = {}
        for call in self.api_calls:
            if call.agent_name not in agent_stats:
                agent_stats[call.agent_name] = {
                    'calls': 0,
                    'tokens': 0,
                    'cost': 0,
                    'duration': 0,
                    'errors': 0
                }
            
            agent_stats[call.agent_name]['calls'] += 1
            agent_stats[call.agent_name]['tokens'] += call.total_tokens
            agent_stats[call.agent_name]['cost'] += call.cost_estimate
            agent_stats[call.agent_name]['duration'] += call.duration_seconds
            if call.error:
                agent_stats[call.agent_name]['errors'] += 1
        
        # Model breakdown
        model_stats = {}
        for call in self.api_calls:
            if call.model not in model_stats:
                model_stats[call.model] = {
                    'calls': 0,
                    'tokens': 0,
                    'cost': 0
                }
            
            model_stats[call.model]['calls'] += 1
            model_stats[call.model]['tokens'] += call.total_tokens
            model_stats[call.model]['cost'] += call.cost_estimate
        
        return {
            'session_id': self.session_id,
            'session_start': self.session_start_time,
            'generated_at': datetime.utcnow(),
            'summary': {
                'total_calls': total_calls,
                'successful_calls': successful_calls,
                'failed_calls': failed_calls,
                'success_rate': (successful_calls / total_calls) * 100 if total_calls > 0 else 0,
                'total_tokens': total_tokens,
                'total_cost_usd': total_cost,
                'total_duration_seconds': total_duration,
                'average_tokens_per_call': total_tokens / total_calls if total_calls > 0 else 0,
                'average_duration_per_call': total_duration / total_calls if total_calls > 0 else 0
            },
            'by_agent': agent_stats,
            'by_model': model_stats,
            'recent_calls': [asdict(call) for call in self.api_calls[-10:]]  # Last 10 calls
        }
    
    async def _save_conversation_data(self, file_path: Path) -> None:
        """Save conversation data to JSON file."""
        data = {
            'session_id': self.session_id,
            'session_start': self.session_start_time,
            'api_calls': [asdict(call) for call in self.api_calls],
            'conversation_messages': [asdict(msg) for msg in self.conversation_messages],
            'saved_at': datetime.utcnow()
        }
        
        async with aiofiles.open(file_path, 'w', encoding='utf-8') as f:
            await f.write(json.dumps(data, indent=2, default=str))
    
    def get_session_statistics(self) -> Dict[str, Any]:
        """Get current session statistics."""
        if not self.session_start_time:
            return {}
        
        duration = datetime.utcnow() - self.session_start_time
        
        # Calculate token statistics
        total_prompt_tokens = sum(call.prompt_tokens for call in self.api_calls)
        total_completion_tokens = sum(call.completion_tokens for call in self.api_calls)
        total_tokens = sum(call.total_tokens for call in self.api_calls)
        
        stats = {
            'session_id': self.session_id,
            'duration_seconds': duration.total_seconds(),
            'api_calls': len(self.api_calls),
            'conversation_messages': len(self.conversation_messages),
            'total_prompt_tokens': total_prompt_tokens,
            'total_completion_tokens': total_completion_tokens,
            'total_tokens': total_tokens,
            'total_cost': sum(call.cost_estimate for call in self.api_calls),
            'errors': len([call for call in self.api_calls if call.error]),
            'hooks_enabled': self._hooks_enabled
        }
        
        # Add hook-specific statistics if available
        if self._hooks_enabled and self.api_calls:
            hook_captured_calls = len([call for call in self.api_calls if call.request_id])
            avg_duration = sum(call.duration_seconds for call in self.api_calls) / len(self.api_calls)
            
            # Agent breakdown with detailed token info
            agent_breakdown = {}
            for call in self.api_calls:
                agent = call.agent_name
                if agent not in agent_breakdown:
                    agent_breakdown[agent] = {
                        "calls": 0,
                        "prompt_tokens": 0,
                        "completion_tokens": 0,
                        "total_tokens": 0,
                        "cost": 0.0,
                        "avg_duration": 0.0
                    }
                
                agent_breakdown[agent]["calls"] += 1
                agent_breakdown[agent]["prompt_tokens"] += call.prompt_tokens
                agent_breakdown[agent]["completion_tokens"] += call.completion_tokens
                agent_breakdown[agent]["total_tokens"] += call.total_tokens
                agent_breakdown[agent]["cost"] += call.cost_estimate
                agent_breakdown[agent]["avg_duration"] = (
                    agent_breakdown[agent]["avg_duration"] * (agent_breakdown[agent]["calls"] - 1) +
                    call.duration_seconds
                ) / agent_breakdown[agent]["calls"]
            
            stats.update({
                "hook_captured_calls": hook_captured_calls,
                "average_call_duration": avg_duration,
                "agent_breakdown": agent_breakdown
            })
        
        return stats
    
    def setup_instructor_hooks(self, instructor_client) -> None:
        """
        Setup Instructor hooks for automatic token tracking.
        
        Args:
            instructor_client: The Instructor client to attach hooks to
        """
        def capture_request_start(*args, **kwargs):
            """Capture the start of an API request."""
            self._current_request_start = datetime.utcnow()
            self._current_request_data = {
                "model": kwargs.get("model", "unknown"),
                "start_time": self._current_request_start,
                "messages_count": len(kwargs.get("messages", [])),
                "tools_count": len(kwargs.get("tools", []))
            }
            
            if self.settings.logging_settings.log_api_calls:
                self.logger.debug(f"API request started - Agent: {self._current_agent_name}, "
                                f"Model: {self._current_request_data['model']}")
        
        def capture_token_usage(response):
            """Capture token usage from API response."""
            if not hasattr(response, 'usage') or not response.usage:
                self.logger.warning("No usage information available in API response")
                return response
            
            usage = response.usage
            end_time = datetime.utcnow()
            
            # Calculate duration
            duration_seconds = 0.0
            if self._current_request_start:
                duration_seconds = (end_time - self._current_request_start).total_seconds()
            
            # Extract token data directly from the response
            prompt_tokens = usage.prompt_tokens
            completion_tokens = usage.completion_tokens
            total_tokens = usage.total_tokens
            
            # Calculate cost estimate using the existing method
            cost_estimate = self._estimate_cost(
                self._current_request_data.get("model", "unknown"),
                prompt_tokens,
                completion_tokens
            )
            
            # Log automatically to the LoggingService
            asyncio.create_task(self.log_api_call(
                agent_name=self._current_agent_name,
                model=self._current_request_data.get("model", "unknown"),
                prompt_tokens=prompt_tokens,
                completion_tokens=completion_tokens,
                total_tokens=total_tokens,
                duration_seconds=duration_seconds,
                cost_estimate=cost_estimate,
                request_id=getattr(response, 'id', None)
            ))
            
            if self.settings.logging_settings.log_api_calls:
                self.logger.info(f"✅ API call completed - Agent: {self._current_agent_name}, "
                               f"Prompt: {prompt_tokens}, Completion: {completion_tokens}, "
                               f"Total: {total_tokens} tokens, Duration: {duration_seconds:.2f}s, "
                               f"Cost: ${cost_estimate:.4f}")
            
            return response
        
        def capture_api_error(error):
            """Capture API errors for logging."""
            end_time = datetime.utcnow()
            duration_seconds = 0.0
            
            if self._current_request_start:
                duration_seconds = (end_time - self._current_request_start).total_seconds()
            
            # Estimate prompt tokens (rough approximation)
            estimated_prompt_tokens = self._estimate_prompt_tokens()
            
            # Log error
            asyncio.create_task(self.log_api_call(
                agent_name=self._current_agent_name,
                model=self._current_request_data.get("model", "unknown"),
                prompt_tokens=estimated_prompt_tokens,
                completion_tokens=0,
                total_tokens=estimated_prompt_tokens,
                duration_seconds=duration_seconds,
                cost_estimate=0.0,
                error=str(error)
            ))
            
            self.logger.error(f"API call failed - Agent: {self._current_agent_name}, "
                            f"Error: {str(error)}, Duration: {duration_seconds:.2f}s")
            
            return error
        
        def capture_parse_error(error):
            """Capture parsing errors."""
            self.logger.warning(f"Parsing error in {self._current_agent_name}: {str(error)}")
            return error
        
        def capture_last_attempt(error):
            """Capture when making the final retry attempt."""
            self.logger.warning(f"Final retry attempt for {self._current_agent_name}: {str(error)}")
            return error
        
        # Register all hooks
        instructor_client.on("completion:kwargs", capture_request_start)
        instructor_client.on("completion:response", capture_token_usage)
        instructor_client.on("completion:error", capture_api_error)
        instructor_client.on("parse:error", capture_parse_error)
        instructor_client.on("completion:last_attempt", capture_last_attempt)
        
        self._hooks_enabled = True
        self.logger.info("✅ Instructor hooks registered successfully for automatic token tracking")
    
    def set_current_agent(self, agent_name: str) -> None:
        """
        Set the current agent name for logging context.
        
        Args:
            agent_name: Name of the currently active agent
        """
        self._current_agent_name = agent_name
        if self.settings.logging_settings.log_api_calls:
            self.logger.debug(f"Current agent set to: {agent_name}")
    
    def _estimate_prompt_tokens(self) -> int:
        """
        Estimate prompt tokens when actual count is not available (only for error cases).
        
        Note: This is only used when API calls fail and we don't get the actual
        token count from the response. In successful calls, we use the exact
        values from response.usage.prompt_tokens, response.usage.completion_tokens, etc.
        
        Returns:
            Estimated number of prompt tokens
        """
        # Very rough estimation based on message and tool count (fallback only)
        messages_count = self._current_request_data.get("messages_count", 0)
        tools_count = self._current_request_data.get("tools_count", 0)
        
        # Estimate: ~50 tokens per message + ~100 tokens per tool
        estimated_tokens = (messages_count * 50) + (tools_count * 100)
        return max(estimated_tokens, 10)  # At least 10 tokens