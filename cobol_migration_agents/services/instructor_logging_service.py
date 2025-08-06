"""
Enhanced logging service with Instructor hooks integration.

This service extends the existing LoggingService to automatically capture
token usage and API metrics using Instructor's hook system.
"""

import asyncio
from datetime import datetime
from typing import Optional, Dict, Any
import logging

from .logging_service import LoggingService
from ..config.settings import Settings


class InstructorLoggingService(LoggingService):
    """
    Enhanced logging service that automatically captures token consumption
    and API metrics using Instructor's hook system.
    
    This service extends the base LoggingService and adds automatic hooks
    to capture API usage data without manual intervention.
    """
    
    def __init__(self, settings: Settings):
        """Initialize the enhanced logging service."""
        super().__init__(settings)
        self.logger = logging.getLogger(__name__)
        
        # State for tracking current requests
        self._current_request_start: Optional[datetime] = None
        self._current_agent_name: str = "unknown"
        self._current_request_data: Dict[str, Any] = {}
        
        self.logger.info("InstructorLoggingService initialized with hook support")
    
    def setup_instructor_hooks(self, instructor_client) -> None:
        """
        Setup Instructor hooks for automatic token tracking.
        
        Args:
            instructor_client: The Instructor client to attach hooks to
        """
        # Hook per catturare l'inizio della richiesta
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
        
        # Hook per catturare token usage dalla risposta
        def capture_token_usage(response):
            """Capture token usage from API response."""
            if not hasattr(response, 'usage') or not response.usage:
                self.logger.warning("No usage information available in API response")
                return response
            
            usage = response.usage
            end_time = datetime.utcnow()
            
            # Calcola durata
            duration_seconds = 0.0
            if self._current_request_start:
                duration_seconds = (end_time - self._current_request_start).total_seconds()
            
            # Estrai dati token
            prompt_tokens = getattr(usage, 'prompt_tokens', 0)
            completion_tokens = getattr(usage, 'completion_tokens', 0)
            total_tokens = getattr(usage, 'total_tokens', 0)
            
            # Calcola costo stimato
            cost_estimate = self._estimate_cost(
                self._current_request_data.get("model", "unknown"),
                prompt_tokens,
                completion_tokens
            )
            
            # Log automaticamente alla LoggingService
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
                self.logger.info(f"API call completed - Agent: {self._current_agent_name}, "
                               f"Tokens: {total_tokens}, Duration: {duration_seconds:.2f}s, "
                               f"Cost: ${cost_estimate:.4f}")
            
            return response
        
        # Hook per catturare errori API
        def capture_api_error(error):
            """Capture API errors for logging."""
            end_time = datetime.utcnow()
            duration_seconds = 0.0
            
            if self._current_request_start:
                duration_seconds = (end_time - self._current_request_start).total_seconds()
            
            # Stima token prompt (approssimativa)
            estimated_prompt_tokens = self._estimate_prompt_tokens()
            
            # Log errore
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
        
        # Hook per errori di parsing
        def capture_parse_error(error):
            """Capture parsing errors."""
            self.logger.warning(f"Parsing error in {self._current_agent_name}: {str(error)}")
            return error
        
        # Hook per ultimo tentativo
        def capture_last_attempt(error):
            """Capture when making the final retry attempt."""
            self.logger.warning(f"Final retry attempt for {self._current_agent_name}: {str(error)}")
            return error
        
        # Registra tutti gli hook
        instructor_client.on("completion:kwargs", capture_request_start)
        instructor_client.on("completion:response", capture_token_usage)
        instructor_client.on("completion:error", capture_api_error)
        instructor_client.on("parse:error", capture_parse_error)
        instructor_client.on("completion:last_attempt", capture_last_attempt)
        
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
        Estimate prompt tokens when actual count is not available.
        
        Returns:
            Estimated number of prompt tokens
        """
        # Stima molto approssimativa basata sul numero di messaggi
        messages_count = self._current_request_data.get("messages_count", 0)
        tools_count = self._current_request_data.get("tools_count", 0)
        
        # Stima: ~50 token per messaggio + ~100 token per tool
        estimated_tokens = (messages_count * 50) + (tools_count * 100)
        return max(estimated_tokens, 10)  # Almeno 10 token
    
    async def log_agent_conversation_with_tokens(
        self,
        agent_name: str,
        user_message: str,
        assistant_response: str,
        metadata: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Log a complete agent conversation with automatic token estimation.
        
        Args:
            agent_name: Name of the agent
            user_message: User's input message
            assistant_response: Agent's response
            metadata: Additional metadata
        """
        # Stima token (approssimativa: 4 caratteri per token)
        user_tokens = len(user_message) // 4
        assistant_tokens = len(assistant_response) // 4
        
        # Log messaggio utente
        await self.log_conversation_message(
            role="user",
            agent_name=agent_name,
            content=user_message,
            tokens=user_tokens,
            metadata=metadata
        )
        
        # Log risposta assistente
        await self.log_conversation_message(
            role="assistant",
            agent_name=agent_name,
            content=assistant_response,
            tokens=assistant_tokens,
            metadata=metadata
        )
        
        if self.settings.logging_settings.log_api_calls:
            self.logger.debug(f"Conversation logged - Agent: {agent_name}, "
                            f"User tokens: {user_tokens}, Assistant tokens: {assistant_tokens}")
    
    def get_enhanced_session_statistics(self) -> Dict[str, Any]:
        """
        Get enhanced session statistics including hook-captured data.
        
        Returns:
            Dictionary with comprehensive session statistics
        """
        base_stats = self.get_session_statistics()
        
        # Aggiungi statistiche specifiche per hook
        if self.api_calls:
            hook_captured_calls = len([call for call in self.api_calls if call.request_id])
            avg_duration = sum(call.duration_seconds for call in self.api_calls) / len(self.api_calls)
            
            # Statistiche per agente
            agent_breakdown = {}
            for call in self.api_calls:
                agent = call.agent_name
                if agent not in agent_breakdown:
                    agent_breakdown[agent] = {
                        "calls": 0,
                        "tokens": 0,
                        "cost": 0.0,
                        "avg_duration": 0.0
                    }
                
                agent_breakdown[agent]["calls"] += 1
                agent_breakdown[agent]["tokens"] += call.total_tokens
                agent_breakdown[agent]["cost"] += call.cost_estimate
                agent_breakdown[agent]["avg_duration"] = (
                    agent_breakdown[agent]["avg_duration"] * (agent_breakdown[agent]["calls"] - 1) +
                    call.duration_seconds
                ) / agent_breakdown[agent]["calls"]
            
            base_stats.update({
                "hook_captured_calls": hook_captured_calls,
                "average_call_duration": avg_duration,
                "agent_breakdown": agent_breakdown,
                "hooks_enabled": True
            })
        else:
            base_stats.update({
                "hook_captured_calls": 0,
                "hooks_enabled": True
            })
        
        return base_stats
    
    async def export_enhanced_conversation_log(
        self,
        output_folder: str,
        session_id: Optional[str] = None
    ) -> str:
        """
        Export enhanced conversation log with hook-captured metrics.
        
        Args:
            output_folder: Directory to save the log
            session_id: Session ID to export (current session if None)
            
        Returns:
            Path to the exported log file
        """
        # Usa il metodo base ma con statistiche potenziate
        log_path = await self.export_conversation_log(output_folder, session_id)
        
        self.logger.info(f"Enhanced conversation log with hook data exported to: {log_path}")
        return log_path


def create_instructor_logging_service(settings: Settings) -> InstructorLoggingService:
    """
    Factory function to create an InstructorLoggingService instance.
    
    Args:
        settings: Application settings
        
    Returns:
        Configured InstructorLoggingService instance
    """
    service = InstructorLoggingService(settings)
    
    # Log initialization
    logger = logging.getLogger(__name__)
    logger.info("Created InstructorLoggingService with automatic token tracking")
    
    return service